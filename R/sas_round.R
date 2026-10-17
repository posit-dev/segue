#' Round numbers the same way as SAS
#'
#' `sas_round()` replicates the SAS `ROUND()` function.
#'
#' @param x A numeric vector.
#' @param digits A single whole number between -15 and 15 for the number of
#' decimal places, as in [round()]. Negative values round to a power of ten
#' (e.g., `digits = -2` rounds to the nearest hundred).
#' @param unit A single positive number for the rounding unit (the
#' `rounding-unit` argument of the SAS function). The default, `10^-digits`,
#' is determined by `digits`. Use `unit` for units that are not powers of ten,
#' such as 0.25 or 1/7. Supply only one of `digits` and `unit`.
#' @details
#' R's [round()] and SAS's `ROUND()` can give different answers for two
#' reasons:
#'
#'  - When a value is halfway between two multiples of `unit`, `round()`
#'    returns the even multiple (e.g., `round(2.5)` is 2), while SAS returns
#'    the multiple farther from zero (3).
#'  - Many decimal values do not have an exact binary representation. For
#'    example, `1.005` is stored as slightly less than 1.005, so
#'    `round(1.005, 2)` is 1. SAS treats values that are within a small
#'    relative tolerance of the halfway point as being halfway (called
#'    "fuzzing"), so `ROUND(1.005, 0.01)` is 1.01.
#'
#' `sas_round()` follows the algorithm described in the SAS documentation:
#'
#'  - `x / unit` is rounded half away from zero, after adding a tolerance of
#'    `min(1e-6, abs(x / unit) * 1e-12)` to its absolute value.
#'  - When `unit` is the reciprocal of an integer (e.g., 0.01 or 1/7), the
#'    result is computed by dividing by that integer so that it matches the
#'    decimal value (`sas_round(0.33, 0.1)` is identical to `0.3`).
#'  - When `unit` is not an integer but has at most four decimal places, the
#'    result is computed with integer arithmetic so that it also matches the
#'    decimal value.
#'  - Otherwise, the result is the rounded integer times `unit`.
#'
#' Missing values are returned as missing.
#' @return A double vector the same length as `x`.
#' @references SAS Institute Inc. "ROUND Function." _SAS Functions and CALL
#' Routines: Reference_.
#' @examples
#' x <- c(0.5, 1.5, 2.5, -2.5)
#' round(x)
#' sas_round(x)
#'
#' round(1.005, digits = 2)
#' sas_round(1.005, digits = 2)
#'
#' sas_round(1234.5, digits = -2)
#'
#' sas_round(pi, unit = 1 / 7) == 22 / 7
#' @export
sas_round <- function(x, digits = 0, unit = 10^-digits) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg x} should be a numeric vector, not {.obj_type_friendly {x}}."
    )
  }
  if (!missing(digits) && !missing(unit)) {
    cli::cli_abort("Supply only one of {.arg digits} and {.arg unit}.")
  }
  rlang::check_number_whole(digits, min = -15, max = 15)
  rlang::check_number_decimal(unit, allow_infinite = FALSE)
  if (unit <= 0) {
    cli::cli_abort("{.arg unit} should be a positive number, not {unit}.")
  }

  divisor <- reciprocal_integer(unit)
  places <- decimal_places(unit)

  if (unit == round(unit)) {
    res <- round_half_away(x / unit) * unit
  } else if (!is.null(divisor)) {
    res <- round_half_away(x * divisor) / divisor
  } else if (!is.null(places)) {
    unit_int <- round(unit * 10^places)
    res <- round_half_away(x / unit) * unit_int / 10^places
  } else {
    res <- round_half_away(x / unit) * unit
  }
  res
}

#' Format numbers using SAS rounding
#'
#' `sas_format()` rounds with [sas_round()] and then formats the result with
#' [format()], so that the printed values match what SAS would show.
#'
#' @param x A numeric vector.
#' @param digits A single whole number between 0 and 15 for the number of
#' decimal places to show. As in [round()], and unlike [format()], this is the
#' number of decimal places and not the number of significant digits.
#' @param ... Other arguments passed to [format()], such as `width`,
#' `big.mark`, or `justify`. `nsmall` is set by `sas_format()`. Scientific
#' notation is not used unless `scientific = TRUE` is passed.
#' @return A character vector the same length as `x`. As with [format()], the
#' values share a common width and names are kept.
#' @seealso [sas_round()]
#' @examples
#' format(round(1.005, 2), nsmall = 2)
#' sas_format(1.005, digits = 2)
#'
#' sas_format(c(0.5, 1.5, 2.5, -2.5))
#' sas_format(1234567.891, digits = 2, big.mark = ",")
#' @export
sas_format <- function(x, digits = 0, ...) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg x} should be a numeric vector, not {.obj_type_friendly {x}}."
    )
  }
  rlang::check_number_whole(digits, min = 0, max = 15)
  opts <- list(...)
  if ("nsmall" %in% names(opts)) {
    cli::cli_abort(
      "{.arg nsmall} is set by {.fn sas_format} and cannot be passed to
       {.fn format}."
    )
  }
  if (!"scientific" %in% names(opts)) {
    opts$scientific <- FALSE
  }

  rounded <- sas_round(x, digits = digits)
  # 15 significant digits shows a rounded double without formatting noise
  # while keeping format() from rounding it again.
  rlang::inject(format(rounded, digits = 15, nsmall = digits, !!!opts))
}

round_half_away <- function(x) {
  abs_x <- abs(x)
  fuzz <- pmin(1e-6, abs_x * 1e-12)
  res <- floor(abs_x + 0.5 + fuzz)
  # Doubles this large are already whole numbers and adding 0.5 could change
  # them.
  is_large <- !is.na(abs_x) & abs_x >= 2^52
  res[is_large] <- abs_x[is_large]
  sign(x) * res
}

# SAS treats `unit` as the reciprocal of an integer when 1 / unit is within
# "three or four least significant bits" of an integer.
reciprocal_integer <- function(unit) {
  if (unit >= 1) {
    return(NULL)
  }
  recip <- 1 / unit
  recip_int <- round(recip)
  if (abs(recip - recip_int) > 16 * .Machine$double.eps * recip_int) {
    return(NULL)
  }
  recip_int
}

decimal_places <- function(unit) {
  for (places in 1:4) {
    scaled <- unit * 10^places
    if (abs(scaled - round(scaled)) <= 16 * .Machine$double.eps * scaled) {
      return(places)
    }
  }
  NULL
}
