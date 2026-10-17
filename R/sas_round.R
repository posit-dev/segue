#' Round numbers the same way as SAS
#'
#' `sas_round()` replicates the SAS `ROUND()` function.
#'
#' @param x A numeric vector.
#' @param unit A single positive number for the rounding unit (the
#' `rounding-unit` argument of the SAS function). The default rounds to the
#' nearest integer.
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
#' round(1.005, 2)
#' sas_round(1.005, unit = 0.01)
#'
#' sas_round(pi, unit = 1 / 7) == 22 / 7
#' @export
sas_round <- function(x, unit = 1) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg x} should be a numeric vector, not {.obj_type_friendly {x}}."
    )
  }
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
