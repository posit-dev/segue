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
#' such as 0.25 or 1/7. Supply only one of `digits` and `unit`. As in SAS,
#' units at or below `.Machine$double.xmin` (about 2.2e-308) are not allowed.
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
#'    rounded value of `x / unit` is divided by that integer so that the
#'    result matches the decimal value (`sas_round(0.33, 0.1)` is identical to
#'    `0.3`). As in SAS, `1 / unit` must be within 16 units in the last place
#'    of the integer.
#'  - When `unit` is not an integer but has at most four decimal places, the
#'    result is computed with integer arithmetic so that it also matches the
#'    decimal value.
#'  - Otherwise, the result is the rounded integer times `unit`. When that
#'    product is within a few units in the last place of a decimal with at
#'    most four decimal places, the decimal value is returned instead
#'    (`sas_round(0.0015, unit = 0.00015)` is identical to `0.0015`).
#'  - When `abs(x / unit)` is at least 2^52, `x` is returned unchanged.
#'
#' Missing values are returned as missing.
#'
#' These results were compared with about 70,000 results of the SAS
#' `ROUND()` function (SAS Viya, `V.04.00M0P060826`), including every value
#' with up to three decimal places between -3 and 3, and every case in the
#' package's tests. They are identical except
#' when `abs(x)` or `abs(x / unit)` is 1e15 or more. There, values have more
#' than 15 significant digits and SAS is not exact either: for example,
#' `ROUND(1000000000000000.5, 0.5)` is 1000000000000001, although the value is
#' already a multiple of 0.5.
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
  # SAS returns a missing value with an "invalid argument" note for these.
  smallest <- .Machine$double.xmin
  if (unit <= smallest) {
    cli::cli_abort(
      "{.arg unit} should be larger than {.code .Machine$double.xmin}
       ({signif(smallest, 3)}), not {unit}."
    )
  }

  divisor <- reciprocal_integer(unit)
  places <- decimal_places(unit)

  if (unit == round(unit)) {
    res <- round_half_away(x / unit) * unit
  } else if (!is.null(divisor)) {
    # Round x / unit, not x * divisor: they can differ in the last bit, and
    # SAS decides values exactly 1e-12 from halfway using x / unit.
    res <- round_half_away(x / unit) / divisor
  } else if (!is.null(places)) {
    unit_int <- round(unit * 10^places)
    res <- round_half_away(x / unit) * unit_int / 10^places
  } else {
    res <- snap_to_decimal(round_half_away(x / unit) * unit)
  }
  # When x / unit is this large, x is already a multiple of unit at double
  # precision, and scaling it could lose precision or overflow.
  is_large <- !is.na(x) & abs(x / unit) >= 2^52
  res[is_large] <- x[is_large]
  res
}

#' Format numbers the same way as SAS
#'
#' `sas_format()` formats numbers with a fixed number of decimal places so
#' that the printed values match the SAS `w.d` format (e.g., `8.2`).
#'
#' @param x A numeric vector.
#' @param digits A single whole number between 0 and 15 for the number of
#' decimal places to show. As in [round()], and unlike [format()], this is the
#' number of decimal places and not the number of significant digits.
#' @param ... Formatting options, as in [format()]: `width`, `justify`,
#' `trim`, `big.mark`, `big.interval`, `small.mark`, `small.interval`,
#' `decimal.mark`, and `scientific`. Scientific notation is not used unless
#' `scientific = TRUE` is passed, in which case the rounded values are
#' formatted with `format(digits = 15)`.
#' @details
#' SAS formats do not round with the `ROUND()` function, so `sas_format()`
#' does not use [sas_round()]. With the default `DECIMALCONV=COMPATIBLE`
#' system option, SAS:
#'
#'  - Rounds `x * 10^digits` half away from zero. Values within a relative
#'    1e-16 of a halfway point (about one unit in the last place) are treated
#'    as halfway, so `2.675` (stored as slightly less than 2.675) is shown as
#'    `2.68` with two decimal places.
#'  - Shows at most 15 significant digits. When more digits are needed, the
#'    number is first rounded to 15 significant digits and zeros are shown
#'    after them (`sas_format(123456789.123456, 10)` is
#'    `"123456789.1234560000"`).
#'  - Keeps the sign of negative numbers that round to zero, so `-0.4` is
#'    shown as `-0` and `-0.001` as `-0.00` with two decimal places.
#'  - Treats subnormal numbers (smaller than `.Machine$double.xmin`) as zero.
#'
#' The tolerance is much smaller than the one used by `ROUND()`:
#' `0.4999999999999982` is shown as `0`, but `sas_round(0.4999999999999982)`
#' is 1. So `sas_format(x, digits)` can differ from
#' `format(sas_round(x, digits), nsmall = digits)`.
#'
#' These results were compared with about 100,000 results of the SAS `32.d`
#' formats (SAS Viya, `V.04.00M0P060826`), including every value with up to
#' four decimal places in a range around zero. They are identical except
#' within a few units in the last place of a halfway point, where SAS's
#' tolerance varies from value to value, and when 15 or more digits are shown,
#' where SAS's digits are not consistent (`1/3` is shown as
#' `0.333333333333330` with 15 decimal places).
#'
#' With `DECIMALCONV=STDIEEE`, SAS formats the exact binary value instead
#' (`2.675` is shown as `2.67`); `sas_format()` does not replicate that.
#' @return A character vector the same length as `x`. As with [format()], the
#' values share a common width and names are kept.
#' @seealso [sas_round()]
#' @examples
#' format(round(1.005, 2), nsmall = 2)
#' sas_format(1.005, digits = 2)
#'
#' sas_format(c(0.5, 1.5, 2.5, -2.5))
#' sas_format(-0.4)
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
  unknown <- setdiff(names(opts), sas_format_options)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "{.fn sas_format} does not use {.arg {unknown}}. It can use
       {.arg {sas_format_options}}."
    )
  }

  dec <- sas_decimal(x, digits)
  if (isTRUE(opts$scientific)) {
    # Scientific notation is not a SAS w.d format; show the rounded value.
    res <- rlang::inject(format(dec$value, digits = 15, !!!opts))
    names(res) <- names(x)
    return(res)
  }

  txt <- dec$text
  mark_opts <- opts[intersect(names(opts), sas_format_marks)]
  if (length(mark_opts) > 0) {
    is_num <- !is.na(dec$value) & is.finite(dec$value)
    txt[is_num] <- rlang::inject(prettyNum(txt[is_num], !!!mark_opts))
  }
  justify <- opts$justify
  if (is.null(justify)) {
    justify <- "right"
  }
  if (isTRUE(opts$trim)) {
    justify <- "none"
  }
  res <- format(txt, width = opts$width, justify = justify)
  names(res) <- names(x)
  res
}

sas_format_marks <- c(
  "big.mark",
  "big.interval",
  "small.mark",
  "small.interval",
  "decimal.mark"
)
sas_format_options <- c(
  sas_format_marks,
  "width",
  "justify",
  "trim",
  "scientific"
)

# The SAS w.d result as decimal text and as the closest double.
#
# While the scaled value abs(x) * 10^digits is below 1e15, it is rounded half
# away from zero in floating point, with values within a relative 1e-16 of
# halfway treated as halfway. Larger results are shown with 15 significant
# digits, followed by zeros; sprintf() gives those digits from the exact
# binary value.
sas_decimal <- function(x, digits) {
  x <- as.double(x)
  # SAS treats subnormal numbers as zero
  x[!is.na(x) & abs(x) < .Machine$double.xmin] <- 0
  text <- trimws(format(x))
  value <- x
  ok <- is.finite(x)
  if (!any(ok)) {
    return(list(text = text, value = value))
  }
  int_digits <- character(sum(ok))
  scaled <- numeric(sum(ok))

  v <- abs(x[ok]) * 10^digits
  small <- v < 1e15
  n <- floor(v[small] + 0.5 + v[small] * 1e-16)
  int_digits[small] <- sprintf("%.0f", n)
  scaled[small] <- scale_by_ten(n, rep(-digits, length(n)))

  large <- sig15_digits(abs(x[ok][!small]), digits)
  int_digits[!small] <- large$int_digits
  scaled[!small] <- large$value

  sgn <- character(sum(ok))
  # SAS keeps the sign of negative values that round to zero ("-0.00")
  sgn[x[ok] < 0] <- "-"
  text[ok] <- paste0(sgn, insert_decimal_point(int_digits, digits))
  value[ok] <- sign(x[ok]) * scaled
  list(text = text, value = value)
}

# For a positive x with x * 10^digits >= 1e15: x rounded to 15 significant
# digits, as an integer string of the value times 10^digits (the 15 digits
# followed by zeros), and the closest double to that value.
sig15_digits <- function(x, digits) {
  txt <- sprintf("%.14e", x)
  mantissa <- sub(".", "", sub("e.*$", "", txt), fixed = TRUE)
  expon <- as.integer(sub("^.*e", "", txt))
  # x is the 15-digit integer `mantissa` times 10^(expon - 14), and at
  # `digits` places that is `mantissa` followed by `zeros` zeros. Because
  # x * 10^digits >= 1e15, no digits are dropped.
  zeros <- expon - 14 + digits
  stopifnot(all(zeros >= 0))
  list(
    int_digits = paste0(mantissa, strrep("0", zeros)),
    value = scale_by_ten(as.numeric(mantissa), expon - 14)
  )
}

insert_decimal_point <- function(int_digits, digits) {
  if (digits == 0) {
    return(int_digits)
  }
  short <- nchar(int_digits) <= digits
  int_digits[short] <- paste0(
    strrep("0", digits + 1 - nchar(int_digits[short])),
    int_digits[short]
  )
  n <- nchar(int_digits)
  paste0(
    substr(int_digits, 1, n - digits),
    ".",
    substr(int_digits, n - digits + 1, n)
  )
}

# n * 10^k, rounded once when 10^|k| is exact (|k| <= 22)
scale_by_ten <- function(n, k) {
  res <- n * 10^k
  neg <- k < 0
  res[neg] <- n[neg] / 10^(-k[neg])
  res
}

round_half_away <- function(x) {
  abs_x <- abs(x)
  fuzz <- pmin(1e-6, abs_x * 1e-12)
  sign(x) * floor(abs_x + 0.5 + fuzz)
}

# SAS treats `unit` as the reciprocal of an integer when 1 / unit is within
# "three or four least significant bits" of an integer, i.e., fewer than 16
# units in the last place. Observed in SAS: a unit whose reciprocal is 20 units
# in the last place from 7 is not treated as 1/7.
reciprocal_integer <- function(unit) {
  recip <- 1 / unit
  if (unit >= 1 || is.infinite(recip)) {
    return(NULL)
  }
  recip_int <- round(recip)
  ulp <- 2^(floor(log2(recip_int)) - 52)
  if (abs(recip - recip_int) >= 16 * ulp) {
    return(NULL)
  }
  recip_int
}

# SAS matches decimal arithmetic when the expected result has at most four
# decimal places: values within a few units in the last place of such a
# decimal are replaced by the double nearest to it. Dividing an integer below
# 2^53 by an exact power of ten rounds once, so that double is exact.
snap_to_decimal <- function(res) {
  out <- res
  done <- !is.finite(res)
  for (places in 0:4) {
    scaled <- res * 10^places
    scaled_int <- round(scaled)
    near <- !done &
      is.finite(scaled) &
      abs(scaled_int) < 2^53 &
      abs(scaled - scaled_int) <= 16 * .Machine$double.eps * abs(scaled)
    out[near] <- scaled_int[near] / 10^places
    done <- done | near
  }
  out
}

decimal_places <- function(unit) {
  if (unit == round(unit)) {
    return(NULL)
  }
  for (places in 1:4) {
    scaled <- unit * 10^places
    if (abs(scaled - round(scaled)) <= 16 * .Machine$double.eps * scaled) {
      return(places)
    }
  }
  NULL
}
