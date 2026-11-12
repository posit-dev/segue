# Creates cases.csv, the test cases for sas_round() that are also run in SAS by
# round.sas. Run from the package root:
#
#   Rscript tests/testthat/fixtures/sas-round/make-cases.R
#
# The `expected` values never come from sas_round(). They are taken from the
# SAS documentation or computed with exact integer arithmetic. Numbers are also
# stored as the hex representation of the double so that R and SAS use the
# exact same bits.
#
# Sources:
#
#  - "doc": examples from the SAS ROUND documentation.
#  - "doc-halfway": the halfway examples from the documentation, with expected
#    values from the documentation's DATA step approximation of the fuzz. The
#    documentation says that this approximation is not always exact.
#  - "known": widely reported differences between SAS and R.
#  - "decimal": random decimal values (including exact halves) where the
#    documentation says that ROUND matches decimal arithmetic: results with at
#    most nine significant digits and a unit that is an integer, a power of
#    10, or has at most four decimal places.
#  - "explore": cases without a documented answer. `expected` is empty; SAS
#    output defines the answer.

dbl_to_hex <- function(x) {
  hex <- vapply(
    x,
    function(val) {
      if (is.na(val)) {
        return("")
      }
      paste(as.character(writeBin(val, raw(), endian = "big")), collapse = "")
    },
    character(1)
  )
  toupper(unname(hex))
}

new_cases <- function(source, x, unit, expected = NA_real_) {
  data.frame(source = source, x = x, unit = unit, expected = expected)
}

half_away <- function(num, den) {
  sign(num) * ((2 * abs(num) + den) %/% (2 * den))
}

# ------------------------------------------------------------------------------

doc_units <- c(100, 10, 1, 0.1, 0.01, 0.001, 0.0001, 0.00001, 0.1111)
doc_expected <- c(
  1200,
  1230,
  1235,
  1234.6,
  1234.57,
  1234.568,
  1234.5679,
  1234.56789,
  1234.5432
)
quarters <- seq(0, 4, by = 0.25)

doc <- rbind(
  new_cases("doc", 1234.56789, doc_units, doc_expected),
  new_cases("doc", 1234.56789, 0.11111, 11111 * 0.11111),
  new_cases("doc", 0.33, 0.1, 0.3),
  new_cases("doc", pi, 1 / 7, 22 / 7),
  new_cases("doc", quarters, 1, floor(quarters + 0.5))
)

halfway_value <- c(0.5 - 10^-(1:17), -0.5 + 10^-(1:17))
halfway_abs <- abs(halfway_value)
halfway_epsilon <- pmin(1e-6, halfway_abs * 1e-12)
halfway_temp <- halfway_abs + 0.5 + halfway_epsilon
halfway <- new_cases(
  "doc-halfway",
  halfway_value,
  1,
  sign(halfway_value) * (halfway_temp - halfway_temp %% 1)
)

known <- rbind(
  new_cases("known", c(2.5, -2.5, 0.5, -0.5), 1, c(3, -3, 1, -1)),
  new_cases("known", 1.005, 0.01, 1.01),
  new_cases("known", 2.675, 0.01, 2.68),
  new_cases("known", 0.285, 0.01, 0.29),
  new_cases("known", 0.125, 0.01, 0.13),
  new_cases("known", 0.15, 0.1, 0.2),
  new_cases("known", -1.005, 0.01, -1.01)
)

# ------------------------------------------------------------------------------
# A unit of unit_int / 10^unit_places is applied to x = x_int / 10^x_places, so
# x / unit = (x_int * 10^unit_places) / (unit_int * 10^x_places) is an exact
# ratio of integers.

set.seed(83921)

decimal_units <- data.frame(
  unit_int = c(1, 5, 10, 100, 1, 5, 3, 1, 5, 25, 1, 75, 1, 25, 1111),
  unit_places = c(0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4)
)

decimal <- vector(mode = "list", length = nrow(decimal_units))
for (i in seq_len(nrow(decimal_units))) {
  unit_int <- decimal_units$unit_int[i]
  unit_places <- decimal_units$unit_places[i]

  num_random <- 20
  x_places <- sample(0:4, num_random, replace = TRUE)
  x_int <- (sample.int(2 * 10^8 + 1, num_random) - (10^8 + 1)) %/%
    10^(4 - x_places)

  # Exact halves: x = (2n + 1) * unit / 2
  num_halves <- 10
  half_n <- sample(-10^4:10^4, num_halves)
  x_int <- c(x_int, (2 * half_n + 1) * unit_int * 5)
  x_places <- c(x_places, rep(unit_places + 1, num_halves))

  num <- x_int * 10^unit_places
  den <- unit_int * 10^x_places
  # Keep cases where the distance from a non-halfway value to the halfway
  # point is larger than the fuzz.
  keep <- abs(num) < 1e11 & abs(x_int) < 1e9
  num <- num[keep]
  den <- den[keep]

  expected <- half_away(num, den) * unit_int / 10^unit_places
  decimal[[i]] <- new_cases(
    "decimal",
    x_int[keep] / 10^x_places[keep],
    unit_int / 10^unit_places,
    expected
  )
}
decimal <- do.call(rbind, decimal)

explore <- rbind(
  new_cases("explore", runif(5, -100, 100), 1 / 7),
  new_cases("explore", runif(5, -100, 100), 1 / 3),
  new_cases("explore", runif(5, -100, 100), 0.11111),
  new_cases("explore", runif(5, -100, 100), 0.123456),
  new_cases("explore", c(2^52 + 1, 2^53, 1e15 + 0.5, 1e17), 1),
  new_cases("explore", c(1e15 + 0.5, 123456789012.345), 0.01),
  new_cases("explore", c(1.5e-20, 2.5e-20, 3.49999e-20), 1e-20),
  new_cases("explore", c(0.0001234, 0.00012345), 1e-7),
  # x / unit overflows
  new_cases("explore", c(1e300, -1e300), 1e-15),
  new_cases("explore", 5e9, 1e-300),
  new_cases("explore", 1.7e308, c(0.1111, 0.11111)),
  # 1 / unit overflows
  new_cases("explore", 1, c(1e-320, 5e-324)),
  # Negative values that round to zero, to see if SAS returns -0
  new_cases("explore", c(-0.4, -0.04), c(1, 0.1)),
  # The reciprocal of the unit is 20 ulps from 7, more than the "three or four
  # least significant bits" in the documentation.
  new_cases("explore", pi, 1 / (7 + 80 * .Machine$double.eps)),
  # A unit with five decimal places and results with four
  new_cases("explore", c(0.0015, 0.0033), 0.00015),
  new_cases("explore", 0.0121, 0.00055)
)

# ------------------------------------------------------------------------------

cases <- rbind(doc, halfway, known, decimal, explore)
cases$expected[cases$expected %in% 0] <- 0
cases <- data.frame(
  id = seq_len(nrow(cases)),
  source = cases$source,
  x_hex = dbl_to_hex(cases$x),
  unit_hex = dbl_to_hex(cases$unit),
  expected_hex = dbl_to_hex(cases$expected),
  x = sprintf("%.17g", cases$x),
  unit = sprintf("%.17g", cases$unit),
  expected = ifelse(
    is.na(cases$expected),
    "",
    sprintf("%.17g", cases$expected)
  )
)

utils::write.csv(
  cases,
  file.path("tests", "testthat", "fixtures", "sas-round", "cases.csv"),
  row.names = FALSE,
  quote = FALSE
)
