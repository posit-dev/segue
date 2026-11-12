# Expected values come from the SAS ROUND documentation, exact decimal
# arithmetic, or results observed in SAS (SAS Viya, V.04.00M0P060826, with the
# default DECIMALCONV=COMPATIBLE). See fixtures/sas-round/ for the full set of
# cases, the SAS program that verifies them, and the observed results.

test_that("sas_round() rounds halves away from zero", {
  expect_identical(sas_round(c(0.5, 1.5, 2.5, -0.5, -2.5)), c(1, 2, 3, -1, -3))
  expect_identical(
    sas_round(seq(0, 4, by = 0.25)),
    c(0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4)
  )
})

test_that("sas_round() matches documented SAS examples", {
  x <- 1234.56789
  expect_identical(sas_round(x, digits = -2), 1200)
  expect_identical(sas_round(x, digits = -1), 1230)
  expect_identical(sas_round(x), 1235)
  expect_identical(sas_round(x, digits = 1), 1234.6)
  expect_identical(sas_round(x, digits = 2), 1234.57)
  expect_identical(sas_round(x, digits = 3), 1234.568)
  expect_identical(sas_round(x, digits = 4), 1234.5679)
  expect_identical(sas_round(x, digits = 5), 1234.56789)
  expect_identical(sas_round(x, unit = 0.1111), 1234.5432)
  expect_identical(sas_round(x, unit = 0.11111), 11111 * 0.11111)

  expect_identical(sas_round(0.33, digits = 1), 0.3)
  expect_identical(sas_round(pi, unit = 1 / 7), 22 / 7)
})

test_that("sas_round() matches decimal results with at most four places", {
  # Observed in SAS (fixture cases 538-540). The documentation's condition is
  # on the expected result, not on the unit: 0.00015 has five decimal places
  # but 10 * 0.00015 = 0.0015 has four. 10 * 0.00015 is 1 ulp below 0.0015.
  expect_identical(sas_round(0.0015, unit = 0.00015), 0.0015)
  expect_identical(sas_round(0.0033, unit = 0.00015), 0.0033)
  expect_identical(sas_round(0.0121, unit = 0.00055), 0.0121)

  # Results with more decimal places are the product (as with 0.11111 above)
  expect_identical(sas_round(0.001234, unit = 0.00015), 8 * 0.00015)
})

test_that("sas_round() needs 1 / unit to be close to an integer", {
  # Observed in SAS (fixture case 537): 1 / unit is 20 ulps from 7, so the
  # result is 22 * unit, not 22 / 7.
  unit <- 0x1.2492492492485p-3
  expect_identical(sas_round(pi, unit = unit), 22 * unit)
  expect_false(sas_round(pi, unit = unit) == 22 / 7)
})

test_that("sas_round() gives the same results with digits or unit", {
  x <- c(1234.56789, -0.285, 1.005)
  for (digits in -3:5) {
    expect_identical(
      sas_round(x, digits = digits),
      sas_round(x, unit = 10^-digits)
    )
  }
})

test_that("sas_round() treats values near halfway as halfway", {
  expect_identical(sas_round(1.005, digits = 2), 1.01)
  expect_identical(sas_round(-1.005, digits = 2), -1.01)
  expect_identical(sas_round(2.675, digits = 2), 2.68)
  expect_identical(sas_round(0.285, digits = 2), 0.29)
  expect_identical(sas_round(0.15, digits = 1), 0.2)

  # The documented fuzz is min(1e-6, 1e-12 * value), so 0.5 - 1e-12 rounds
  # down and 0.5 - 1e-13 rounds up.
  expect_identical(sas_round(0.5 - 10^-(8:17)), rep(c(0, 1), each = 5))
  expect_identical(sas_round(-0.5 + 10^-(8:17)), rep(c(0, -1), each = 5))
})

test_that("sas_round() matches SAS exactly 1e-12 from halfway", {
  # Observed in SAS: x / unit is 1e-12 (relative) below 1.5 and 2.5. The
  # decision depends on the last bit of x / unit.
  expect_identical(sas_round(0.15 * (1 - 1e-12), digits = 1), 0.1)
  expect_identical(sas_round(-0.15 * (1 - 1e-12), digits = 1), -0.1)
  expect_identical(sas_round(0.25 * (1 - 1e-12), digits = 1), 0.2)
})

test_that("sas_round() returns large values unchanged", {
  expect_identical(sas_round(2^52 + 1), 2^52 + 1)
  expect_identical(sas_round(2^53), 2^53)
  expect_identical(sas_round(-(2^52 + 1)), -(2^52 + 1))
})

test_that("sas_round() returns large values unchanged when x / unit overflows", {
  # Observed in SAS (with SAS's 1e300, which is a few ulps from R's)
  expect_identical(sas_round(1e300, digits = 15), 1e300)
  expect_identical(sas_round(1e15, unit = 1e-300), 1e15)
  expect_identical(sas_round(1e100, unit = 1e-300), 1e100)
  big <- .Machine$double.xmax
  for (unit in c(1e-300, 1e-15, 0.125, 0.3, 0.7, 1 / 3, 1 / 7, 1e20)) {
    expect_identical(sas_round(big, unit = unit), big)
    expect_identical(sas_round(-big, unit = unit), -big)
  }

  expect_true(is.finite(sas_round(5e9, unit = 1e-300)))
  expect_true(is.finite(sas_round(1.7e308, unit = 0.1111)))
  expect_true(is.finite(sas_round(1.7e308, unit = 0.11111)))

  # Observed in SAS (fixture cases 529-532)
  expect_identical(sas_round(-1e300, digits = 15), -1e300)
  expect_identical(sas_round(5e9, unit = 1e-300), 5e9)
  expect_identical(sas_round(1.7e308, unit = 0.1111), 1.7e308)
  expect_identical(sas_round(1.7e308, unit = 0.11111), 1.7e308)
})

test_that("sas_round() works with very large units", {
  expect_identical(sas_round(1, unit = 1e308), 0)
})

test_that("sas_round() works with very small units", {
  # Observed in SAS: 1e-300 is a valid unit, but .Machine$double.xmin is not
  expect_identical(sas_round(1, unit = 1e-300), 1)
  expect_identical(sas_round(2.675, unit = 1e-300), 2.675)
  expect_identical(sas_round(1e-300, unit = 1e-300), 1e-300)
  expect_identical(sas_round(1e-300, unit = 1e-15), 0)
})

test_that("sas_round() matches cases with documented or decimal answers", {
  cases <- read_sas_round_cases()
  cases <- cases[!is.na(cases$expected), ]
  expected <- cases$expected
  names(expected) <- cases$id

  expect_gt(nrow(cases), 400)
  expect_identical(sas_round_cases(cases), expected)
})

test_that("sas_round() matches results observed in SAS", {
  observed <- read_sas_observed("round")
  # Rows with a note are outside the documented accuracy, e.g. "beyond 15
  # digits" when abs(x) or abs(x / unit) is 1e15 or more.
  exact <- observed[observed$note == "", ]
  expected <- exact$sas
  names(expected) <- exact$id

  expect_gt(nrow(exact), 10000)
  expect_identical(sas_round_cases(exact), expected)
})

test_that("sas_round() matches SAS output", {
  path <- test_path("fixtures", "sas-round", "sas-results.csv")
  skip_if_not(file.exists(path), "SAS results have not been generated.")

  cases <- read_sas_round_cases()
  sas_res <- utils::read.csv(path, colClasses = "character")
  names(sas_res) <- tolower(names(sas_res))
  cases <- merge(cases, sas_res, by = "id")
  cases <- cases[order(as.integer(cases$id)), ]
  expect_identical(nrow(cases), nrow(read_sas_round_cases()))

  # HEX16. writes a missing value as ".". SAS returns a missing value for
  # units it rejects (e.g., 1e-320), where sas_round() gives an error.
  result_hex <- trimws(cases$result_hex)
  sas_missing <- !grepl("^[0-9A-F]{16}$", result_hex)
  for (i in which(sas_missing)) {
    expect_error(
      sas_round(cases$x[i], unit = cases$unit[i]),
      class = "rlang_error"
    )
  }

  cases <- cases[!sas_missing, ]
  expected <- hex_to_dbl(result_hex[!sas_missing])
  names(expected) <- cases$id
  expect_identical(sas_round_cases(cases), expected)
})

test_that("sas_round() handles missing values and attributes", {
  x <- c(a = 1.5, b = NA, c = -2.5)
  expect_identical(sas_round(x), c(a = 2, b = NA, c = -3))
  expect_identical(sas_round(c(NA_real_, NaN)), c(NA_real_, NaN))
  expect_identical(sas_round(numeric(0)), numeric(0))
  expect_identical(sas_round(1:3), c(1, 2, 3))
})

test_that("sas_round() validates inputs", {
  expect_snapshot(error = TRUE, sas_round("a"))
  expect_snapshot(error = TRUE, sas_round(1, unit = 0))
  expect_snapshot(error = TRUE, sas_round(1, unit = -1))
  expect_snapshot(error = TRUE, sas_round(1, unit = c(1, 2)))
  expect_snapshot(error = TRUE, sas_round(1, unit = Inf))
  expect_snapshot(error = TRUE, sas_round(1, unit = .Machine$double.xmin))
  expect_snapshot(error = TRUE, sas_round(1, unit = .Machine$double.xmin / 2))
  expect_snapshot(error = TRUE, sas_round(1, digits = 1.5))
  expect_snapshot(error = TRUE, sas_round(1, digits = 16))
  expect_snapshot(error = TRUE, sas_round(1, digits = 2, unit = 0.01))
})

test_that("sas_format() uses SAS rounding", {
  expect_identical(format(round(1.005, 2), nsmall = 2), "1.00")
  expect_identical(sas_format(1.005, digits = 2), "1.01")
  expect_identical(sas_format(c(0.5, 1.5, 2.5)), c("1", "2", "3"))
  expect_identical(sas_format(0.15, digits = 1), "0.2")
  expect_identical(sas_format(2.675, digits = 2), "2.68")
})

test_that("sas_format() keeps the sign of negative values that round to zero", {
  # Observed in SAS
  expect_identical(sas_format(-0.4), "-0")
  expect_identical(sas_format(-0.04, digits = 1), "-0.0")
  expect_identical(sas_format(-0.001, digits = 2), "-0.00")
  expect_identical(sas_format(c(-0.4, 10)), c("-0", "10"))
  expect_identical(sas_format(c(-0.4, 1)), c("-0", " 1"))
  expect_identical(sas_format(-0.4, width = 4), "  -0")
  # Zero itself, negative zero, and subnormal numbers are shown without a sign
  expect_identical(sas_format(c(0, -0, -5e-324)), c("0", "0", "0"))
})

test_that("sas_format() uses a smaller tolerance than sas_round()", {
  # Observed in SAS: 0.5 - 2^-54 is the largest double below 0.5
  expect_identical(sas_format(0.5 - 2^-54), "1")
  expect_identical(sas_format(-0.5 + 2^-54), "-1")
  # 16 units in the last place below 0.5: halfway for ROUND() but not for
  # formats
  x <- 0.5 - 16 * 2^-54
  expect_identical(sas_format(x), "0")
  expect_identical(sas_round(x), 1)
  expect_identical(sas_format(1.5 - 32 * 2^-53), "1")
})

test_that("sas_format() shows the requested decimal places", {
  expect_identical(sas_format(c(1, 2.25), digits = 1), c("1.0", "2.3"))
  expect_identical(sas_format(1234.56789, digits = 4), "1234.5679")
  expect_identical(sas_format(0.1 + 0.2, digits = 15), "0.300000000000000")
  expect_identical(sas_format(1e-10, digits = 2), "0.00")
  expect_identical(sas_format(1e10 + 0.125, digits = 2), "10000000000.13")
})

test_that("sas_format() pads with zeros beyond 15 significant digits", {
  # Observed in SAS with the 32.15 format
  expect_identical(
    sas_format(1234567.895, digits = 15),
    "1234567.895000000000000"
  )
  expect_identical(sas_format(0.1 + 0.2, digits = 15), "0.300000000000000")
  expect_identical(
    sas_format(123456789.123456, digits = 10),
    "123456789.1234560000"
  )
  expect_identical(sas_format(1e20, digits = 2), "100000000000000000000.00")
})

test_that("sas_format() matches results observed in SAS", {
  observed <- read_sas_observed("format")
  # Rows with a note are outside the documented accuracy: "near halfway"
  # (within a few units in the last place) or "15 or more digits shown".
  exact <- observed[observed$note == "", ]
  res <- mapply(
    function(x, digits) sas_format(x, digits = digits),
    exact$x,
    exact$digits,
    USE.NAMES = FALSE
  )
  names(res) <- exact$id
  expected <- exact$sas_text
  names(expected) <- exact$id

  expect_gt(nrow(exact), 5000)
  expect_identical(res, expected)
})

test_that("sas_format() behaves like format()", {
  expect_identical(sas_format(c(-2.5, 10)), c("-3", "10"))
  expect_identical(
    sas_format(c(a = 1.5, b = NA)),
    c(a = " 2", b = "NA")
  )
  expect_identical(sas_format(numeric(0)), character(0))
  expect_identical(
    sas_format(1234567.891, digits = 2, big.mark = ","),
    "1,234,567.89"
  )
  expect_identical(sas_format(1.5, width = 4), "   2")
  expect_identical(sas_format(1e10, scientific = TRUE), "1e+10")
})

test_that("sas_format() validates inputs", {
  expect_snapshot(error = TRUE, sas_format("a"))
  expect_snapshot(error = TRUE, sas_format(1, digits = 1.5))
  expect_snapshot(error = TRUE, sas_format(1, digits = 16))
  expect_snapshot(error = TRUE, sas_format(1, digits = -1))
  expect_snapshot(error = TRUE, sas_format(1, digits = 3, nsmall = 2))
  expect_snapshot(error = TRUE, sas_format(1, drop0trailing = TRUE))
})
