# Expected values come from the SAS ROUND documentation or exact decimal
# arithmetic. See fixtures/sas-round/ for the full set of cases and the SAS
# program that verifies them.

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

test_that("sas_round() returns large values unchanged", {
  expect_identical(sas_round(2^52 + 1), 2^52 + 1)
  expect_identical(sas_round(2^53), 2^53)
  expect_identical(sas_round(-(2^52 + 1)), -(2^52 + 1))
})

test_that("sas_round() matches cases with documented or decimal answers", {
  cases <- read_sas_round_cases()
  cases <- cases[!is.na(cases$expected), ]
  expected <- cases$expected
  names(expected) <- cases$id

  expect_gt(nrow(cases), 400)
  expect_identical(sas_round_cases(cases), expected)
})

test_that("sas_round() matches SAS output", {
  path <- test_path("fixtures", "sas-round", "sas-results.csv")
  skip_if_not(file.exists(path), "SAS results have not been generated.")

  cases <- read_sas_round_cases()
  sas_res <- utils::read.csv(path, colClasses = "character")
  names(sas_res) <- tolower(names(sas_res))
  cases <- merge(cases, sas_res, by = "id")
  cases <- cases[order(as.integer(cases$id)), ]
  expected <- hex_to_dbl(cases$result_hex)
  names(expected) <- cases$id

  expect_identical(nrow(cases), nrow(read_sas_round_cases()))
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
  expect_snapshot(error = TRUE, sas_round(1, digits = 1.5))
  expect_snapshot(error = TRUE, sas_round(1, digits = 16))
  expect_snapshot(error = TRUE, sas_round(1, digits = 2, unit = 0.01))
})

test_that("sas_format() uses SAS rounding", {
  expect_identical(format(round(1.005, 2), nsmall = 2), "1.00")
  expect_identical(sas_format(1.005, digits = 2), "1.01")
  expect_identical(sas_format(c(0.5, 1.5, 2.5)), c("1", "2", "3"))
  expect_identical(sas_format(0.15, digits = 1), "0.2")
  expect_identical(sas_format(-0.4), "0")
})

test_that("sas_format() shows the requested decimal places", {
  expect_identical(sas_format(c(1, 2.25), digits = 1), c("1.0", "2.3"))
  expect_identical(sas_format(1234.56789, digits = 4), "1234.5679")
  expect_identical(sas_format(0.1 + 0.2, digits = 15), "0.300000000000000")
  expect_identical(sas_format(1e-10, digits = 2), "0.00")
  expect_identical(sas_format(1e10 + 0.125, digits = 2), "10000000000.13")
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
})
