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
  expect_identical(sas_round(x, 100), 1200)
  expect_identical(sas_round(x, 10), 1230)
  expect_identical(sas_round(x), 1235)
  expect_identical(sas_round(x, 0.1), 1234.6)
  expect_identical(sas_round(x, 0.01), 1234.57)
  expect_identical(sas_round(x, 0.001), 1234.568)
  expect_identical(sas_round(x, 0.0001), 1234.5679)
  expect_identical(sas_round(x, 0.00001), 1234.56789)
  expect_identical(sas_round(x, 0.1111), 1234.5432)
  expect_identical(sas_round(x, 0.11111), 11111 * 0.11111)

  expect_identical(sas_round(0.33, 0.1), 0.3)
  expect_identical(sas_round(pi, 1 / 7), 22 / 7)
})

test_that("sas_round() treats values near halfway as halfway", {
  expect_identical(sas_round(1.005, 0.01), 1.01)
  expect_identical(sas_round(-1.005, 0.01), -1.01)
  expect_identical(sas_round(2.675, 0.01), 2.68)
  expect_identical(sas_round(0.285, 0.01), 0.29)
  expect_identical(sas_round(0.15, 0.1), 0.2)

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
})
