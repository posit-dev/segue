# The expected values come from SAS PROC FREQ. See
# fixtures/exact-binom-test-sas/ for the SAS program and the SAS data set it
# wrote.

# ---- Comparisons with PROC FREQ ----------------------------------------------

test_that("the same p-values are missing as in PROC FREQ", {
  cases <- read_sas_binom_fixture()
  res <- sas_binom_fixture_results(cases)
  keep <- !is_sas_binom_tie(cases)

  expect_identical(is.na(res$p_value[keep]), is.na(cases$p_value[keep]))
  expect_all_false(is.na(res$p_value[cases$alternative == "two.sided"]))
})

test_that("p-values match PROC FREQ", {
  cases <- read_sas_binom_fixture()
  res <- sas_binom_fixture_results(cases)
  keep <- !is_sas_binom_tie(cases) & !is.na(cases$p_value)

  # PROC FREQ and pbinom() use different algorithms, so the values agree to
  # about 5.5e-14 but not to the last bit. The tolerance is absolute because
  # small right tails lose their relative precision in both.
  expect_lt(
    max(abs(res$p_value[keep] - cases$p_value[keep])),
    1e-13
  )
})

test_that("small right tails lose precision as in PROC FREQ", {
  cases <- read_sas_binom_fixture()
  res <- sas_binom_fixture_results(cases)
  right <- cases$alternative == "greater" &
    !is.na(cases$p_value) &
    !is_sas_binom_tie(cases)

  # Exactly computed, none of these right tails are zero.
  expect_gt(sum(cases$p_value[right] == 0), 300)
  expect_gt(sum(res$p_value[right] == 0), 300)
  expect_all_true(
    res$p_value[right][cases$p_value[right] == 0] < 1e-13
  )
})

test_that("estimates are identical to PROC FREQ", {
  cases <- read_sas_binom_fixture()
  res <- sas_binom_fixture_results(cases)
  expect_identical(res$estimate, cases$estimate)
})

test_that("tied tails report one tail with the PROC FREQ value", {
  cases <- read_sas_binom_fixture()
  ties <- cases[is_sas_binom_tie(cases), ]
  one_sided <- ties[ties$alternative != "two.sided", ]
  sas_tail <- tapply(one_sided$p_value, one_sided$n, sum, na.rm = TRUE)
  sas_filled <- tapply(!is.na(one_sided$p_value), one_sided$n, sum)
  expect_all_equal(as.vector(sas_filled), 1L)

  n <- as.integer(names(sas_tail))
  res <- purrr::map(n, function(n) {
    exact_binom_test_sas(n / 2, n)
  })
  tails <- purrr::map(res, "tails")
  expect_all_equal(purrr::map_int(tails, function(x) sum(!is.na(x))), 1L)
  expect_lt(
    max(abs(
      purrr::map_dbl(tails, function(x) x[!is.na(x)]) - as.vector(sas_tail)
    )),
    1e-13
  )
  expect_all_equal(purrr::map_dbl(res, "p.value"), 1)
  expect_all_equal(ties$p_value[ties$alternative == "two.sided"], 1)
})

# ---- Worked examples ---------------------------------------------------------

test_that("the two-sided p-value doubles the smaller tail", {
  res <- exact_binom_test_sas(5, 10, p = 0.25)
  right <- 1 - stats::pbinom(4, 10, 0.25)
  expect_identical(res$p.value, 2 * right)
  expect_equal(res$p.value, 0.1562538, tolerance = 1e-6)
})

test_that("an exactly representable left tail is close to the exact value", {
  # With p = 1/4, every term is a multiple of 4^-11, so the exact tail is a
  # double.
  exact <- 1909251 / 4194304
  res <- exact_binom_test_sas(2, 11, p = 0.25, alternative = "less")
  expect_lte(abs(res$p.value - exact), 64 * 2^-54)
  expect_identical(res$tails[["greater"]], NA_real_)
})

test_that("a small right tail is computed by subtraction", {
  res <- exact_binom_test_sas(6, 6, p = 0.01, alternative = "greater")
  exact <- 0.01^6
  expect_identical(res$p.value, 1 - stats::pbinom(5, 6, 0.01))
  # The subtraction leaves only a few significant digits
  expect_gt(abs(res$p.value - exact), 1e-17)
  expect_lt(abs(res$p.value - exact), 2e-16)
})

# ---- Relationship with binom.test() ------------------------------------------

test_that("reported one-sided p-values match binom.test()", {
  grid <- expand.grid(n = c(1, 7, 20, 51), p = c(0.03, 0.4, 0.5, 0.85))
  for (i in seq_len(nrow(grid))) {
    n <- grid$n[i]
    p <- grid$p[i]
    for (x in 0:n) {
      res <- exact_binom_test_sas(x, n, p = p)
      if (!is.na(res$tails[["less"]])) {
        expect_identical(
          res$tails[["less"]],
          stats::binom.test(x, n, p, alternative = "less")$p.value
        )
      } else {
        expect_lt(
          max(abs(
            res$tails[["greater"]] -
              stats::binom.test(x, n, p, alternative = "greater")$p.value
          )),
          1e-13
        )
      }
      expect_identical(
        res$p.value,
        min(1, 2 * min(res$tails, na.rm = TRUE))
      )
    }
  }
})

test_that("exactly one tail is reported, and it is the smaller one", {
  for (x in 0:15) {
    res <- exact_binom_test_sas(x, 15, p = 0.3)
    expect_identical(sum(is.na(res$tails)), 1L)
    left <- stats::pbinom(x, 15, 0.3)
    right <- 1 - stats::pbinom(x - 1, 15, 0.3)
    expect_identical(unname(res$tails[!is.na(res$tails)]), min(left, right))
  }
})

test_that("the result has the same elements as binom.test()", {
  res <- exact_binom_test_sas(7, 23, p = 0.1)
  ref <- stats::binom.test(7, 23, p = 0.1)
  expect_s3_class(res, "htest")
  expect_named(res, c(setdiff(names(ref), "conf.int"), "tails"))
  expect_named(res$tails, c("less", "greater"))
  for (nm in c("statistic", "parameter", "estimate", "null.value")) {
    expect_identical(res[[nm]], ref[[nm]])
  }
  expect_identical(res$data.name, "7 and 23")
  expect_snapshot(res)
})

test_that("successes and failures can be given as x", {
  expect_identical(
    exact_binom_test_sas(c(7, 16), p = 0.1)[1:7],
    exact_binom_test_sas(7, 23, p = 0.1)[1:7]
  )
  expect_identical(exact_binom_test_sas(c(7, 16))$data.name, "c(7, 16)")
})

# ---- Edge cases --------------------------------------------------------------

test_that("no successes or no failures give the right tails", {
  none <- exact_binom_test_sas(0, 8, p = 0.2)
  expect_equal(none$tails[["less"]], 0.8^8)
  expect_equal(none$p.value, 2 * 0.8^8)

  all <- exact_binom_test_sas(8, 8, p = 0.2, alternative = "greater")
  expect_identical(all$p.value, 1 - stats::pbinom(7, 8, 0.2))
  expect_equal(all$p.value, 0.2^8, tolerance = 1e-8)
})

test_that("null values of 0 and 1 are allowed", {
  expect_identical(exact_binom_test_sas(2, 8, p = 0)$p.value, 0)
  expect_identical(exact_binom_test_sas(0, 8, p = 0)$p.value, 1)
  expect_identical(exact_binom_test_sas(2, 8, p = 1)$p.value, 0)
  expect_identical(exact_binom_test_sas(8, 8, p = 1)$p.value, 1)
})

# ---- Errors ------------------------------------------------------------------

test_that("bad input gives informative errors", {
  expect_snapshot(error = TRUE, {
    exact_binom_test_sas(2.5, 10)
    exact_binom_test_sas(-1, 10)
    exact_binom_test_sas(NA, 10)
    exact_binom_test_sas("a", 10)
    exact_binom_test_sas(c(1, 2, 3))
    exact_binom_test_sas(11, 10)
    exact_binom_test_sas(1, 0)
    exact_binom_test_sas(1, 10, p = 1.5)
    exact_binom_test_sas(1, 10, p = c(0.1, 0.2))
    exact_binom_test_sas(1, 10, alternative = "lesser")
  })
})
