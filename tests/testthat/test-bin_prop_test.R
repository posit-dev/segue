test_that("bin_prop_test() returns a segue object", {
  y <- factor(c("a", "b", "a", "a", NA, "b", "a"))
  res <- bin_prop_test(y)

  expect_s3_class(res, c("bin_prop_test", "segue_object"))
  expect_named(res, c("code", "results", "options", "data_characteristics"))
  expect_named(res$code, c("r", "sas"))
  expect_equal(
    res$options,
    list(
      method = "exact",
      event_level = "a",
      alternative = "two_sided",
      null_value = 0.5
    )
  )
  expect_equal(res$data_characteristics$missing, 1L)
  expect_equal(res$data_characteristics$events, 4L)
  expect_equal(res$data_characteristics$total, 6L)
  expect_s3_class(res$results, "htest")
  expect_equal(
    res$results$p.value,
    stats::binom.test(4, 6)$p.value
  )
  expect_equal(
    res$code$r,
    rlang::call2(
      "binom.test",
      .ns = "stats",
      x = 4L,
      n = 6L,
      alternative = "two.sided",
      p = 0.5
    )
  )
})

test_that("bin_prop_test() passes options to binom.test()", {
  y <- factor(c("a", "b", "a", "a", "b", "a"))
  res <- bin_prop_test(
    y,
    event_level = "b",
    null_value = 0.2,
    alternative = "greater"
  )

  expect_equal(res$data_characteristics$events, 2L)
  expect_equal(res$data_characteristics$missing, 0L)
  expect_equal(
    res$results$p.value,
    stats::binom.test(2, 6, p = 0.2, alternative = "greater")$p.value
  )
})

test_that("bin_prop_test() can use exact_binom_test_sas()", {
  y <- factor(rep(c("a", "b", "a", NA), c(12, 8, 5, 2)))
  res <- bin_prop_test(
    y,
    method = "exact_sas",
    null_value = 0.3,
    alternative = "greater"
  )

  expect_s3_class(res, c("bin_prop_test", "segue_object"))
  expect_equal(res$options$method, "exact_sas")
  expect_equal(res$data_characteristics$missing, 2L)
  expect_equal(res$data_characteristics$events, 17L)
  expect_equal(res$data_characteristics$total, 25L)
  expect_equal(
    res$results,
    exact_binom_test_sas(17L, 25L, p = 0.3, alternative = "greater")
  )
  expect_equal(
    res$code$r,
    rlang::call2(
      "exact_binom_test_sas",
      .ns = "segue",
      x = 17L,
      n = 25L,
      alternative = "greater",
      p = 0.3
    )
  )
  expect_equal(
    res$code$sas,
    list(proc = "freq", tables = "resp / binomial", exact = "binomial")
  )
})

test_that("bin_prop_test() can use prop.test()", {
  y <- factor(rep(c("a", "b", "a", NA), c(12, 8, 5, 2)))
  res <- bin_prop_test(
    y,
    method = "score",
    event_level = "b",
    null_value = 0.3,
    alternative = "lesser"
  )

  expect_s3_class(res, c("bin_prop_test", "segue_object"))
  expect_equal(res$options$method, "score")
  expect_equal(res$data_characteristics$events, 8L)
  expect_equal(res$data_characteristics$total, 25L)
  expect_equal(
    res$results$p.value,
    stats::prop.test(
      8,
      25,
      p = 0.3,
      alternative = "less",
      correct = FALSE
    )$p.value
  )
  expect_equal(
    res$code$r,
    rlang::call2(
      "prop.test",
      .ns = "stats",
      x = 8L,
      n = 25L,
      alternative = "less",
      p = 0.3,
      correct = FALSE
    )
  )
  expect_equal(
    res$code$sas,
    list(
      proc = "freq",
      data = "{data set}",
      tables = "{analysis column} / binomial(cl=score)"
    )
  )
})

test_that("bin_prop_test() reports errors from the underlying test", {
  all_missing <- factor(c(NA, NA), levels = c("a", "b"))

  expect_snapshot(error = TRUE, bin_prop_test(all_missing, method = "exact"))
  expect_snapshot(
    error = TRUE,
    bin_prop_test(all_missing, method = "exact_sas")
  )
  expect_snapshot(error = TRUE, bin_prop_test(all_missing, method = "score"))
})

test_that("bin_prop_test() creates SAS code", {
  skip("until new methods available")
  y <- factor(c("a", "b", "a", "a", "b", "a"))

  expect_snapshot(rlang::exec("sas_proc", !!!(bin_prop_test(y)$code$sas)))

  expect_snapshot(
    rlang::exec(
      "sas_proc",
      !!!(bin_prop_test(y, null_value = 0.2)$code$sas)
    )
  )
  expect_snapshot(
    rlang::exec(
      "sas_proc",
      !!!(bin_prop_test(y, event_level = "b")$code$sas)
    )
  )
  expect_snapshot(
    rlang::exec(
      "sas_proc",
      !!!(bin_prop_test(y, event_level = "b", null_value = 0.2)$code$sas)
    )
  )
})

test_that("bin_prop_test() validates inputs", {
  y <- factor(c("a", "b", "a"))

  expect_snapshot(error = TRUE, bin_prop_test(1:3))
  expect_snapshot(error = TRUE, bin_prop_test(factor(c("a", "a"))))
  expect_snapshot(error = TRUE, bin_prop_test(y, event_level = "c"))
  expect_snapshot(error = TRUE, bin_prop_test(y, alternative = "two.sided"))
  expect_snapshot(error = TRUE, bin_prop_test(y, null_value = 2))
  expect_snapshot(error = TRUE, bin_prop_test(y, conf.level = 0.9))
})

test_that("bin_prop_test() print method", {
  y <- factor(c("a", "b", "a"))
  y_miss <- factor(c("a", "b", NA, "a", NA))
  y_one <- factor(c("a", "b", NA))

  expect_snapshot(res <- print(bin_prop_test(y)))
  expect_s3_class(res, "bin_prop_test")
  expect_snapshot(print(bin_prop_test(y_miss)))
  expect_snapshot(print(bin_prop_test(y_one)))
})
