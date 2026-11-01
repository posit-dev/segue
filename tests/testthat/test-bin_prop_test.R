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

test_that("bin_prop_test() creates SAS code", {
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
