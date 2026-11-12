test_that("translate() shows R and SAS code", {
  y <- factor(c("a", "b", "a", "a"))
  res <- bin_prop_test(y)

  expect_snapshot(out <- translate(res))
  expect_identical(out, res)
})

test_that("translate() shows SAS code when it is available", {
  y <- factor(rep(c("a", "b"), c(15, 10)))

  expect_snapshot(translate(bin_prop_test(y, method = "exact_sas")))
  expect_snapshot(translate(bin_prop_test(y, method = "score")))
})

test_that("translate() skips missing code", {
  res <- new_segue(code = list(r = NULL, sas = list()))
  expect_snapshot(translate(res))
})

test_that("translate() validates inputs", {
  y <- factor(c("a", "b", "a", "a"))
  res <- bin_prop_test(y)

  expect_snapshot(error = TRUE, translate(1))
  expect_snapshot(error = TRUE, translate(res, extra = TRUE))
})
