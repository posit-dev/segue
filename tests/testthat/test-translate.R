test_that("translate() shows R and SAS code", {
  y <- factor(c("a", "b", "a", "a"))
  res <- bin_prop_test(y)

  expect_snapshot(out <- translate(res))
  expect_identical(out, res)
})

test_that("translate() skips missing code", {
  res <- new_segue(calls = list(r = NULL, sas = NULL))
  expect_snapshot(translate(res))
})

test_that("translate() validates inputs", {
  y <- factor(c("a", "b", "a", "a"))
  res <- bin_prop_test(y)

  expect_snapshot(error = TRUE, translate(1))
  expect_snapshot(error = TRUE, translate(res, extra = TRUE))
})
