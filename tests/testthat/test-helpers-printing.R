test_that("report_missing_simple() is silent without missing data counts", {
  res <- new_segue(code = list(r = NULL, sas = list()))

  expect_silent(out <- report_missing_simple(res))
  expect_identical(out, res)
})
