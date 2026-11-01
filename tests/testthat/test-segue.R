test_that("new_segue() creates objects", {
  res <- new_segue(
    code = list(r = quote(f(x)), sas = list()),
    classes = "thing"
  )
  expect_s3_class(res, c("thing", "segue_object"))
  expect_named(res, c("code", "results", "options", "data_characteristics"))

  res_null <- new_segue(code = list(r = NULL, sas = list()))
  expect_s3_class(res_null, "segue_object")
  expect_null(res_null$code$r)
  expect_equal(res_null$code$sas, list())
})

test_that("new_segue() validates inputs", {
  cls <- list(r = quote(f(x)), sas = list())

  expect_snapshot(error = TRUE, new_segue(code = 1))
  expect_snapshot(error = TRUE, new_segue(code = list(r = NULL)))
  expect_snapshot(error = TRUE, new_segue(code = list(r = 1, sas = list())))
  expect_snapshot(
    error = TRUE,
    new_segue(code = list(r = quote(f(x)), sas = 1))
  )
  expect_snapshot(error = TRUE, new_segue(code = cls, options = 1))
  expect_snapshot(error = TRUE, new_segue(code = cls, results = 1))
  expect_snapshot(error = TRUE, new_segue(code = cls, data_characteristics = 1))
})

test_that("tidy() works on segue objects", {
  y <- factor(c("a", "b", "a", "a"))
  res <- bin_prop_test(y)
  tidy_res <- tidy(res)

  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(tidy_res$estimate, 0.75, ignore_attr = TRUE)
  expect_equal(tidy_res$p.value, res$results$p.value)
})
