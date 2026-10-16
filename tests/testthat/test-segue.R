test_that("new_segue() creates objects", {
  res <- new_segue(
    calls = list(r = quote(f(x)), sas = "run;"),
    classes = "thing"
  )
  expect_s3_class(res, c("thing", "segue_object"))
  expect_named(res, c("calls", "results", "options", "data"))

  res_null <- new_segue(calls = list(r = NULL, sas = NULL))
  expect_s3_class(res_null, "segue_object")
  expect_null(res_null$calls$r)
  expect_null(res_null$calls$sas)
})

test_that("new_segue() validates inputs", {
  cls <- list(r = quote(f(x)), sas = "run;")

  expect_snapshot(error = TRUE, new_segue(calls = 1))
  expect_snapshot(error = TRUE, new_segue(calls = list(r = NULL)))
  expect_snapshot(error = TRUE, new_segue(calls = list(r = 1, sas = "run;")))
  expect_snapshot(
    error = TRUE,
    new_segue(calls = list(r = quote(f(x)), sas = 1))
  )
  expect_snapshot(error = TRUE, new_segue(calls = cls, options = 1))
  expect_snapshot(error = TRUE, new_segue(calls = cls, results = 1))
  expect_snapshot(error = TRUE, new_segue(calls = cls, data = 1))
})

test_that("tidy() works on segue objects", {
  y <- factor(c("a", "b", "a", "a"))
  res <- bin_prop_test(y)
  tidy_res <- tidy(res)

  expect_s3_class(tidy_res, "tbl_df")
  expect_equal(tidy_res$estimate, 0.75, ignore_attr = TRUE)
  expect_equal(tidy_res$p.value, res$results$p.value)
})
