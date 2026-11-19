test_that("check_factor() validates factors", {
  expect_null(check_factor(factor(c("a", "b"))))
  expect_null(check_factor(factor("a"), mult_lvls = FALSE))

  expect_snapshot(error = TRUE, check_factor(letters))
  expect_snapshot(error = TRUE, check_factor(factor("a")))
  expect_snapshot(error = TRUE, check_factor(1:2, arg = "outcome"))
})

test_that("check_level() validates levels", {
  y <- factor(c("a", "b"), levels = c("b", "a"))

  expect_equal(check_level(NULL, y), "b")
  expect_equal(check_level("a", y), "a")

  expect_snapshot(error = TRUE, check_level(c("a", "b"), y))
  expect_snapshot(error = TRUE, check_level(NA_character_, y))
  expect_snapshot(error = TRUE, check_level("c", y))
})

test_that("select_single_column() selects one column", {
  df <- data.frame(a = 1, b = 2, c = 3)

  expect_equal(select_single_column(rlang::quo(b), df, "x"), "b")
  expect_equal(select_single_column(rlang::quo("c"), df, "x"), "c")

  expect_snapshot(
    error = TRUE,
    select_single_column(rlang::quo(c(a, b)), df, "x")
  )
  expect_snapshot(
    error = TRUE,
    select_single_column(rlang::quo(starts_with("z")), df, "x")
  )
  expect_snapshot(error = TRUE, select_single_column(rlang::quo(z), df, "x"))
})
