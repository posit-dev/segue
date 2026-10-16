test_that("check_factor() validates factors", {
  expect_null(check_factor(factor(c("a", "b"))))
  expect_null(check_factor(factor("a"), mult_lvls = FALSE))

  expect_snapshot(error = TRUE, check_factor(letters))
  expect_snapshot(error = TRUE, check_factor(factor("a")))
  expect_snapshot(error = TRUE, check_factor(1:2, nm = "outcome"))
})

test_that("check_level() validates levels", {
  y <- factor(c("a", "b"), levels = c("b", "a"))

  expect_equal(check_level(NULL, y), "b")
  expect_equal(check_level("a", y), "a")

  expect_snapshot(error = TRUE, check_level(c("a", "b"), y))
  expect_snapshot(error = TRUE, check_level(NA_character_, y))
  expect_snapshot(error = TRUE, check_level("c", y))
})

test_that("check_empty_ellipse() catches extra arguments", {
  expect_length(check_empty_ellipse(), 0)
  expect_snapshot(error = TRUE, check_empty_ellipse(a = 1, b = 2))
})
