test_that("convert_alternative() converts values", {
  expect_equal(convert_alternative("two_sided"), "two.sided")
  expect_equal(convert_alternative("greater"), "greater")
  expect_equal(convert_alternative("lesser"), "less")
})

test_that("convert_alternative() validates inputs", {
  expect_snapshot(error = TRUE, convert_alternative("less"))
  expect_snapshot(error = TRUE, convert_alternative("two.sided"))
})
