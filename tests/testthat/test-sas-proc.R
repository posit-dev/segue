test_that("code is written in SAS order with defaults", {
  expect_snapshot({
    # Slot order, default model, named proc option
    sas_proc(
      "mixed",
      data = "mtcars",
      lsmeans = "cyl / diff cl",
      by = "am",
      class = "cyl",
      options = c(method = "reml")
    )
    # Slashes in quotes, repeated statements
    sas_proc(
      "logistic",
      data = "mtcars",
      model = "am(event='1') = wt hp / clodds=pl",
      estimate = "'wt/hp' wt 1 hp -1 / exp",
      estimate = "'wt' wt 1"
    )
    # Vector statements, mixed proc options
    sas_proc(
      "means",
      data = "mtcars",
      class = c("cyl", "am"),
      var = "mpg",
      options = c("n", alpha = 0.05)
    )
    # Upper case name, default data, interactive proc ends with quit
    sas_proc("REG")
    # Unknown proc, no data
    sas_proc("sgplot", data = NULL)
  })
})

test_that("NULL leaves out a default statement", {
  expect_no_match(as.character(sas_proc("mixed", class = NULL)), "class")
  no_class <- NULL
  expect_no_match(as.character(sas_proc("ttest", class = no_class)), "class")
})

test_that("printing uses syntax colors", {
  local_reproducible_output(crayon = TRUE)
  expect_snapshot(sas_proc(
    "logistic",
    model = "am(event='1') = wt / alpha=0.05"
  ))
})

test_that("bad input gives informative errors", {
  expect_snapshot(error = TRUE, {
    sas_proc("freq", "cyl*am")
    sas_proc("freq", tables = 1)
    sas_proc(c("freq", "means"))
    sas_proc("freq", options = list("chisq"))
  })
})
