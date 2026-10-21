test_that("statements are written in slot order", {
  res <- sas_proc(
    "mixed",
    lsmeans = "&trt / diff cl",
    model = "&y = &trt / solution ddfm=kr",
    by = "&site",
    class = "&trt &subj",
    options = c(method = "reml")
  )
  expect_equal(
    as.character(res),
    paste(
      "proc mixed data=&dsn method=reml;",
      "  class &trt &subj;",
      "  model &y = &trt / solution ddfm=kr;",
      "  lsmeans &trt / diff cl;",
      "  by &site;",
      "run;",
      sep = "\n"
    )
  )
})

test_that("interactive procs end with quit", {
  expect_match(as.character(sas_proc("reg")), "quit;$")
  expect_match(as.character(sas_proc("GLM")), "quit;$")
  expect_match(as.character(sas_proc("logistic")), "run;$")
})

test_that("default statements are added when left out", {
  expect_equal(
    as.character(sas_proc("freq")),
    "proc freq data=&dsn;\n  tables &row*&col;\nrun;"
  )
  # A given statement replaces the default
  expect_no_match(
    as.character(sas_proc("freq", tables = "&a*&b")),
    "&row"
  )
  # NULL leaves out the default
  expect_no_match(
    as.character(sas_proc("mixed", class = NULL)),
    "class"
  )
  # Unknown procs have no defaults
  expect_equal(
    as.character(sas_proc("sgplot", data = NULL)),
    "proc sgplot;\nrun;"
  )
})

test_that("named statement arguments work like other statements", {
  # Passed statements replace the default, others keep it
  expect_equal(
    as.character(sas_proc("mixed", model = "&y = &trt &visit")),
    paste(
      "proc mixed data=&dsn;",
      "  class &trt &subj;",
      "  model &y = &trt &visit;",
      "run;",
      sep = "\n"
    )
  )
  # A variable holding NULL also leaves out the default
  no_class <- NULL
  expect_no_match(
    as.character(sas_proc("ttest", class = no_class)),
    "class"
  )
  # Named and extra statements are sorted together
  expect_equal(
    as.character(
      sas_proc("phreg", strata = "&site", by = "&study", where = "&age > 18")
    ),
    paste(
      "proc phreg data=&dsn;",
      "  model &time*&censor(0) = &trt;",
      "  strata &site;",
      "  by &study;",
      "  where &age > 18;",
      "run;",
      sep = "\n"
    )
  )
})

test_that("statement options are split on the first unquoted slash", {
  res <- sas_proc(
    "logistic",
    model = "&y(event='a/b') = &x / clodds=pl",
    estimate = "'x/y' &trt 1 -1 / exp",
    estimate = "'t1' &trt 1 0"
  )
  expect_equal(
    as.character(res),
    paste(
      "proc logistic data=&dsn;",
      "  model &y(event='a/b') = &x / clodds=pl;",
      "  estimate 'x/y' &trt 1 -1 / exp;",
      "  estimate 't1' &trt 1 0;",
      "run;",
      sep = "\n"
    )
  )
})

test_that("vector statements and proc options are joined", {
  res <- sas_proc(
    "means",
    class = c("&trt", "&sex"),
    options = c("n", "mean", alpha = 0.05)
  )
  expect_match(as.character(res), "proc means data=&dsn n mean alpha=0.05;")
  expect_match(as.character(res), "class &trt &sex;")
})

test_that("printing uses syntax colors", {
  local_reproducible_output(crayon = TRUE)
  expect_snapshot(sas_proc(
    "logistic",
    model = "&y(event='1') = age / alpha=0.05"
  ))
})

test_that("bad input gives informative errors", {
  expect_snapshot(error = TRUE, {
    sas_proc("freq", "&a*&b")
    sas_proc("freq", tables = 1)
    sas_proc(c("freq", "means"))
    sas_proc("freq", options = list("chisq"))
  })
})
