test_that("numeric_rank_test() returns a segue object", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  res <- numeric_rank_test(y, g, data = df)

  expect_s3_class(res, c("numeric_rank_test", "segue_object"))
  expect_named(res, c("code", "results", "options", "data_characteristics"))
  expect_named(res$code, c("r", "sas"))
  expect_equal(
    res$options,
    list(
      method = "wilcoxon",
      outcome = "y",
      group = "g",
      strata = NULL,
      alternative = "two_sided",
      distribution = "asymptotic",
      reference_choice = "smaller"
    )
  )
  expect_equal(
    res$data_characteristics,
    list(missing = 1L, total = 11L, groups = 2L, strata = 1L, reference = "b")
  )
  expect_s4_class(res$results, "ScalarIndependenceTest")
  expect_equal(
    res$code$r,
    rlang::call2(
      "wilcox_test",
      .ns = "coin",
      quote(y ~ g),
      data = quote(data),
      alternative = "two.sided",
      distribution = "asymptotic"
    )
  )
})

test_that("numeric_rank_test() selects columns with tidyselect", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  res <- numeric_rank_test(y, g, data = df)
  res_chr <- numeric_rank_test("y", tidyselect::starts_with("g"), data = df)

  expect_equal(res_chr$options, res$options)
  expect_equal(res_chr$code$r, res$code$r)
})

test_that("numeric_rank_test() can use method = 'wilcoxon'", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  complete <- df[!is.na(df$y), ]
  # "b" has fewer observations, so it is the reference group by default.
  complete_b <- complete
  complete_b$g <- factor(complete_b$g, levels = c("b", "a"))
  res <- numeric_rank_test(y, g, data = df)
  res_greater <- numeric_rank_test(y, g, data = df, alternative = "greater")

  expect_equal(
    tidy(res)$p.value,
    stats::wilcox.test(y ~ g, data = complete_b, exact = FALSE)$p.value
  )
  expect_equal(
    tidy(res_greater)$p.value,
    stats::wilcox.test(
      y ~ g,
      data = complete_b,
      exact = FALSE,
      alternative = "greater"
    )$p.value
  )
  expect_equal(
    res$code$sas,
    list(
      proc = "npar1way",
      options = "wilcoxon",
      class = "&group",
      var = "&var"
    )
  )
})

test_that("numeric_rank_test() can use the first level as the reference", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  complete <- df[!is.na(df$y), ]
  res_smaller <- numeric_rank_test(y, g, data = df)
  res_first <- numeric_rank_test(y, g, data = df, reference_choice = "first")
  res_lesser <- numeric_rank_test(
    y,
    g,
    data = df,
    alternative = "lesser",
    reference_choice = "first"
  )

  expect_equal(res_first$options$reference_choice, "first")
  expect_equal(res_first$data_characteristics$reference, "a")
  expect_equal(tidy(res_first)$statistic, -tidy(res_smaller)$statistic)
  expect_equal(tidy(res_first)$p.value, tidy(res_smaller)$p.value)
  expect_equal(
    tidy(res_lesser)$p.value,
    stats::wilcox.test(
      y ~ g,
      data = complete,
      exact = FALSE,
      alternative = "less"
    )$p.value
  )
})

test_that("numeric_rank_test() keeps the level order for groups of equal size", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  df <- df[!is.na(df$y), ][-1, ]
  res <- numeric_rank_test(y, g, data = df)

  expect_equal(res$data_characteristics$reference, "a")
})

test_that("numeric_rank_test() uses coin's p-value for exact tests", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  complete <- df[!is.na(df$y), ]
  res <- numeric_rank_test(y, g, data = df, distribution = "exact")

  expect_equal(
    tidy(res)$p.value,
    stats::wilcox.test(y ~ g, data = complete, exact = TRUE)$p.value
  )
  expect_equal(
    res$code$r,
    rlang::call2(
      "wilcox_test",
      .ns = "coin",
      quote(y ~ g),
      data = quote(data),
      alternative = "two.sided",
      distribution = "exact"
    )
  )
  expect_equal(res$code$sas$exact, "wilcoxon")
})

test_that("numeric_rank_test() can use method = 'wilcoxon_stratified'", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  complete <- df[!is.na(df$y), ]
  res <- numeric_rank_test(
    y,
    g,
    data = df,
    strata = s,
    method = "wilcoxon_stratified"
  )
  expected_z <- van_elteren_z(complete$y, complete$g, complete$s)

  expect_s3_class(res, c("numeric_rank_test", "segue_object"))
  expect_equal(res$options$method, "wilcoxon_stratified")
  expect_equal(res$options$strata, "s")
  expect_equal(res$data_characteristics$strata, 3L)
  expect_equal(tidy(res)$statistic, expected_z)
  expect_equal(tidy(res)$p.value, 2 * stats::pnorm(-abs(expected_z)))
  expect_equal(
    res$code$r,
    rlang::call2(
      "independence_test",
      .ns = "coin",
      quote(y ~ g | s),
      data = quote(data),
      ytrafo = quote(segue::van_elteren_trafo(data[["s"]])),
      alternative = "two.sided",
      distribution = "asymptotic"
    )
  )
  expect_equal(
    res$code$sas,
    list(
      proc = "freq",
      tables = "&strata*&group*&var / cmh2 scores=modridit noprint"
    )
  )
})

test_that("van_elteren_trafo() computes van Elteren's scores", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  df <- df[!is.na(df$y), ]
  strata <- factor(df$s)
  scores <- van_elteren_trafo(strata)(df["y"])

  expect_equal(
    as.vector(scores),
    stats::ave(df$y, strata, FUN = function(y) rank(y) / (length(y) + 1))
  )
  expect_snapshot(error = TRUE, van_elteren_trafo(df$s))
})

test_that("numeric_rank_test() can use method = 'kruskal_wallis'", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  complete <- df[!is.na(df$y), ]
  res <- numeric_rank_test(y, k, data = df, method = "kruskal_wallis")
  expected <- stats::kruskal.test(y ~ k, data = complete)

  expect_equal(res$data_characteristics$groups, 3L)
  expect_equal(tidy(res)$statistic, unname(expected$statistic))
  expect_equal(tidy(res)$parameter, 2L)
  expect_equal(tidy(res)$p.value, expected$p.value)
  expect_equal(
    res$code$r,
    rlang::call2(
      "kruskal_test",
      .ns = "coin",
      quote(y ~ k),
      data = quote(data),
      distribution = "asymptotic"
    )
  )
  expect_equal(
    res$code$sas,
    list(
      proc = "npar1way",
      options = "wilcoxon",
      class = "&group",
      var = "&var"
    )
  )
})

test_that("numeric_rank_test() reports errors from the underlying engine", {
  skip_if_not_installed("coin")
  bad <- data.frame(y = c("a", "b"), g = c(1, 2), s = c(1, 1))
  settings <- list(
    outcome = "y",
    group = "g",
    strata = "s",
    distribution = "asymptotic"
  )

  expect_snapshot(
    error = TRUE,
    numeric_rank_test_wilcoxon(bad, settings = settings)
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test_wilcoxon_stratified(bad, settings = settings)
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test_kruskal_wallis(bad, settings = settings)
  )
})

test_that("numeric_rank_test() validates inputs", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  df$chr <- letters[seq_len(nrow(df))]
  df$num <- seq_len(nrow(df))

  expect_snapshot(error = TRUE, numeric_rank_test(y, g, data = as.matrix(df)))
  expect_snapshot(error = TRUE, numeric_rank_test(c(y, num), g, data = df))
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(y, g, data = df, method = "t")
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(y, g, data = df, alternative = "less")
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(y, g, data = df, distribution = "normal")
  )
  expect_snapshot(error = TRUE, numeric_rank_test(chr, g, data = df))
  expect_snapshot(error = TRUE, numeric_rank_test(y, num, data = df))
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(
      y,
      g,
      data = df,
      strata = num,
      method = "wilcoxon_stratified"
    )
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(y, g, data = df, method = "wilcoxon_stratified")
  )
  expect_snapshot(error = TRUE, numeric_rank_test(y, g, data = df, strata = s))
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(
      y,
      k,
      data = df,
      method = "kruskal_wallis",
      alternative = "greater"
    )
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(
      y,
      k,
      data = df,
      method = "kruskal_wallis",
      distribution = "exact"
    )
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(y, g, data = df, reference_choice = "largest")
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(
      y,
      k,
      data = df,
      method = "kruskal_wallis",
      reference_choice = "smaller"
    )
  )
  expect_snapshot(error = TRUE, numeric_rank_test(y, k, data = df))
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(y, g, data = df[df$g == "a", ], method = "kruskal_wallis")
  )
  expect_snapshot(
    error = TRUE,
    numeric_rank_test(y, g, data = df, not_an_arg = 1)
  )
})

test_that("numeric_rank_test() creates SAS code", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  sas_code <- function(...) {
    rlang::exec("sas_proc", !!!numeric_rank_test(..., data = df)$code$sas)
  }

  expect_snapshot(sas_code(y, g))
  expect_snapshot(sas_code(
    y,
    g,
    alternative = "greater",
    distribution = "exact"
  ))
  expect_snapshot(sas_code(y, g, distribution = "approximate"))
  expect_snapshot(sas_code(y, g, strata = s, method = "wilcoxon_stratified"))
  expect_equal(
    numeric_rank_test(
      y,
      g,
      data = df,
      strata = s,
      method = "wilcoxon_stratified",
      distribution = "exact"
    )$code$sas,
    list()
  )
  expect_snapshot(sas_code(y, k, method = "kruskal_wallis"))
  expect_snapshot(
    sas_code(y, k, method = "kruskal_wallis", distribution = "approximate")
  )
})

test_that("numeric_rank_test() print method", {
  skip_if_not_installed("coin")
  df <- rank_test_data()
  df_none <- df[!is.na(df$y), ]
  df_two <- df
  df_two$g[1] <- NA

  expect_snapshot(res <- print(numeric_rank_test(y, g, data = df_none)))
  expect_s3_class(res, "numeric_rank_test")
  expect_snapshot(print(numeric_rank_test(y, g, data = df)))
  expect_snapshot(print(numeric_rank_test(y, g, data = df_two)))
  expect_snapshot(
    print(
      numeric_rank_test(y, k, data = df, method = "kruskal_wallis")
    )
  )
})
