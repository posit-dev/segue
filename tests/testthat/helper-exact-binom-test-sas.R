# Proc Freq results written by
# fixtures/exact-binom-test-sas/bin_prop_test.sas. The SAS data set stores
# 8-byte doubles, so haven reads the values exactly as SAS computed them.
read_sas_binom_fixture <- function() {
  skip_if_not_installed("haven")
  path <- test_path(
    "fixtures",
    "exact-binom-test-sas",
    "bin_prop_test_sas.sas7bdat"
  )
  cases <- haven::read_sas(path)
  cases <- haven::zap_labels(haven::zap_formats(cases))
  data.frame(
    p = cases$p,
    alternative = cases$alternative,
    x = as.integer(cases$num_events),
    n = as.integer(cases$num_total),
    estimate = cases$estimate,
    p_value = cases$p_value
  )
}

# Runs exact_binom_test_sas() for every row of the fixture.
sas_binom_fixture_results <- function(cases) {
  res <- purrr::pmap(
    list(cases$x, cases$n, cases$p, cases$alternative),
    function(x, n, p, alternative) {
      exact_binom_test_sas(x, n, p = p, alternative = alternative)
    }
  )
  data.frame(
    estimate = purrr::map_dbl(res, function(r) unname(r$estimate)),
    p_value = purrr::map_dbl(res, "p.value")
  )
}

# p_0 = 0.5 and x = n / 2, where the tails are equal and which one is reported
# depends on rounding error.
is_sas_binom_tie <- function(cases) {
  cases$p == 0.5 & 2 * cases$x == cases$n
}

# Both one-sided p-values, with NA for the tail that Proc Freq does not report.
reported_tails <- function(x, n, p = 0.5) {
  c(
    less = exact_binom_test_sas(x, n, p = p, alternative = "less")$p.value,
    greater = exact_binom_test_sas(x, n, p = p, alternative = "greater")$p.value
  )
}
