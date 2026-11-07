
<!-- README.md is generated from README.Rmd. Please edit that file -->

# segue

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/segue)](https://CRAN.R-project.org/package=segue)
[![R-CMD-check](https://github.com/posit-dev/segue/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/posit-dev/segue/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/posit-dev/segue/graph/badge.svg)](https://app.codecov.io/gh/posit-dev/segue)
<!-- badges: end -->

segue is a package designed to help people transition from SAS to R with
a focus on clinical statistics. Some issues that make that transition
difficult are:

- SAS has a more limited number of options than R. It is often hard to
  discover if there is an R package that fits their needs, and when
  there are many options, picking the one that is optimal.
- SAS has a very unified syntax; R does not. Overall, there is a huge
  strength for R, but for someone new, it can be a nightmare.
- There are methodological differences between the languages, even for
  commonly used methods. Also, defaults between the two can be different
  and produce different results.
- SAS has copious amounts of generally good documentation. R often has
  fantastic documentation, but more often than not, it is lacking or
  poorly written for users.
- R revels in lazy computations; it often gives you a minimal amount of
  returned values and printed output, and you can ask for more. SAS
  tends to dump everything on you, even if you didn’t really need it.

Most of these issues *are not technical problems*. segue is meant to be
a way for SAS folks to use R in a way that meets them in the middle. It
repackages existing R functions such that:

- They are **discoverable**. Naming is hard but important, and segue has
  a taxonomy built into its names that should make things obvious.
- The computations for each function are **well documented** so that the
  user can understand what they will get. We can explicitly show the
  underlying R code and its SAS analog.
- **Compatibility** is also documented. If there are numerical
  differences between SAS and R implementations of the same thing, how
  large are they?
- **Defaults** are biased towards those from SAS. In almost any case,
  there is no uniformly “right” choice, but we can make the transition
  easier with judicious choices.
- Errors are more **informative** than what base R would produce.
- In some cases, the R API isn’t great (e.g., `binom.test()`) and the
  segue functions have a different interface.

## Installation

You can install segue from CRAN with:

``` r
install.packages("segue")
```

You can install the development version of segue like so:

``` r
install.packages("pak")
pak::pak("posit-dev/segue")
```

## Example

`bin_prop_test()` tests a single binomial proportion using a two-level
factor. Here, we test whether the proportion of patients with cognitive
impairment is 0.5:

``` r
library(segue)

cog_ability <- modeldata::ad_data$Class
table(cog_ability)
#> cog_ability
#> Impaired  Control 
#>       91      242

test_res <- bin_prop_test(cog_ability)
test_res
#> Testing a Single Binomial Proportion via exact Test
#> No missing values
```

The results can be tidied into a tibble:

``` r
tidy(test_res)
#> # A tibble: 1 × 8
#>   estimate statistic  p.value parameter conf.low conf.high method    alternative
#>      <dbl>     <dbl>    <dbl>     <dbl>    <dbl>     <dbl> <chr>     <chr>      
#> 1    0.273        91 5.86e-17       333    0.226     0.325 Exact bi… two.sided
```

`translate()` shows the R code that was used, along with the analogous
SAS code:

``` r
translate(test_res)
#> ── R Code template ─────────────────────────────────────────────────────────────
#> stats::binom.test(x = 91L, n = 333L, alternative = "two.sided", 
#>     p = 0.5)
#> 
#> ── SAS Code template ───────────────────────────────────────────────────────────
#> No known SAS equivalent
```

## Code of Conduct

Please note that the segue project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
