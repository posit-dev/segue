#' Exact binomial test, computed as SAS `Proc Freq` does
#'
#' @description
#' `exact_binom_test_sas()` takes similar arguments as
#' [stats::binom.test()] but returns the p-values that SAS `Proc Freq` reports for
#'
#' ```
#' proc freq;
#'   tables resp / binomial(p = <p> level = "<event>");
#'   exact binomial;
#' run;
#' ```
#'
#' [stats::binom.test()] and `Proc Freq` differ in how the two-sided p-value is
#' defined in terms of:
#'
#'  - how one-sided p-values are reported, and
#'  - how the correct tail is computed.
#'
#' Each difference is described below. Only the hypothesis test is computed by
#' this function (i.e., not automatic production of confidence intervals).
#'
#'
#' @details
#'
#' ## Notation
#'
#' Let \eqn{X \sim \text{Binomial}(n, p_0)}{X ~ Binomial(n, p_0)}, where \eqn{p_0} is `p`, and let
#' \eqn{x} be the observed number of successes. The two one-sided p-values are
#' the tails
#'
#' \deqn{
#' p_L = \Pr(X \le x) = \sum_{k=0}^{x} \binom{n}{k} p_0^k (1 - p_0)^{n - k}
#' }{
#' p_L = Pr(X <= x) = sum over k = 0, ..., x of choose(n, k) p_0^k (1 - p_0)^(n - k)
#' }
#'
#' \deqn{
#' p_R = \Pr(X \ge x) = \sum_{k=x}^{n} \binom{n}{k} p_0^k (1 - p_0)^{n - k}
#' }{
#' p_R = Pr(X >= x) = sum over k = x, ..., n of choose(n, k) p_0^k (1 - p_0)^(n - k)
#' }
#'
#' ## One-sided p-values
#'
#' `Proc Freq` computes both tails but reports only the smaller one:
#'
#' - When \eqn{p_L \le p_R}, the left tail is reported (`XPL_BIN`) and the
#'   right tail (`XPR_BIN`) is missing.
#' - Otherwise, the right tail is reported and the left tail is missing.
#'
#' `exact_binom_test_sas()` does the same. With `alternative = "less"`,
#' `p.value` is \eqn{p_L} when it is the smaller tail and `NA` otherwise. With
#' `alternative = "greater"`, `p.value` is \eqn{p_R} when it is the smaller
#' tail and `NA` otherwise. Since \eqn{p_L + p_R = 1 + \Pr(X = x) > 1}{p_L + p_R = 1 + Pr(X = x) > 1}, the
#' smaller tail is the one on the same side of \eqn{n p_0} as \eqn{x}, apart
#' from ties (see "Precision").
#'
#' ## Two-sided p-value
#'
#' The two functions define the two-sided p-value differently. In the terms of
#' Fay (2010), SAS `Proc Freq` uses the "central" method: twice the smaller tail,
#' capped at 1.  This is also  called "doubling" or "TST" by Hirji (2006).
#'
#' For example, with \eqn{n = 10}, \eqn{x = 5}, and \eqn{p_0 = 0.25}, the right
#' tail is \eqn{p_R = 0.0781269} and is the smaller tail, SAS computes:
#'
#' \preformatted{  p_2 = 2 * 0.0781269 = 0.1562538}
#'
#' ## Precision
#'
#' `Proc Freq` computes the right tail as one minus the left tail at \eqn{x - 1}:
#'
#' \deqn{
#' p_R = 1 - \Pr(X \le x - 1)
#' }{
#' p_R = 1 - Pr(X <= x - 1)
#' }
#'
#' When \eqn{p_R} is small, \eqn{\Pr(X \le x - 1)}{Pr(X <= x - 1)} is close to 1 and the
#' subtraction loses relative precision. For example, with \eqn{n = 6},
#' \eqn{x = 6}, and \eqn{p_0 = 0.01}, \eqn{p_R = 10^{-12}}{p_R = 1e-12}, and
#' SAS's value is correct to only about four significant digits instead of the
#' usual 15 or 16. Right tails below about
#' \eqn{10^{-16}}{1e-16} come out as exactly 0.
#'
#' `exact_binom_test_sas()` uses the same subtraction,
#' `1 - pbinom(x - 1, n, p)`, so that it returns these values too.
#'
#' The results are not identical to `Proc Freq`'s to the last bit. R's
#' [stats::pbinom()] and `Proc Freq` use different algorithms for the binomial
#' distribution function, and their results usually differ by a few dozen units in
#' the last place (at most a few hundred). This is random rounding error
#' in both, not a bias in either. Over 11,466 combinations of
#' \eqn{1 \le x \le n \le 50} and \eqn{p_0 \in \{0.01, 0.25, 0.5\}}{p_0 = 0.01, 0.25, or 0.5}, the
#' p-values from `exact_binom_test_sas()` are within
#' \eqn{5.5 \times 10^{-14}}{5.5e-14} (absolute) of `Proc Freq`'s. Because of the
#' cancellation, the agreement for small right tails is only absolute: a right
#' tail that is 0 in one can be about \eqn{10^{-16}}{1e-16} in the other.
#'
#' ### Ties
#'
#' When \eqn{p_0 = 0.5} and \eqn{x = n / 2}, the two tails are exactly equal,
#' and which tail is reported depends on rounding error. `Proc Freq` reported the
#' right tail for some even \eqn{n} and the left tail for others, and
#' `exact_binom_test_sas()` can choose differently. For the 25 ties with
#' \eqn{n \le 50}{n <= 50}, `Proc Freq` reported the right tail in 14, so the
#' `NA` is on the other alternative in those cases. The reported values differ
#' from `Proc Freq`'s by at most \eqn{1.2 \times 10^{-14}}{1.2e-14}, as in the
#' cases without ties. When the computed tails are equal, the left tail is
#' reported. The two-sided p-value is 1 either way.
#'
#' @param x A single whole number for the number of successes, or a vector of
#'   two whole numbers for the numbers of successes and failures, as in
#'   [stats::binom.test()].
#' @param n A single whole number for the number of trials. Ignored when `x`
#'   has two values.
#' @param p A single number in `[0, 1]` for the probability of success under
#'   the null hypothesis.
#' @param alternative A single string for the alternative hypothesis:
#'   `"two.sided"` (the default), `"less"`, or `"greater"`.
#'
#' @returns An object of class `htest` with the same elements as the result of
#'   [stats::binom.test()], except `conf.int`:
#'
#'   - `statistic`: the number of successes.
#'   - `parameter`: the number of trials.
#'   - `p.value`: the p-value, as `Proc Freq` reports it. It is `NA` for a
#'     one-sided alternative when the requested tail is not the smaller one.
#'   - `estimate`: the sample proportion, `x / n`.
#'   - `null.value`: `p`.
#'   - `alternative`, `method`, and `data.name`: strings that describe the
#'     test.
#'
#' @references
#' SAS Institute Inc. "Binomial Proportion." _SAS/STAT User's Guide: The FREQ
#' Procedure_.
#'
#' Fay, M. P. (2010). Two-sided exact tests and matching confidence intervals
#' for discrete data. _The R Journal_, 2(1), 53-58.
#'
#' Hirji, K. F. (2006). _Exact Analysis of Discrete Data_. Chapman and
#' Hall/CRC.
#'
#' Blaker, H. (2000). Confidence curves and improved exact confidence
#' intervals for discrete distributions. _Canadian Journal of Statistics_,
#' 28(4), 783-798.
#'
#' @seealso [stats::binom.test()]
#'
#' @examples
#' # The two-sided p-values differ by definition
#' exact_binom_test_sas(5, 10, p = 0.25)
#' binom.test(5, 10, p = 0.25)$p.value
#'
#' # Only the smaller tail is reported
#' exact_binom_test_sas(5, 10, p = 0.25, alternative = "greater")$p.value
#' exact_binom_test_sas(5, 10, p = 0.25, alternative = "less")$p.value
#'
#' # A small right tail loses precision, as in SAS
#' exact_binom_test_sas(6, 6, p = 0.01, alternative = "greater")$p.value
#' binom.test(6, 6, p = 0.01, alternative = "greater")$p.value
#' @keywords internal
#' @export
exact_binom_test_sas <- function(
  x,
  n,
  p = 0.5,
  alternative = c("two.sided", "less", "greater")
) {
  data_name <- deparse1(substitute(x))
  if (!is.numeric(x) || anyNA(x) || any(x < 0) || any(x != round(x))) {
    cli::cli_abort("{.arg x} must contain non-negative whole numbers.")
  }
  if (length(x) == 2) {
    n <- sum(x)
    x <- x[1]
  } else if (length(x) == 1) {
    check_number_whole(n, min = 1)
    if (x > n) {
      cli::cli_abort(
        "{.arg x} must be no larger than {.arg n} ({n}), not {x}."
      )
    }
    data_name <- paste(data_name, "and", deparse1(substitute(n)))
  } else {
    cli::cli_abort("{.arg x} must have one or two values, not {length(x)}.")
  }
  check_number_decimal(p, min = 0, max = 1)
  alternative <- arg_match(alternative)

  tails <- sas_binom_tails(x, n, p)
  p_value <- switch(
    alternative,
    two.sided = tails$two_sided,
    less = tails$less,
    greater = tails$greater
  )

  estimate <- x / n
  names(x) <- "number of successes"
  names(n) <- "number of trials"
  names(estimate) <- names(p) <- "probability of success"

  structure(
    list(
      statistic = x,
      parameter = n,
      p.value = p_value,
      estimate = estimate,
      null.value = p,
      alternative = alternative,
      method = "Exact binomial test (SAS Proc Freq)",
      data.name = data_name
    ),
    class = "htest"
  )
}

# Both one-sided tails and the central two-sided p-value. `less` and `greater`
# are NA for the tail that Proc Freq does not report. Vectorized over x, n,
# and p.
sas_binom_tails <- function(x, n, p) {
  left <- stats::pbinom(x, n, p)
  # Proc Freq gets the right tail by subtraction, so small right tails lose
  # precision. This is done on purpose to return the same values as SAS.
  right <- 1 - stats::pbinom(x - 1, n, p)

  is_left <- left <= right
  one_sided <- right
  one_sided[is_left] <- left[is_left]

  less <- left
  less[!is_left] <- NA_real_
  greater <- right
  greater[is_left] <- NA_real_

  list(
    less = less,
    greater = greater,
    two_sided = pmin(1, 2 * one_sided)
  )
}
