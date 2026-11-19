#' Rank-Based Tests for Differences Between Groups
#'
#' Tests whether the distribution of a numeric outcome differs between groups
#' using rank-based statistics such as the Wilcoxon test.
#'
#' @family Numeric Outcomes
#' @param outcome One numeric column of `data` for the outcome. Uses
#'  <[`tidy-select`][tidyselect::language]> to specify the value.
#' @param group One factor or character column of `data` for the groups. Uses
#'  <[`tidy-select`][tidyselect::language]> to specify the value.
#' @param data A data frame.
#' @param ... Not currently used.
#' @param strata One factor or character column of `data` for the strata. Uses
#'  <[`tidy-select`][tidyselect::language]> to specify the value. Use only with
#'  `method = "wilcoxon_stratified"`.
#' @param method A single character string with possible values:
#'   - `"wilcoxon"`: The Wilcoxon rank-sum test via [coin::wilcox_test()].
#'     The `group` argument must have two levels.
#'   - `"wilcoxon_stratified"`: The stratified Wilcoxon rank-sum test of van
#'     Elteren via [coin::independence_test()]. The `group` argument must have
#'     two levels and `strata` is required.
#'   - `"kruskal_wallis"`: The Kruskal-Wallis test via [coin::kruskal_test()].
#'     The `group` argument must have two or more levels.
#' @param alternative A character value indicating the alternative hypothesis
#' and must be one of `"two_sided"`, `"greater"`, or `"lesser"`. The
#' Kruskal-Wallis test allows only `"two_sided"`.
#' @param distribution A single character string for the null distribution of
#' the test statistic: `"asymptotic"`, `"exact"`, or `"approximate"` (Monte
#' Carlo resampling). The Kruskal-Wallis test does not allow `"exact"`.
#' @param reference_choice A single character string for the group whose rank
#' sum gives the Wilcoxon statistic, which sets the sign of \eqn{Z} and the
#' direction of `alternative`. See Details below. Used only with
#' `method = "wilcoxon"`.
#'
#' @details
#'
#' Rows with a missing value in the outcome, group, or strata column are
#' removed before the analysis, as \pkg{SAS} does for the analysis variables.
#' In \pkg{SAS}, the order of the group levels follows the `ORDER=` option
#' (formatted values by default). In \pkg{R}, it follows the order of the
#' factor levels.
#'
#' ## Wilcoxon rank-sum test
#'
#' This hypothesis test, also known as the Mann–Whitney \eqn{U} test, is a
#' nonparametric, rank-based statistical test. It is used to determine whether
#' two independent samples come from populations with the same distribution
#' (or median).
#'
#' Using `method = "wilcoxon"` compares two groups with the sum of the ranks
#' in one group. In \pkg{SAS}, this is achieved by using `Proc Npar1way`
#' with the `wilcoxon` option.
#'
#' When assessing whether the \pkg{R} implementation used here is consistent
#' with \pkg{SAS}, there are a few aspects to consider:
#'
#' - **Reference group**: The \eqn{Z} statistic comes from the rank sum of one
#'   group, which sets the sign of \eqn{Z} and the meaning of a one-sided
#'   `alternative`. \pkg{SAS} uses the smaller group, or its first class level
#'   when the groups are the same size. [coin::wilcox_test()] uses the first
#'   factor level. The `reference_choice` argument selects either one. When the
#'   groups are the same size, `"smaller"` uses the first factor level. The
#'   \pkg{R} code from [translate()] assumes that the reference group is the
#'   first factor level.
#' - **Continuity correction**: \pkg{SAS} applies a correction of 0.5 to the
#'   asymptotic test by default. \pkg{coin} has no correction. [tidy()]
#'   applies the correction to the \pkg{coin} results, so the object in
#'   `results` is not corrected. Exact and Monte Carlo p-values have no
#'   correction.
#' - **One-sided correction**: The correction moves \eqn{Z} toward the null
#'   mean for all alternatives.
#' - **Statistic**: \pkg{SAS} reports the rank sum \eqn{S}, \eqn{Z}, and a
#'   \eqn{t} approximation. This function reports \eqn{Z}.
#' - **Ties**: \pkg{SAS} and \pkg{coin} both use mid-ranks and the variance
#'   adjusted for ties.
#' - **Null distribution**: \pkg{SAS}, \pkg{coin}, and this function use the
#'   asymptotic distribution by default. `distribution = "exact"` and
#'   `"approximate"` match the \pkg{SAS} statements `exact wilcoxon;` and
#'   `exact wilcoxon / mc;`.
#' - **Alternative**: \pkg{SAS} has no option for the alternative.
#'   `numeric_rank_test()` reports both the one-sided and the two-sided p-values.
#'
#' ## Stratified Wilcoxon rank-sum test
#'
#' `method = "wilcoxon_stratified"` combines Wilcoxon tests across strata
#' (also known as van Elteren's test). The scores are the ranks within each
#' stratum divided by the stratum size plus one. In \pkg{SAS}, this is
#' `Proc Freq` with `cmh2 scores=modridit`.
#'
#' Some important aspects:
#'
#' - **Scores**: The stratified version of [coin::wilcox_test()] uses the ranks
#'   within each stratum without weights (in \pkg{SAS}, `scores=rank`), which
#'   is not van Elteren's test. This function uses [coin::independence_test()]
#'   with van Elteren's scores instead.
#' - **Statistic**: \pkg{SAS} reports a chi-square statistic (\eqn{Z^2}) with
#'   one degree of freedom, which is two-sided only. `numeric_rank_test()`
#'   reports \eqn{Z} from \pkg{coin}, with the first factor level as the
#'   reference group.
#' - **Null distribution**: \pkg{SAS} has only the asymptotic test. The exact
#'   and Monte Carlo versions from \pkg{coin} have no \pkg{SAS} code.
#'
#' ## Kruskal-Wallis test
#'
#' `method = "kruskal_wallis"` compares two or more groups with the mean rank
#' in each group. In \pkg{SAS}, this is `Proc Npar1way` with the `wilcoxon`
#' option.
#'
#' Note:
#'
#' - **Statistic**: \pkg{SAS} and \pkg{coin} give the same chi-square
#'   statistic, adjusted for ties, with one degree of freedom less than the
#'   number of groups.
#' - **Null distribution**: \pkg{SAS} has asymptotic, exact, and Monte Carlo
#'   versions. \pkg{coin} has no exact version, so `distribution = "exact"` is
#'   an error.
#' - **Alternative**: The test is two-sided only.
#'
#' @return A [segue object][segue_object] with classes `"numeric_rank_test"`
#' and `"segue_object"`. `results` is the object from \pkg{coin}, without the
#' continuity correction. `data_characteristics` has these elements:
#'   - `missing`: The number of rows removed because of missing values.
#'   - `total`: The number of rows used in the analysis.
#'   - `groups`: The number of group levels.
#'   - `strata`: The number of strata, or 1 when there are none.
#'   - `reference`: The reference group when `method = "wilcoxon"`, or `NULL`
#'     for the other methods.
#' @examplesIf rlang::is_installed(c("coin", "modeldata"))
#' ad_data <- modeldata::ad_data
#' res <- numeric_rank_test(tau, group = Class, data = ad_data)
#' res
#'
#' # Can be tidied to obtain a tibble of results:
#' tidy(res)
#'
#' # Show the equivalent R and SAS code:
#' translate(res)
#'
#' numeric_rank_test(
#'   tau,
#'   group = Class,
#'   data = ad_data,
#'   strata = Genotype,
#'   method = "wilcoxon_stratified"
#' ) |>
#'   tidy()
#'
#' numeric_rank_test(
#'   tau,
#'   group = Genotype,
#'   data = ad_data,
#'   method = "kruskal_wallis"
#' ) |>
#'   tidy()
#' @seealso [coin::wilcox_test()], [coin::independence_test()],
#' [coin::kruskal_test()]
#' @references
#' Hollander, M., Wolfe, D. A., and Chicken, E. 2014. _Nonparametric
#' Statistical Methods_. 3rd ed. John Wiley and Sons Inc., Hoboken.
#'
#' Hothorn, T., Hornik, K., van de Wiel, M. A., and Zeileis, A. 2006. "A Lego
#' System for Conditional Inference." _The American Statistician_ 60 (3):
#' 257–263. \doi{10.1198/000313006X118430}.
#'
#' Kruskal, W. H., and Wallis, W. A. 1952. "Use of Ranks in One-Criterion
#' Variance Analysis." _Journal of the American Statistical Association_ 47
#' (260): 583–621. \doi{10.1080/01621459.1952.10483441}.
#'
#' SAS Institute Inc. 2025. "The NPAR1WAY Procedure." In _SAS/STAT 15.4 User's
#' Guide_. SAS Institute Inc., Cary, NC.
#'
#' van Elteren, P. H. 1960. "On the Combination of Independent Two-Sample
#' Tests of Wilcoxon." _Bulletin of the International Statistical Institute_
#' 37: 351–361.
#' @export
numeric_rank_test <- function(
  outcome,
  group,
  data,
  ...,
  strata = NULL,
  method = c("wilcoxon", "wilcoxon_stratified", "kruskal_wallis"),
  alternative = "two_sided",
  distribution = c("asymptotic", "exact", "approximate"),
  reference_choice = c("smaller", "first")
) {
  rlang::check_installed("coin")
  if (!is.data.frame(data)) {
    rlang::stop_input_type(data, "a data frame")
  }
  method <- rlang::arg_match(
    method,
    c("wilcoxon", "wilcoxon_stratified", "kruskal_wallis")
  )
  stats_alternative <- convert_alternative(alternative)
  distribution <- rlang::arg_match(
    distribution,
    c("asymptotic", "exact", "approximate")
  )
  reference_given <- !missing(reference_choice)
  reference_choice <- rlang::arg_match(reference_choice, c("smaller", "first"))

  outcome_col <- select_single_column(rlang::enquo(outcome), data, "outcome")
  group_col <- select_single_column(rlang::enquo(group), data, "group")
  strata_quo <- rlang::enquo(strata)
  has_strata <- !rlang::quo_is_null(strata_quo)
  strata_col <- NULL
  if (has_strata) {
    strata_col <- select_single_column(strata_quo, data, "strata")
  }

  if (!is.numeric(data[[outcome_col]])) {
    cli::cli_abort(
      "The {.arg outcome} column {.field {outcome_col}} must be numeric, not
       {.obj_type_friendly {data[[outcome_col]]}}."
    )
  }
  check_grouping_column(data[[group_col]], group_col, "group")
  if (has_strata) {
    check_grouping_column(data[[strata_col]], strata_col, "strata")
  }

  if (method == "wilcoxon_stratified" && !has_strata) {
    cli::cli_abort(
      "{.code method = 'wilcoxon_stratified'} requires a {.arg strata} column."
    )
  }
  if (method != "wilcoxon_stratified" && has_strata) {
    cli::cli_abort(
      "{.arg strata} can only be used with
       {.code method = 'wilcoxon_stratified'}, not {.code method = '{method}'}."
    )
  }
  if (method == "kruskal_wallis" && alternative != "two_sided") {
    cli::cli_abort(
      "{.code method = 'kruskal_wallis'} requires
       {.code alternative = 'two_sided'}, not {.val {alternative}}."
    )
  }
  if (method == "kruskal_wallis" && distribution == "exact") {
    cli::cli_abort(
      c(
        "{.code method = 'kruskal_wallis'} does not support
         {.code distribution = 'exact'}.",
        "i" = "Use {.code distribution = 'approximate'} instead."
      )
    )
  }
  if (method != "wilcoxon" && reference_given) {
    cli::cli_abort(
      "{.arg reference_choice} can only be used with
       {.code method = 'wilcoxon'}, not {.code method = '{method}'}."
    )
  }
  rlang::check_dots_empty()

  # ----------------------------------------------------------------------------
  # Spit out options

  opts <- list(
    method = method,
    outcome = outcome_col,
    group = group_col,
    strata = strata_col,
    alternative = alternative,
    distribution = distribution,
    reference_choice = reference_choice
  )
  # Argument names and values used by the coin functions
  call_opts <- list(
    alternative = stats_alternative,
    distribution = distribution
  )

  # ----------------------------------------------------------------------------
  # Account for missing data (complete cases over the columns used)

  analysis_cols <- c(outcome_col, group_col, strata_col)
  analysis_data <- as.data.frame(data)[, analysis_cols, drop = FALSE]
  is_complete <- stats::complete.cases(analysis_data)
  num_missing <- sum(!is_complete)
  analysis_data <- analysis_data[is_complete, , drop = FALSE]
  rownames(analysis_data) <- NULL

  analysis_data[[group_col]] <- droplevels(factor(analysis_data[[group_col]]))
  num_groups <- nlevels(analysis_data[[group_col]])
  num_strata <- 1L
  if (has_strata) {
    analysis_data[[strata_col]] <- droplevels(factor(analysis_data[[
      strata_col
    ]]))
    num_strata <- nlevels(analysis_data[[strata_col]])
  }
  check_group_levels(num_groups, method, group_col)

  # coin computes the Wilcoxon statistic from the first level, so put the
  # reference group first.
  reference_level <- NULL
  if (method == "wilcoxon") {
    analysis_data[[group_col]] <- order_reference_level(
      analysis_data[[group_col]],
      reference_choice
    )
    reference_level <- levels(analysis_data[[group_col]])[1]
  }

  # ----------------------------------------------------------------------------

  orig_res <- switch(
    method,
    wilcoxon = numeric_rank_test_wilcoxon(analysis_data, call_opts, opts),
    wilcoxon_stratified = numeric_rank_test_wilcoxon_stratified(
      analysis_data,
      call_opts,
      opts
    ),
    kruskal_wallis = numeric_rank_test_kruskal_wallis(
      analysis_data,
      call_opts,
      opts
    )
  )

  res <- new_segue(
    code = orig_res$code,
    options = opts,
    results = orig_res$results,
    data_characteristics = list(
      missing = num_missing,
      total = nrow(analysis_data),
      groups = num_groups,
      strata = num_strata,
      reference = reference_level
    ),
    classes = "numeric_rank_test"
  )
  res
}

#' @export
print.numeric_rank_test <- function(x, ...) {
  test_label <- rank_test_labels[[x$options$method]]
  cli::cli_inform(
    "Testing for differences in {x$options$outcome} between
     {x$options$group} levels via the {test_label} test"
  )
  report_missing_simple(x)
  invisible(x)
}

#' @export
tidy.numeric_rank_test <- function(x, ...) {
  rlang::check_dots_empty()
  rlang::check_installed("coin")
  method <- x$options$method
  statistic <- as.numeric(coin::statistic(x$results))
  p_value <- as.numeric(coin::pvalue(x$results))

  # SAS applies a continuity correction to the asymptotic Wilcoxon test by
  # default; coin does not.
  if (method == "wilcoxon" && x$options$distribution == "asymptotic") {
    statistic <- sas_corrected_z(x$results)
    p_value <- normal_p_value(statistic, x$options$alternative)
  }

  res <- tibble::tibble(
    statistic = statistic,
    p.value = p_value,
    method = rank_test_labels[[method]],
    alternative = x$options$alternative
  )
  if (method == "kruskal_wallis") {
    res$parameter <- x$data_characteristics$groups - 1L
    res <- res[, c(
      "statistic",
      "parameter",
      "p.value",
      "method",
      "alternative"
    )]
  }
  res
}

# ------------------------------------------------------------------------------
# Helpers

rank_test_labels <- c(
  wilcoxon = "Wilcoxon rank-sum",
  wilcoxon_stratified = "stratified Wilcoxon rank-sum (van Elteren)",
  kruskal_wallis = "Kruskal-Wallis"
)

# `exact` statements for PROC NPAR1WAY, by `distribution`.
sas_npar1way_exact <- list(
  asymptotic = NULL,
  exact = "wilcoxon",
  approximate = "wilcoxon / mc"
)

check_grouping_column <- function(x, col, arg, call = rlang::caller_env()) {
  if (!is.factor(x) && !is.character(x)) {
    cli::cli_abort(
      "The {.arg {arg}} column {.field {col}} must be a factor or character
       vector, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
  invisible(NULL)
}

check_group_levels <- function(
  num_groups,
  method,
  col,
  call = rlang::caller_env()
) {
  if (method != "kruskal_wallis" && num_groups != 2) {
    cli::cli_abort(
      "{.code method = '{method}'} requires exactly two levels in the
       {.arg group} column {.field {col}} after removing missing values, not
       {num_groups}.",
      call = call
    )
  }
  if (method == "kruskal_wallis" && num_groups < 2) {
    cli::cli_abort(
      "{.code method = 'kruskal_wallis'} requires at least two levels in the
       {.arg group} column {.field {col}} after removing missing values, not
       {num_groups}.",
      call = call
    )
  }
  invisible(NULL)
}

# Put the reference group first. "smaller" keeps the current order when the
# groups are the same size.
order_reference_level <- function(group, reference_choice) {
  counts <- table(group)
  if (reference_choice == "smaller" && counts[[2]] < counts[[1]]) {
    group <- factor(group, levels = rev(levels(group)))
  }
  group
}

# Evaluate a coin call where `data` is the complete-case data. coin evaluates
# the formula in its environment, which needs the search path (for example,
# `stats::model.frame()`), as when a user runs the code from translate().
eval_rank_call <- function(cl, analysis_data) {
  eval_env <- rlang::env(rlang::global_env(), data = analysis_data)
  rlang::eval_bare(cl, eval_env)
}

# The Wilcoxon Z with the continuity correction of 0.5 toward the mean that
# PROC NPAR1WAY uses by default.
# TODO: verify that SAS corrects toward the mean for one-sided alternatives.
sas_corrected_z <- function(x) {
  linear <- as.numeric(coin::statistic(x, type = "linear"))
  centered <- linear - as.numeric(coin::expectation(x))
  (centered - 0.5 * sign(centered)) / sqrt(as.numeric(coin::variance(x)))
}

normal_p_value <- function(z, alternative) {
  switch(
    alternative,
    two_sided = 2 * stats::pnorm(-abs(z)),
    greater = stats::pnorm(z, lower.tail = FALSE),
    lesser = stats::pnorm(z)
  )
}

#' Van Elteren scores for coin
#'
#' Makes a function for the `ytrafo` argument of [coin::independence_test()]
#' that computes van Elteren's scores: the ranks within each stratum divided by
#' the stratum size plus one. With these scores, a stratified
#' [coin::independence_test()] gives van Elteren's test, which
#' [numeric_rank_test()] uses for `method = "wilcoxon_stratified"`. In
#' \pkg{SAS}, these are the `scores=modridit` scores of `Proc Freq`.
#'
#' @param strata A factor with the stratum of each row of the data.
#' @return A function with one argument, the data frame of outcome values that
#'   \pkg{coin} passes to `ytrafo`. It returns the scores as a one-column
#'   matrix.
#' @seealso [numeric_rank_test()]
#' @examplesIf rlang::is_installed("coin")
#' dat <- data.frame(
#'   y = c(1.2, 3.4, 2.2, 5.1, 4.4, 6.0, 8.1, 7.3),
#'   g = factor(rep(c("a", "b"), 4)),
#'   s = factor(rep(c("s1", "s2"), each = 4))
#' )
#' coin::independence_test(
#'   y ~ g | s,
#'   data = dat,
#'   ytrafo = van_elteren_trafo(dat$s)
#' )
#' @keywords internal
#' @export
van_elteren_trafo <- function(strata) {
  rlang::check_installed("coin")
  check_factor(strata, mult_lvls = FALSE)
  function(data) {
    coin::trafo(
      data,
      numeric_trafo = function(y) coin::rank_trafo(y) / (length(y) + 1),
      block = strata
    )
  }
}

# ------------------------------------------------------------------------------
# Specific computational methods

numeric_rank_test_wilcoxon <- function(
  analysis_data,
  options = list(),
  settings = list()
) {
  rank_formula <- rlang::call2(
    "~",
    rlang::sym(settings$outcome),
    rlang::sym(settings$group)
  )
  cl <- rlang::call2(
    "wilcox_test",
    .ns = "coin",
    rank_formula,
    data = quote(data),
    !!!options
  )
  res <- try(eval_rank_call(cl, analysis_data), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_abort(
      "{.fn numeric_rank_test} with {.code method = 'wilcoxon'} failed with
       error: {res}",
      call = NULL
    )
  }

  # SAS has no option for `alternative`: PROC NPAR1WAY reports both one-sided
  # and two-sided p-values. The default continuity correction (correct=yes)
  # is applied by tidy().
  sas_code <- list(
    proc = "npar1way",
    options = "wilcoxon",
    class = "&group",
    var = "&var"
  )
  sas_code$exact <- sas_npar1way_exact[[settings$distribution]]

  list(results = res, code = list(r = cl, sas = sas_code))
}

numeric_rank_test_wilcoxon_stratified <- function(
  analysis_data,
  options = list(),
  settings = list()
) {
  rank_formula <- rlang::call2(
    "~",
    rlang::sym(settings$outcome),
    rlang::call2("|", rlang::sym(settings$group), rlang::sym(settings$strata))
  )
  # coin::wilcox_test() ignores `ytrafo`, so use coin::independence_test()
  # with van Elteren's scores.
  van_elteren_call <- rlang::call2(
    "van_elteren_trafo",
    .ns = "segue",
    rlang::call2("[[", quote(data), settings$strata)
  )
  cl <- rlang::call2(
    "independence_test",
    .ns = "coin",
    rank_formula,
    data = quote(data),
    ytrafo = van_elteren_call,
    !!!options
  )
  res <- try(eval_rank_call(cl, analysis_data), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_abort(
      "{.fn numeric_rank_test} with {.code method = 'wilcoxon_stratified'}
       failed with error: {res}",
      call = NULL
    )
  }

  # PROC FREQ reports the van Elteren chi-square (Z^2) as the "Row Mean Scores
  # Differ" CMH statistic, which is two-sided. It has no option for
  # `alternative` and no exact or Monte Carlo version of this test.
  sas_code <- list()
  if (settings$distribution == "asymptotic") {
    sas_code <- list(
      proc = "freq",
      tables = "&strata*&group*&var / cmh2 scores=modridit noprint"
    )
  }

  list(results = res, code = list(r = cl, sas = sas_code))
}

numeric_rank_test_kruskal_wallis <- function(
  analysis_data,
  options = list(),
  settings = list()
) {
  rank_formula <- rlang::call2(
    "~",
    rlang::sym(settings$outcome),
    rlang::sym(settings$group)
  )
  # The Kruskal-Wallis test has no `alternative`.
  cl <- rlang::call2(
    "kruskal_test",
    .ns = "coin",
    rank_formula,
    data = quote(data),
    distribution = options$distribution
  )
  res <- try(eval_rank_call(cl, analysis_data), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_abort(
      "{.fn numeric_rank_test} with {.code method = 'kruskal_wallis'} failed
       with error: {res}",
      call = NULL
    )
  }

  sas_code <- list(
    proc = "npar1way",
    options = "wilcoxon",
    class = "&group",
    var = "&var"
  )
  sas_code$exact <- sas_npar1way_exact[[settings$distribution]]

  list(results = res, code = list(r = cl, sas = sas_code))
}
