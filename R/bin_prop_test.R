#' Test a Single Binomial Proportion
#'
#' Conducts a basic hypothesis test for a single Binomial proportion. This is
#' a wrapper around functions such as [binom.test()] but takes a factor vector
#' with two levels.
#'
#' @family Binomial Outcomes
#' @param y A factor vector with two levels.
#' @param method A single character string with possible values:
#'   - `"exact"`: An exact test via [binom.test()].
#'   - `"score"`: An asymptotic score test via [prop.test()].
#'   - `"midp"`: An exact test geared for small sample sizes via
#'               [exactci::binom.exact()]
#'
#' @param event_level A single character value for the level of the factor that
#' corresponds to the event of interest. The default is the first factor level.
#' @param alternative A character value indicating the alternative hypothesis
#' and must be one of `"two_sided"`, `"greater"`, or `"lesser"`. See
#' [convert_alternative()] for the translation to the values used by
#' [stats::binom.test()].
#' @param null_value A single number in `[0, 1]` for the proportion under the
#' null hypothesis.
#' @param ... Not currently used.
#'
#' @details
#'
#' This function performs one of several Frequentist hypothesis tests on a
#' single Binomial proportion.
#'
#' @includeRmd man/_rmd/bin_prop_test.Rmd
#'
#'
#' @return An object with classes `"bin_prop_test"` and `"segue_object"` with
#' elements
#'   - `code`: contains code in R and SAS for [translate()].
#'   - `results`: the results,
#'   - `data_characteristics`: characteristics of the data.
#'   - `options`: records options that were used by the function.
#' @examples
#' \donttest{
#' if (rlang::is_installed(c("modeldata"))) {
#'   cog_ability <- modeldata::ad_data$Class
#'   str(cog_ability)
#'   test_res <- bin_prop_test(cog_ability)
#'   test_res
#'
#'   # Can be tidied to obtain a tibble of results:
#'   tidy(test_res)
#' }
#' }
#' @seealso [binom.test()], [prop.test()], [exactci::binom.exact()],
#' [broom::tidy.htest()]
#' @export
bin_prop_test <- function(y, ...) {
  UseMethod("bin_prop_test")
}

#' @rdname bin_prop_test
#' @export
#' @export bin_prop_test.default
bin_prop_test.default <- function(y, ...) {
  cli::cli_abort(
    "There is no known method for {.fn bin_prop_test} for {.obj_type_friendly {y}}."
  )
}

#' @rdname bin_prop_test
#' @export
#' @export bin_prop_test.factor
bin_prop_test.factor <- function(
  y,
  method = c("exact", "score", "midp"),
  event_level = NULL,
  null_value = 0.5,
  alternative = "two_sided",
  ...
) {
  check_factor(y)
  event_level <- check_level(event_level, y)
  method <- rlang::arg_match(method, c("exact", "score", "midp"))
  stats_alternative <- convert_alternative(alternative)
  rlang::check_number_decimal(
    null_value,   min = 0.0,
    max = 1.0,
    allow_infinite = FALSE
  )
  rlang::check_dots_empty()

  # ----------------------------------------------------------------------------
  # Spit out options

  opts <- list(
    method = method,
    event_level = event_level,
    alternative = alternative,
    null_value = null_value
  )
  # Argument names and values used by the underlying stats functions
  call_opts <- list(alternative = stats_alternative, p = null_value)

  # ----------------------------------------------------------------------------
  # Account for missing data

  is_na <- is.na(y)
  num_missing <- sum(is_na)

  compl_y <- y[!is_na]
  num_events <- sum(compl_y == event_level)
  num_total <- length(compl_y)

  # ----------------------------------------------------------------------------

  orig_res <- switch(
    method,
    exact = bin_prop_test_exact(num_events, num_total, call_opts),
    score = bin_prop_test_score(num_events, num_total, call_opts),
    midp = bin_prop_test_midp(num_events, num_total, call_opts)
  )

  res <- new_segue(
    code = orig_res$code,
    options = opts,
    results = orig_res$results,
    data_characteristics = list(
      missing = num_missing,
      events = num_events,
      total = num_total
    ),
    classes = "bin_prop_test"
  )
  res
}

#' @export
print.bin_prop_test <- function(x, ...) {
  cli::cli_inform(
    "Testing a Single Binomial Proportion via {x$options$method} Test"
  )
  report_missing_simple(x)
  invisible(x)
}

# ------------------------------------------------------------------------------
# Specific computational methods

bin_prop_test_exact <- function(num_events, num_total, options = list()) {
  cl <- rlang::call2(
    "binom.test",
    .ns = "stats",
    x = num_events,
    n = num_total,
    !!!options
  )
  res <- try(rlang::eval_tidy(cl), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_abort(
      "{.fn bin_prop_test} with {.code method = 'exact'}
                   failed with error: {res}",
      call = NULL
    )
  }

  sas_code <-
    "
proc freq data = {data set};
  table {data column} / binomial;
  exact binomial;
run;
"

  list(results = res, code = list(r = cl, sas = sas_code))
}

bin_prop_test_midp <- function(num_events, num_total, options = list()) {
  rlang::check_installed("exactci")
  cl <- rlang::call2(
    "binom.exact",
    .ns = "exactci",
    x = num_events,
    n = num_total,
    !!!options,
    tsmethod = "central"
  )
  res <- try(rlang::eval_tidy(cl), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_abort(
      "{.fn bin_prop_test} with {.code method = 'midp'}
                   failed with error: {res}",
      call = NULL
    )
  }

  sas_code <-
    "
proc freq data = {data set};
  table {data column} / binomial;
  exact binomial / midp;
run;
"

  list(results = res, code = list(r = cl, sas = sas_code))
}


bin_prop_test_score <- function(num_events, num_total, options = list()) {
  cl <- rlang::call2(
    "prop.test",
    .ns = "stats",
    x = num_events,
    n = num_total,
    !!!options,
    correct = FALSE
  )
  res <- try(rlang::eval_tidy(cl), silent = TRUE)
  if (inherits(res, "try-error")) {
    cli::cli_abort(
      "{.fn bin_prop_test} with {.code method = 'score'}
                   failed with error: {res}",
      call = NULL
    )
  }

  sas_code <-
    "
proc freq data = {data set};
  tables {data column} / binomial(cl=score);
run;
"

  list(results = res, code = list(r = cl, sas = sas_code))
}
