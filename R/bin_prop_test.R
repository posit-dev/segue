#' Test a Single Binomial Proportion
#'
#' Conducts a basic hypothesis test for a single Binomial proportion. This is
#' a wrapper around functions such as [binom.test()] but takes a factor vector
#' with two levels.
#'
#' @inheritParams stats::binom.test
#' @param y A factor vector with two levels.
#' @param method A single character string with possible values:
#'   - `"exact"`: An exact test via [binom.test()].
#'   - `"score"`: An asymptotic score test via [prop.test()].
#'   - `"midp"`: An exact test geared for small sample sizes via
#'               [exactci::binom.exact()]
#'
#' @param level A single character value for the level of the factor that
#' corresponds to the event of interest. The default is the first factor level.
#' @param ... Not currently used.
#' @return An object with classes `"bin_prop_test"` and `"segue_object"` with
#' elements
#'   - `calls`: contains code in R and SAS for [translate()].
#'   - `results`: the results,
#'   - `data`: characteristics of the data.
#'   - `options`: records options that were used by the function.
#' @examples
#' \donttest{
#' if (rlang::is_installed(c("modeldata"))) {
#'   cog_ability <- modeldata::ad_data$Class
#'   str(cog_ability)
#'   test_res <- bin_prop_test(cog_ability)
#'   test_res
#' }
#' }
#' @seealso [binom.test()]
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
  level = NULL,
  p = 0.5,
  alternative = "two.sided",
  ...
) {
  check_factor(y)
  level <- check_level(level, y)
  method <- rlang::arg_match(method, c("exact", "score", "midp"))
  alternative <- rlang::arg_match(
    alternative,
    c("two.sided", "less", "greater")
  )
  rlang::check_number_decimal(p, min = 0.0, max = 1.0, allow_infinite = FALSE)
  check_empty_ellipse(...)

  # ----------------------------------------------------------------------------
  # Spit out options

  opts <- list(method = method, level = level, alternative = alternative, p = p)
  call_opts <- opts[!names(opts) %in% c("method", "level")]

  # ----------------------------------------------------------------------------
  # Account for missing data

  is_na <- is.na(y)
  num_missing <- sum(is_na)

  compl_y <- y[!is_na]
  num_events <- sum(compl_y == level)
  num_total <- length(compl_y)

  # ----------------------------------------------------------------------------
  # SAS translation

  # TODO kind of a mess; figure out a better way of doing this; a method of
  # some sort to make these.
  sas_code <-
    c(
      "proc freq data=df;",
      "  table outcome_column / binomialOPTIONS;",
      "  exact binomial;",
      "run;"
    )

  has_p <- !identical(p, 0.5)
  has_level <- !identical(level, levels(y)[1])
  has_alt <- !identical(alternative, "two.sided")
  sas_opt <- ""
  if (has_p | has_level) {
    sas_opt <- character(0)
    if (has_p) {
      sas_opt <- c(sas_opt, paste0("p=", p))
    }
    if (has_level) {
      sas_opt <- c(sas_opt, paste0("level='", level, "'"))
    }
    sas_opt <- paste0(sas_opt, collapse = " ")
    sas_opt <- paste0("(", sas_opt, ")")
  }
  sas_code <- gsub("OPTIONS", sas_opt, sas_code)

  # ----------------------------------------------------------------------------

  if (method == "exact") {
    orig_res <- bin_prop_test_exact(num_events, num_total, call_opts)
  } else if (method == "score") {
    orig_res <- bin_prop_test_score(num_events, num_total, call_opts)
  }else if (method == "midp") {
    orig_res <- bin_prop_test_midp(num_events, num_total, call_opts)
  }

  res <- new_segue(
    calls = list(r = orig_res$call, sas = sas_code),
    options = opts,
    results = orig_res$results,
    data = list(
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
  cli::cli_inform("Testing a Single Binomial Proportion via {x$options$method} Test")
  cat("\n")
  # cli::options("Options:")
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
    cli::cli_abort("{.fn bin_prop_test} with {.code method = 'exact'}
                   failed with error: {res}", call = NULL)
  }

  list(results = res, call = cl)
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
    cli::cli_abort("{.fn bin_prop_test} with {.code method = 'midp'}
                   failed with error: {res}", call = NULL)
  }

  list(results = res, call = cl)
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
    cli::cli_abort("{.fn bin_prop_test} with {.code method = 'score'}
                   failed with error: {res}", call = NULL)
  }

  list(results = res, call = cl)
}
