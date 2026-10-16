#' Test a Single Binomial Proportion
#'
#' Conducts a basic hypothesis test for a single Binomial proportion. This is
#' a wrapper around [binom.test()]. Unlike that function, `bin_prop_test()`
#' takes a factor vector with two levels.
#'
#' @inheritParams stats::binom.test
#' @param y A factor with two levels.
#' @param level A single character value for the level of the factor that
#' corresponds to the event of interest.
#' @param ... Not currently used.
#' @return An object with classes `"bin_prop_test"` and `"segue_object"` with
#' elements
#'   - `calls`: contains code in R and SAS for [translate()].
#'   - `results`: the results of [binom.test()],
#'   - `data`: characteristics of the data.
#'   - `options`: records options that were passed to this function.
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
  level = NULL,
  p = 0.5,
  alternative = "two.sided",
  ...
) {
  check_factor(y)
  level <- check_level(level, y)
  alternative <- rlang::arg_match(
    alternative,
    c("two.sided", "less", "greater")
  )
  rlang::check_number_decimal(p, min = 0.0, max = 1.0, allow_infinite = FALSE)
  check_empty_ellipse(...)

  opts <- list(level = level, alternative = alternative, p = p)
  call_opts <- opts[names(opts) != "level"]

  orig_cl <- match.call()
  var_nm <- orig_cl$y

  is_na <- is.na(y)
  num_missing <- sum(is_na)

  compl_y <- y[!is_na]
  num_events <- sum(compl_y == level)
  num_total <- length(compl_y)

  cl <- rlang::call2(
    "binom.test",
    x = num_events,
    n = num_total,
    !!!call_opts
  )

  # ------------------------------------------------------------------------------

  # TODO kind of a mess; figure out a better way of doing this.
  sas_code <-
    c(
      "proc freq data=df;",
      "  table OUTCOME/ binomialOPTIONS;",
      "  exact binomial;",
      "run;"
    )

  sas_code <- gsub("OUTCOME", as.character(orig_cl$y), sas_code)
  has_p <- !identical(p, 0.5)
  has_level <- !identical(level, levels(y)[1])
  has_alt <- !identical(alternative, "two.sided")
  sas_opt <- ""
  if (has_p | has_level) {
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

  # ------------------------------------------------------------------------------

  orig_res <- rlang::eval_tidy(cl)

  res <- new_segue(
    calls = list(r = cl, sas = sas_code),
    options = opts,
    results = orig_res,
    data = list(
      missing = num_missing,
      events = num_events,
      total = num_events
    ),
    classes = "bin_prop_test"
  )
  res
}

#' @export
print.bin_prop_test <- function(x, ...) {
  cli::cli_inform("Testing a Single Binomial Proportion")
  cat("\n")

  if (x$data$missing == 0L) {
    cli::cli_inform("No missing values")
  } else {
    cli::cli_alert_info("There {?was/were} {x$data$missing} missing value{?s}")
  }

  invisible(x)
}
