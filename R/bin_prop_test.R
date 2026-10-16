#' Test a Single Binomial Proportions
#' @param y A factor with two levels.
#' @param ... Not currently used.
#' @export
bin_prop_test <- function(y, ...) {
  UseMethod("bin_prop_test")
}

#' @rdname bin_prop_test
#' @export
#' @export bin_prop_test.default
bin_prop_test.default <- function(y, ...) {
  cli::cli_abort(
    "Argument {.arg {y}} should be a {.cls factor} with at least two levels,
    not {.obj_type_friendly {y}}.",
    call = call
  )
}

#' @rdname bin_prop_test
#' @export
#' @export bin_prop_test.default
bin_prop_test.factor <- function(
  y,
  method = c("Clopper-Pearson", "Wald", "mid-p", "score"),
  level = NULL,
  alt = "two.sided"
) {
  check_factor(y)
  # check_dots_empty()

  cl <- match.call()
  var_nm <- cl$y

  if (is.null(level)) {
    level <- levels(y)[1]
  }

  is_na <- is.na(y)
  num_missing <- sum(is_na)

  compl_y <- y[!is_na(y)]
  num_events <- sum(compl_y == level)
  num_total <- length(compl_y)

  cl <- rlang::call2(
    "binom.test",
    x = num_events,
    n = num_total,
    alternative = alt
  )

  orig_res <- rlang::eval_tidy(cl)

  res <- list(
    r_cl = cl,
    sas_cl = NULL,
    results = orig_res,
    data = c(missing = num_missing, events = num_events, total = num_events)
  )
  class(res) <- c("bin_prop_test", "segue_obj")
  res
}


#' Turn a segue object object into a tidy tibble
#'
#' This method tidies the model in a parsnip model object, if it exists.
#'
#' @inheritParams generics::tidy
#'
#' @return a tibble
#' @export
tidy.segue_obj <- function(x, ...) broom::tidy(x$results, ...)
