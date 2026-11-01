check_factor <- function(
  x,
  nm = "y",
  mult_lvls = TRUE,
  call = rlang::caller_env()
) {
  if (!isTRUE(is.factor(x))) {
    cli::cli_abort(
      "Argument {.arg {nm}} should be a {.cls factor}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
  if (mult_lvls) {
    num_lvls <- length(levels(x))
    if (num_lvls < 2) {
      cli::cli_abort(
        "Argument {.arg {nm}} should be a {.cls factor} with at least two levels.",
        call = call
      )
    }
  }
  invisible(NULL)
}


check_level <- function(lvl, y, nm = "event_level", call = rlang::caller_env()) {
  if (is.null(lvl)) {
    return(levels(y)[1])
  }
  num_lvl <- length(lvl)
  if (num_lvl != 1 || is.na(lvl)) {
    cli::cli_abort(
      "Argument {.arg {nm}} should be a single character value, not {.obj_type_friendly {lvl}}.",
      call = call
    )
  }

  lvls <- levels(y)
  if (!lvl %in% lvls) {
    cli::cli_abort(
      "Argument {.arg {nm}} must be one of {.val {lvls}}.",
      call = call
    )
  }

  invisible(lvl)
}

check_empty_ellipse <- function(..., call = rlang::caller_env()) {
  terms <- rlang::quos(...)
  nms <- names(terms)
  if (!is_empty(terms)) {
    cli::cli_abort(
      "The ellipses are not used for this function. The following options were
      passed to them: {.arg {nms}}.",
      call = call
    )
  }
  terms
}
