new_segue <- function(
  calls = list(),
  results = list(),
  options = list(),
  data = list(),
  classes = NULL,
  call = rlang::caller_env()
) {
  if (!is.list(calls)) {
    cli::cli_abort("The {.arg calls} argument should be a list.", call = call)
  }
  cl_nms <- names(calls)
  if (!identical(cl_nms, c("r", "sas"))) {
    cli::cli_abort(
      "The {.arg calls} argument should be a list with elements named {.val r}
       and {.val sas}.",
      call = call
    )
  }
  if (!is.null(calls$r) & !rlang::is_call(calls$r)) {
    cli::cli_abort(
      "The {.val r} element of {.arg calls} should be a call or NULL,
       not {.obj_type_friendly {calls$r}}.",
      call = call
    )
  }
  check_character(calls$sas, allow_null = TRUE, allow_na = FALSE)

  if (!is.list(options)) {
    cli::cli_abort(
      "The {.arg options} argument should be a list,
       not {.obj_type_friendly {options}}.",
      call = call
    )
  }

  if (!is.list(results)) {
    cli::cli_abort(
      "The {.arg results} argument should be a list,
       not {.obj_type_friendly {results}}.",
      call = call
    )
  }

  if (!is.list(data)) {
    cli::cli_abort(
      "The {.arg data} argument should be a list.,
       not {.obj_type_friendly {data}}.",
      call = call
    )
  }

  res <- list(calls = calls, results = results, options = options, data = data)
  class(res) <- c(classes, "segue_object")
  res
}

# ------------------------------------------------------------------------------

#' Turn a segue object object into a tidy tibble
#'
#' This method tidies the orginal result, if it can.
#'
#' @inheritParams generics::tidy
#'
#' @return a tibble
#' @export
tidy.segue_object <- function(x, ...) {
  broom::tidy(x$results, ...)
}
