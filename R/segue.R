new_segue <- function(
  code = list(),
  results = list(),
  options = list(),
  data_characteristics = list(),
  classes = NULL,
  call = rlang::caller_env()
) {
  if (!is.list(code)) {
    cli::cli_abort("The {.arg code} argument should be a list.", call = call)
  }
  cl_nms <- names(code)
  if (!identical(cl_nms, c("r", "sas"))) {
    cli::cli_abort(
      "The {.arg code} argument should be a list with elements named {.val r}
       and {.val sas}.",
      call = call
    )
  }
  if (!is.null(code$r) & !rlang::is_call(code$r)) {
    cli::cli_abort(
      "The {.val r} element of {.arg code} should be a call or NULL,
       not {.obj_type_friendly {code$r}}.",
      call = call
    )
  }
  if (!is.list(code$sas)) {
    cli::cli_abort(
      "The {.arg sas} element of {.arg code} argument should be a list,
       not {.obj_type_friendly {code$sas}}",
      call = call
    )
  }

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

  if (!is.list(data_characteristics)) {
    cli::cli_abort(
      "The {.arg data_characteristics} argument should be a list,
       not {.obj_type_friendly {data_characteristics}}.",
      call = call
    )
  }

  res <- list(
    code = code,
    results = results,
    options = options,
    data_characteristics = data_characteristics
  )
  class(res) <- c(classes, "segue_object")
  res
}

# ------------------------------------------------------------------------------

#' Turn a segue object object into a tidy tibble
#'
#' This method tidies the original result, if it can.
#'
#' @inheritParams generics::tidy
#'
#' @return a tibble
#' @export
tidy.segue_object <- function(x, ...) {
  broom::tidy(x$results, ...)
}
