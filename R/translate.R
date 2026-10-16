#' @export
translate <- function(x, ...) {
  UseMethod("translate")
}

#' @rdname translate
#' @export
#' @export translate.default
translate.default <- function(x, ...) {
  cli::cli_abort(
    "There is no known method for {.fn translate} for {.obj_type_friendly {x}}."
  )
}

#' @rdname translate
#' @export
#' @export translate.segue_object
translate.segue_object <- function(x, ...) {
  if (!is.null(x$call$r)) {
    cli::cli_rule("R Code template")
    print(x$call$r)
  }
  cat("\n")
  if (!is.null(x$call$sas)) {
    cli::cli_rule("SAS Code template")
    cat(x$call$sas, sep = "\n")
  }
  cat("\n")
  invisible(x)
}
