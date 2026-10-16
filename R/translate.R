#' Show code in R and SAS for an Analysis
#'
#' @param x An object
#' @param ... Not currently used
#' @return `x` invisibly.
#' @examples
#' \donttest{
#' if (rlang::is_installed(c("modeldata"))) {
#'   cog_ability <- modeldata::ad_data$Class
#'   str(cog_ability)
#'   test_res <- bin_prop_test(cog_ability)
#'   translate(test_res)
#' }
#' }
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
  check_empty_ellipse(...)
  if (!is.null(x$calls$r)) {
    cli::cli_rule("R Code template")
    print(x$calls$r)
  }
  cat("\n")
  if (!is.null(x$calls$sas)) {
    cli::cli_rule("SAS Code template")
    cat(x$calls$sas, sep = "\n")
  }
  cat("\n")
  invisible(x)
}
