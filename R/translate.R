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
  rlang::check_dots_empty()
  cli::cli_rule("R Code template")
  if (!is.null(x$code$r)) {
    print(x$code$r)
  } else {
    cli::cli_text("No known R equivalent")
  }
  cli::cli_text("")
  cli::cli_rule("SAS Code template")
  if (length(x$code$sas) > 0) {
    print(rlang::exec("sas_proc", !!!x$code$sas))
  } else {
    cli::cli_text("No known SAS equivalent")
  }
  invisible(x)
}
