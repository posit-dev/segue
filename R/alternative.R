#' Convert the Alternative Hypothesis to the Values Used by Stats
#'
#' Functions in this package use the values `"two_sided"`, `"greater"`, and
#' `"lesser"` for the alternative hypothesis. The functions in the stats package
#' (e.g., [stats::binom.test()]) use `"two.sided"`, `"greater"`, and `"less"`.
#' This helper validates the value and converts it.
#'
#' @param alternative A single character string: one of `"two_sided"`,
#' `"greater"`, or `"lesser"`.
#' @param call The execution environment used in error messages.
#' @return A single character string: one of `"two.sided"`, `"greater"`, or
#' `"less"`.
#' @examples
#' convert_alternative("two_sided")
#' convert_alternative("lesser")
#' @export
convert_alternative <- function(alternative, call = rlang::caller_env()) {
  alternative <- rlang::arg_match(
    alternative,
    c("two_sided", "lesser", "greater"),
    error_call = call
  )
  switch(
    alternative,
    two_sided = "two.sided",
    lesser = "less",
    greater = "greater"
  )
}
