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
