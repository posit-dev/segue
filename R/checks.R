check_factor <- function(
  x,
  mult_lvls = TRUE,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is.factor(x)) {
    rlang::stop_input_type(x, "a factor", arg = arg, call = call)
  }
  if (mult_lvls && length(levels(x)) < 2) {
    cli::cli_abort(
      "Argument {.arg {arg}} should be a {.cls factor} with at least two levels.",
      call = call
    )
  }
  invisible(NULL)
}

# Returns the validated level (or the first level when `lvl` is `NULL`).
check_level <- function(
  lvl,
  y,
  arg = rlang::caller_arg(lvl),
  call = rlang::caller_env()
) {
  lvls <- levels(y)
  if (is.null(lvl)) {
    return(lvls[1])
  }
  if (!rlang::is_string(lvl) || is.na(lvl)) {
    rlang::stop_input_type(
      lvl,
      "a single character value",
      arg = arg,
      call = call
    )
  }
  if (!lvl %in% lvls) {
    cli::cli_abort(
      "Argument {.arg {arg}} must be one of {.val {lvls}}.",
      call = call
    )
  }
  lvl
}

# Returns the name of the one column of `data` that the tidyselect quosure
# `quo` selects.
select_single_column <- function(quo, data, arg, call = rlang::caller_env()) {
  pos <- tidyselect::eval_select(
    quo,
    data,
    allow_rename = FALSE,
    error_call = call
  )
  if (length(pos) != 1) {
    cli::cli_abort(
      "{.arg {arg}} must select exactly one column of {.arg data}, not
       {length(pos)}.",
      call = call
    )
  }
  names(pos)
}
