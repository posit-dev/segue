#' segue objects
#'
#' @description
#' The analysis functions in segue, such as [bin_prop_test()] and
#' [numeric_rank_test()], return a segue object. A segue object is a list with
#' two classes: the name of the function that made it and `"segue_object"`,
#' for example `c("bin_prop_test", "segue_object")`.
#'
#' @section Elements:
#' - `code`: A list with two elements, which [translate()] prints:
#'   - `r`: The call to the R function that computed the results, or `NULL`
#'     when there is no R equivalent.
#'   - `sas`: A list of arguments to [sas_proc()] for the equivalent
#'     \pkg{SAS} code, or an empty list when there is no \pkg{SAS}
#'     equivalent. The code uses placeholders, such as `&dsn` and `&var`, for
#'     the names of the data set and the variables.
#' - `results`: The object from the R function that computed the results, as
#'   it was returned. For example, an `htest` list from [stats::binom.test()]
#'   or an S4 object from \pkg{coin}. Some functions adjust these results in
#'   their [tidy()] method. The help page of each function describes these
#'   adjustments.
#' - `options`: A named list of the option values that the function used. The
#'   names are the argument names of the segue function, not of the R function
#'   that computed the results.
#' - `data_characteristics`: A named list that describes the data after the
#'   missing values are removed. It always has `missing`, the number of
#'   observations or rows that were removed because of missing values. The
#'   other elements depend on the function, such as `events` and `total` for
#'   [bin_prop_test()].
#'
#' @section Methods:
#' - `print()`: Shows a short description of the analysis and the number of
#'   missing values that were removed.
#' - [tidy()]: Returns the results as a tibble.
#' - [translate()]: Shows the R and \pkg{SAS} code for the analysis.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' res <- bin_prop_test(modeldata::ad_data$Class)
#' class(res)
#' names(res)
#' res$options
#' res$data_characteristics
#' @name segue_object
NULL

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

  # S4 results come from engines such as coin.
  if (!is.list(results) && !isS4(results)) {
    cli::cli_abort(
      "The {.arg results} argument should be a list or an S4 object,
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

#' Turn a segue Object into a Tidy Tibble
#'
#' This method tidies the original result, if it can.
#'
#' @inheritParams generics::tidy
#'
#' @return A tibble.
#' @examplesIf rlang::is_installed("modeldata")
#' cog_ability <- modeldata::ad_data$Class
#' test_res <- bin_prop_test(cog_ability)
#' tidy(test_res)
#' @export
tidy.segue_object <- function(x, ...) {
  broom::tidy(x$results, ...)
}
