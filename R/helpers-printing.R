report_missing_simple <- function(x) {
  if (is.null(x$data_characteristics$missing)) {
    return(invisible(x))
  }
  if (x$data_characteristics$missing == 0L) {
    cli::cli_inform("No missing values")
  } else {
    cli::cli_alert_info(
      "There {?was/were} {x$data_characteristics$missing} missing case{?s} that {?was/were} \\
      removed prior to analysis."
    )
  }
  return(invisible(x))
}

# Make another for case-wise deletion in a data frame
