hex_to_dbl <- function(hex) {
  vapply(
    hex,
    function(val) {
      if (is.na(val) || val == "") {
        return(NA_real_)
      }
      bytes <- substring(val, seq(1, 15, by = 2), seq(2, 16, by = 2))
      readBin(as.raw(strtoi(bytes, base = 16L)), "double", endian = "big")
    },
    double(1),
    USE.NAMES = FALSE
  )
}

read_sas_round_cases <- function() {
  path <- test_path("fixtures", "sas-round", "cases.csv")
  cases <- utils::read.csv(path, colClasses = "character")
  cases$x <- hex_to_dbl(cases$x_hex)
  cases$unit <- hex_to_dbl(cases$unit_hex)
  cases$expected <- hex_to_dbl(cases$expected_hex)
  cases
}

# Results are named by case id so that failures show which cases differ.
sas_round_cases <- function(cases) {
  res <- mapply(
    function(x, unit) {
      sas_round(x, unit = unit)
    },
    cases$x,
    cases$unit,
    USE.NAMES = FALSE
  )
  names(res) <- cases$id
  res
}

# Results observed in SAS; see fixtures/sas-round/make-sas-observed.R
read_sas_observed <- function(type = c("round", "format")) {
  type <- match.arg(type)
  file <- paste0("sas-observed-", type, ".csv.gz")
  path <- test_path("fixtures", "sas-round", file)
  res <- utils::read.csv(path, colClasses = "character")
  res$x <- hex_to_dbl(res$x_hex)
  if (type == "round") {
    res$unit <- hex_to_dbl(res$unit_hex)
    res$sas <- hex_to_dbl(res$sas_hex)
  } else {
    res$digits <- as.integer(res$digits)
  }
  res
}
