# Creates sas-observed-round.csv.gz and sas-observed-format.csv.gz: results of
# the SAS ROUND() function and of SAS 32.d formats that were produced in SAS.
# Also creates sas-results.csv, SAS ROUND() for the cases in cases.csv, once
# code/rounding_segue_sas.sas (see below) has been run in SAS.
# Run from the package root:
#
#   Rscript tests/testthat/fixtures/sas-round/make-sas-observed.R
#
# The SAS results come from the r-to-sas-comparisons repository
# (code/rounding_sas.sas, version 2026-11-10.3), which must be checked out
# next to segue. That program was run in SAS Viya (V.04.00M0P060826, Linux
# X64) with the default DECIMALCONV=COMPATIBLE system option. The comparison
# itself is described in code/rounding_sas.md there. sas-results.csv comes
# from code/rounding_segue_sas.sas in the same repository, which has the cases
# of cases.csv embedded in it.
#
# Numbers are stored as the hex representation of the doubles (as written by
# the SAS HEX16. format), so that R uses the exact bits that SAS used.
#
# Rows outside the documented accuracy of sas_round() and sas_format() have a
# `note`; the tests require an exact match for every other row.
#
#  - ROUND: "beyond 15 digits" when abs(x) or abs(x / unit) is 1e15 or more.
#  - Formats: "15 or more digits shown" when the integer digits (at least one)
#    plus `digits` is 15 or more, and "near halfway" when abs(x) * 10^digits
#    is within 8 units in the last place of a halfway point (there, SAS's
#    tolerance varies from value to value).

library(haven)

comparisons <- file.path("..", "r-to-sas-comparisons", "results")
out_dir <- file.path("tests", "testthat", "fixtures", "sas-round")

read_results <- function(name) {
  read_sas(file.path(comparisons, paste0("rounding_", name, "_sas.sas7bdat")))
}

cases <- read_results("cases")
args <- read_results("args")
funs <- read_results("funs")
formats <- read_results("formats")
session <- read_results("session")
stopifnot(session$program_version == "2026-11-10.3")

cases <- cases[is.finite(cases$x), ]
set.seed(20261110)

# Halfway values on the grids, e.g. 2.675 with a unit of 0.01
is_grid <- grepl("^grid_", cases$case_set)
grid_k <- rep(0L, nrow(cases))
grid_k[is_grid] <- as.integer(sub("/.*$", "", cases$case_label[is_grid]))
last_digit_5 <- is_grid & abs(grid_k) %% 10 == 5

sample_ids <- function(ids, n) {
  ids[sample.int(length(ids), min(n, length(ids)))]
}

# ------------------------------------------------------------------------------
# ROUND(x, unit) and ROUND(x), with valid units

round_rows <- merge(
  funs[funs$fun %in% c("ROUND", "ROUND_1ARG"), ],
  cases[, c("case_id", "case_set", "x", "x_hex", "probe_unit_id")],
  by = "case_id"
)
round_rows <- merge(
  round_rows,
  args[, c("arg_id", "arg", "arg_hex")],
  by = "arg_id",
  all.x = TRUE
)
one_arg <- round_rows$fun == "ROUND_1ARG"
round_rows$arg[one_arg] <- 1
round_rows$arg_hex[one_arg] <- "3FF0000000000000"
round_rows <- round_rows[round_rows$err == 0 & !is.na(round_rows$result), ]

grid_round_ids <- c(
  cases$case_id[last_digit_5],
  sample_ids(cases$case_id[is_grid & !last_digit_5], 1500)
)
keep_round <-
  round_rows$case_set %in%
  c("special", "probe_half", "probe_int") |
  round_rows$case_id %in% grid_round_ids
round_rows <- round_rows[keep_round, ]

round_rows$note <- ""
beyond <- abs(round_rows$x) >= 1e15 | abs(round_rows$x / round_rows$arg) >= 1e15
round_rows$note[beyond] <- "beyond 15 digits"

round_out <- data.frame(
  x_hex = round_rows$x_hex,
  unit_hex = round_rows$arg_hex,
  sas_hex = round_rows$result_hex,
  note = round_rows$note
)
round_out <- unique(round_out)
round_out <- round_out[order(round_out$x_hex, round_out$unit_hex), ]
round_out <- data.frame(id = seq_len(nrow(round_out)), round_out)

# ------------------------------------------------------------------------------
# PUT(x, 32.d) under DECIMALCONV=COMPATIBLE

fmt_rows <- formats[
  formats$decimalconv == "COMPATIBLE" &
    grepl("^32\\.[0-9]+$", formats$fmt) &
    formats$skipped == 0,
]
fmt_rows$digits <- as.integer(sub("^32\\.", "", fmt_rows$fmt))
fmt_rows <- merge(
  fmt_rows,
  cases[, c("case_id", "case_set", "x", "x_hex", "probe_unit_id")],
  by = "case_id"
)

# Probes: the number of decimal places that their unit is halfway for (0 for
# units that are not powers of ten)
positive <- !is.na(args$arg) & args$arg > 0
unit_digits <- rep(0, nrow(args))
unit_digits[positive] <- round(-log10(args$arg[positive]))
is_pow10 <- positive & args$arg == 10^-unit_digits
unit_digits[!is_pow10 | unit_digits < 0] <- 0
probe_digits <- unit_digits[match(fmt_rows$probe_unit_id, args$arg_id)]

grid_digits <- c(grid_1dp = 0, grid_3dp = 2, grid_4dp = 3)
fmt_grid_halfway <-
  fmt_rows$case_id %in%
  cases$case_id[last_digit_5] &
  fmt_rows$digits == grid_digits[fmt_rows$case_set]
fmt_grid_sample <-
  fmt_rows$case_id %in% sample_ids(cases$case_id[is_grid], 1000)

keep_fmt <-
  fmt_rows$case_set == "special" |
  (fmt_rows$case_set %in%
    c("probe_half", "probe_int") &
    fmt_rows$digits == probe_digits) |
  fmt_grid_halfway |
  fmt_grid_sample
fmt_rows <- fmt_rows[keep_fmt, ]

int_digits <- pmax(1, floor(log10(abs(fmt_rows$x))) + 1)
int_digits[fmt_rows$x == 0] <- 1
fmt_rows$note <- ""
many_digits <- int_digits + fmt_rows$digits >= 15
scaled <- abs(fmt_rows$x) * 10^fmt_rows$digits
scaled_ulp <- 2^(floor(log2(pmax(scaled, .Machine$double.xmin))) - 52)
near_half <- abs(scaled - (floor(scaled) + 0.5)) <= 8 * scaled_ulp
fmt_rows$note[near_half] <- "near halfway"
fmt_rows$note[many_digits] <- "15 or more digits shown"

fmt_out <- data.frame(
  x_hex = fmt_rows$x_hex,
  digits = fmt_rows$digits,
  sas_text = trimws(fmt_rows$out),
  note = fmt_rows$note
)
fmt_out <- unique(fmt_out)
fmt_out <- fmt_out[order(fmt_out$x_hex, fmt_out$digits), ]
fmt_out <- data.frame(id = seq_len(nrow(fmt_out)), fmt_out)

# ------------------------------------------------------------------------------

write_gz_csv <- function(dat, file) {
  con <- gzfile(file.path(out_dir, file), "w")
  on.exit(close(con))
  utils::write.csv(dat, con, row.names = FALSE, quote = FALSE)
}

write_gz_csv(round_out, "sas-observed-round.csv.gz")
write_gz_csv(fmt_out, "sas-observed-format.csv.gz")

# ------------------------------------------------------------------------------
# ROUND(x, unit) for cases.csv -> sas-results.csv

segue_path <- file.path(comparisons, "rounding_segue_sas.sas7bdat")
if (file.exists(segue_path)) {
  segue_sas <- read_sas(segue_path)
  stopifnot(all(segue_sas$program_version == "2026-11-12.1"))
  segue_sas <- segue_sas[segue_sas$fun == "ROUND", ]

  # The SAS program has its own copy of the cases; check that it is current.
  cases_csv <- utils::read.csv(
    file.path(out_dir, "cases.csv"),
    colClasses = "character"
  )
  both <- merge(cases_csv, segue_sas, by = "id", suffixes = c("", "_sas"))
  stopifnot(
    nrow(both) == nrow(cases_csv),
    nrow(segue_sas) == nrow(cases_csv),
    identical(both$x_hex, both$x_hex_sas),
    identical(both$unit_hex, both$unit_hex_sas)
  )

  results <- data.frame(
    id = as.integer(segue_sas$id),
    result_hex = trimws(segue_sas$result_hex),
    sas_version = segue_sas$sysvlong
  )
  results <- results[order(results$id), ]
  utils::write.csv(
    results,
    file.path(out_dir, "sas-results.csv"),
    row.names = FALSE,
    quote = FALSE
  )
}
