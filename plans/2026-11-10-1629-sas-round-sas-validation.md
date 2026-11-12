# Validate sas_round() and sas_format() against SAS output

## Overview

`sas_round()` and `sas_format()` were written from the SAS documentation. A SAS run in r-to-sas-comparisons (`code/rounding_sas.sas`, version 2026-11-10.3, SAS `V.04.00M0P060826` on Linux, `DECIMALCONV=COMPATIBLE`) produced about 350k `ROUND()` results and 100k `w.d` format results on the same doubles. The comparison is written up in r-to-sas-comparisons `code/rounding_sas.md`. This plan updates both functions and their tests to match what SAS actually does.

## Checklist

- [x] `sas_round()`: decide the rounding on `x / unit` in the reciprocal-of-an-integer branch (fixes values exactly 1e-12 from halfway)
- [x] `sas_round()`: reject units at or below `.Machine$double.xmin` (SAS returns missing with an error for `constant('small')`)
- [x] `sas_format()`: stop using `sas_round()`; round `x * 10^digits` half away from zero with a relative 1e-16 tolerance, and show at most 15 significant digits (zeros after them); subnormals are zero
- [x] `sas_format()`: keep the minus sign on negative values that round to zero (`-0`, `-0.00`)
- [x] Update roxygen docs and regenerate `man/`
- [x] Fixtures `sas-observed-round.csv.gz` and `sas-observed-format.csv.gz` with observed SAS results, plus `make-sas-observed.R`
- [x] Tests: observed SAS results, changed expectations (`-0`), new edge cases, skipped tests that SAS has now answered
- [x] Run tests and `R CMD check` (0 errors, 0 warnings, 0 notes)
- [x] Make the "matches SAS output" test handle missing SAS results (`.` from `HEX16.`) for units SAS rejects
- [x] Add `code/rounding_segue_sas.sas` in r-to-sas-comparisons (cases of `cases.csv` embedded) and have `make-sas-observed.R` write `sas-results.csv` from its results
- [x] Run `code/rounding_segue_sas.sas` in SAS to settle the remaining skipped tests (cases 529-532 and 538-540)
- [x] Build `sas-results.csv`, remove the two `skip()` calls, and fix `sas_round()` for cases 537-540: `1 / unit` must be within 16 ulps of an integer, and products within a few ulps of a decimal with at most four places return that decimal

## Details

- In SAS the halfway fuzz is a relative 1e-12 on `x / unit`, the same as in `sas_round()`. The 5 disagreements were values exactly 1e-12 away, where `x * divisor` and `x / unit` land on different sides. With the change, 27 of 70,590 valid `ROUND` results still differ. All of them have `|x|` or `|x / unit|` of 1e15 or more, where SAS is not exact either, e.g. `ROUND(1000000000000000.5, 0.5)` is `1000000000000001`.
- The `|x / unit| >= 2^52` shortcut stays: removing it gives 283 disagreements, and moving it to 2^53 gives 33.
- SAS `w.d` formats do not use `ROUND()`. Their fuzz is much narrower: `0.4999999999999982` prints as `0` in `32.0`. Rounding to 15 significant digits first matched 99.0% of all `32.d` results. Scaling `x * 10^digits` in floating point with a relative 1e-16 tolerance, and switching to 15 significant digits once the scaled value reaches 1e15, matches 99.40% overall and 99.95% of results with fewer than 15 digits shown. The rest are within a few ulps of halfway, or show 15 or more digits, where SAS's digit generation is irregular.
- Result against the full SAS run: `sas_round()` 27 differences in 70,688 (from 124), `sas_format()` 599 in 100,516 (from 3,207).
- `DECIMALCONV=STDIEEE` formats round the exact binary value (2.675 in `32.2` is `2.67`); `sas_format()` targets the default, `COMPATIBLE`.
