# Scaffold `numeric_rank_test()`

## Overview

Add `numeric_rank_test()`, a rank-based test for differences in a numeric outcome between groups, with three methods built on coin: the Wilcoxon rank-sum test, a stratified (van Elteren) Wilcoxon test, and the Kruskal-Wallis test. Columns come from `data` via tidyselect. The function reproduces the defaults SAS uses (PROC NPAR1WAY and PROC FREQ), including the Wilcoxon continuity correction. This is scaffolding from the `/new-segue-method` skill: complete, runnable structure, with `TODO` notes for the parts that still need checking against SAS output.

## Checklist

- [x] Interview (identity, methods, SAS code, arguments, data and output)
- [x] Prototype the SAS continuity correction and van Elteren scores with coin
- [x] Relax `new_segue()` so `results` can be an S4 object, and update its test and snapshot
- [x] Add `select_single_column()` to `R/checks.R` with tests in `tests/testthat/test-checks.R`
- [x] Write `R/numeric_rank_test.R` (function, print method, tidy method, engine helpers)
- [x] Write `tests/testthat/test-numeric_rank_test.R`
- [x] DESCRIPTION: add tidyselect and tibble to Imports, coin to Suggests
- [x] `NEWS.md` bullet
- [x] `inst/WORDLIST` additions
- [x] Add the `Numeric Outcomes` row to the skill's prefix table (first use of the prefix)
- [x] `air format .`, `devtools::document()`, `devtools::test(filter = "^numeric_rank_test|^segue|^checks")`
- [x] Review `tests/testthat/_snaps/numeric_rank_test.md`
- [ ] TODO: verify the SAS code for `method = 'wilcoxon'` against SAS output
- [ ] TODO: verify the SAS code for `method = 'wilcoxon_stratified'` against SAS output
- [ ] TODO: verify the SAS code for `method = 'kruskal_wallis'` against SAS output
- [x] One-sided continuity correction: decided (2026-11-17) to keep coin with the correction toward the mean and document that it differs from `stats::wilcox.test(correct = TRUE)` when Z points away from the alternative. Considered switching to `stats::wilcox.test()` and rejected it: it reports no Z, has no exact test with ties and no Monte Carlo option, and coin is still needed for van Elteren
- [ ] TODO: check against SAS output whether PROC NPAR1WAY's one-sided correction matches; if it does, remove the caveat from `@details`
- [ ] Note: `air format .` corrupts the hex literal `0x1.2492492492485p-3` in `tests/testthat/test-sas_round.R`; format individual files until that is fixed
- [x] Sign of Z: added `reference_choice = c("smaller", "first")` (default `"smaller"`, as SAS) for `method = "wilcoxon"`; it relevels the group factor so the reference group is first. When the groups are the same size, R's first factor level is used, which can differ from SAS's sorted class order (documented)

## Details

### Identity

- Name: `numeric_rank_test()`. Prefix `numeric`, description `rank`, kind `test`. `@family Numeric Outcomes` (first use of this prefix).
- A plain function, not an S3 generic: `numeric_rank_test(outcome, group, data, ..., strata = NULL, method = c("wilcoxon", "wilcoxon_stratified", "kruskal_wallis"), alternative = "two_sided", distribution = "asymptotic")`.
- `outcome`, `group`, and `strata` use tidyselect, and each must select exactly one column of `data`.

### Methods

| `method` | Engine (R) | SAS |
|---|---|---|
| `wilcoxon` (default) | `coin::wilcox_test(outcome ~ group)`, then the SAS 0.5 continuity correction (asymptotic only) | `proc npar1way wilcoxon; class &group; var &var;` |
| `wilcoxon_stratified` | `coin::independence_test(outcome ~ group \| strata, ytrafo = within-stratum rank / (n_k + 1))` (van Elteren) | `proc freq; tables &strata*&group*&var / cmh2 scores=modridit noprint;` |
| `kruskal_wallis` | `coin::kruskal_test(outcome ~ group)` | `proc npar1way wilcoxon; class &group; var &var;` |

- `coin::wilcox_test()` silently ignores a user `ytrafo`, so the van Elteren test uses `coin::independence_test()`. A prototype matched a hand-computed van Elteren Z.
- The corrected Z, `(T - E - 0.5 * sign(T - E)) / sqrt(V)` from `coin::statistic(type = "linear")`, `coin::expectation()`, and `coin::variance()`, matched `stats::wilcox.test(exact = FALSE, correct = TRUE)` in a prototype.
- SAS applies no continuity correction to exact tests, so the correction applies only when `distribution = "asymptotic"`.
- The Kruskal-Wallis chi-square should match SAS (df = k - 1, mid-ranks, no correction). `coin::kruskal_test()` matched `stats::kruskal.test()`.
- `distribution = "exact"` adds `exact wilcoxon;` to the npar1way code, and `"approximate"` adds `exact wilcoxon / mc;`. SAS and coin both default to 10,000 Monte Carlo samples.
- `wilcoxon_stratified` with a non-asymptotic distribution has no SAS equivalent in proc freq, so its SAS code is empty.
- SAS has no option for `alternative`; npar1way reports both one-sided and two-sided p-values. PROC FREQ's `cmh2` reports a chi-square equal to Z², which is two-sided.
- `res$results` is the raw coin S4 object, so `new_segue()` must accept S4 objects. `tidy.numeric_rank_test()` returns `statistic`, `p.value`, `method`, and `alternative` (plus `parameter` for Kruskal-Wallis), and applies the continuity correction for asymptotic `wilcoxon`.

### Arguments and validation

- `data`: a data frame.
- `outcome`: numeric column. `group`, `strata`: factor or character column, converted to a factor with unused levels dropped after removing incomplete rows.
- `method`: `rlang::arg_match()`. `alternative`: `convert_alternative()`. `distribution`: `rlang::arg_match()` over `"asymptotic"`, `"exact"`, and `"approximate"`.
- Mismatches raise errors: `strata` missing for `wilcoxon_stratified`, `strata` given for the other methods, `alternative` other than `"two_sided"` for `kruskal_wallis`, and `distribution = "exact"` for `kruskal_wallis` (coin cannot compute it).
- Group levels after removing incomplete rows: exactly 2 for the Wilcoxon methods, at least 2 for `kruskal_wallis`.
- `...` must be empty (`rlang::check_dots_empty()`).

### Data and output

- Complete-case removal over the outcome, group, and strata columns. `data_characteristics`: `missing` (rows removed), `total`, `groups` (number of group levels), `strata` (number of strata; 1 when unstratified).
- Print header: "Testing for differences in {outcome} between {group} levels via the {method label} test".
- The R call in `code$r` uses the symbol `data` for the complete-case data, so `translate()` shows `coin::wilcox_test(tau ~ Class, data = data, ...)` rather than the whole data frame.
- Example: `modeldata::ad_data` with `tau`, `Class`, and `Genotype`.
- Dependencies: tidyselect and tibble in Imports; coin in Suggests, with `rlang::check_installed("coin")`.
- References: Hollander, Wolfe, and Chicken (2014); van Elteren (1960); Kruskal and Wallis (1952); Hothorn et al. (2006).
