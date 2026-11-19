---
name: new-segue-method
description: Scaffold a new segue analysis function (hypothesis test, confidence interval, or statistical model) modeled on bin_prop_test(). Interviews the user for the function name, input classes, `method` values, arguments, the R and SAS code for each method, and documentation details, then writes R/{name}.R, tests/testthat/test-{name}.R, a NEWS bullet, and a plan file. Use when the user asks to "add a new test/interval/model function", "scaffold a segue method", or runs /new-segue-method.
---

# Scaffold a new segue analysis function

segue analysis functions wrap one or more computational engines (functions in base R, other packages, or segue itself) behind one idiomatic interface. Each returns a `segue_object` that holds the results, the options used, data characteristics, and R and SAS code templates that `translate()` prints. `R/bin_prop_test.R` and `tests/testthat/test-bin_prop_test.R` are the reference implementation. Read both before you write anything, because the templates in this skill are a loose abstraction of them and the current files win if they disagree.

The output of this skill is scaffolding: complete, runnable structure with the details the user gave you filled in, and clearly marked `TODO` comments where information is missing. It is not a finished, validated statistical method.

Ignore `@includeRmd` and `man/_rmd/` for now. Do not create an Rmd file or add the tag.

## Naming convention

Function names follow `{outcome type}_{description}_{analysis kind}`, all lower snake case.

- **Outcome type**: the type of the outcome data. Use an existing prefix when one fits. Find the current prefixes with `ls R/` and by reading `@family` tags (`grep -h "@family" R/*.R | sort -u`). Known so far:

  | Prefix     | Outcome                                    | `@family`            |
  |------------|--------------------------------------------|----------------------|
  | `bin`      | Binary / binomial                          | `Binomial Outcomes`  |
  | `censored` | Censored, such as time to event (survival) | `Censored Outcomes`  |
  | `numeric`  | Continuous numeric                         | `Numeric Outcomes`   |
  | `count`    | Counts, such as Poisson                    | `Count Outcomes`     |

  `Binomial Outcomes` and `Numeric Outcomes` are in use so far; the other `@family` titles follow the same pattern. Confirm the title with the user the first time a prefix is used, then update this table if it changes. More outcome types will be added. If the user needs a new prefix, confirm it with them, ask for the `@family` title, and add a row to this table in this file so the next run knows about it.

- **Description**: a short noun phrase for the quantity or comparison, such as `prop`, `two_prop`, `mean`, `mean_diff`, `odds_ratio`.

- **Analysis kind**: what the function produces. Use one of `test` (hypothesis test), `interval` (confidence interval), or `model` (statistical model fit). If the user wants a different kind, confirm the word with them.

Examples: `bin_prop_test`, `bin_prop_interval`, `bin_two_prop_test`, `bin_logistic_model`.

## Workflow

### 1. Interview the user

Use the `AskUserQuestion` tool for questions with a small set of natural options, and plain-text questions for free-form content (method descriptions, code, references). Ask in rounds; do not dump every question at once. If the user passed arguments to the skill (for example a proposed name), use them and skip those questions. Offer sensible defaults drawn from `bin_prop_test()` so the user can accept them quickly.

**Round 1: identity**

1. Outcome type prefix: offer `bin`, `censored`, `numeric`, and `count` (the "Other" choice covers a new one).
2. Description stub.
3. Analysis kind: `test`, `interval`, `model`.
4. Input classes that get S3 methods (for example `factor`, `numeric`, `data.frame`, `formula`). The first argument is the dispatch argument; ask what to call it (`y` for a single outcome vector, `formula` or `data` for models). There is always a `.default` method that errors.

Assemble the name, check it against the convention, and check that `R/{name}.R` does not already exist. Show the name and confirm before continuing.

**Round 2: methods**

For each value of `method` (the first one is the default), collect:

- the value, in lower snake case (for example `"exact"`, `"exact_sas"`, `"score"`, `"wald"`);
- a one-line description for the `@param method` bullet;
- the R function that computes it, with namespace (for example `stats::binom.test`), and the arguments it receives from the data and from the options. If no R function exists yet, record that; the helper gets a `TODO` and its test is skipped;
- the SAS equivalent (see "SAS code" below);
- whether `broom::tidy()` can tidy the result. If not, note that a `tidy.{name}` method is needed and add it to the plan as a `TODO`.

**R and SAS defaults**

Before you ask about SAS code, compare the defaults of the R engine with the defaults of the SAS proc for each method: continuity corrections, tie handling, score types, variance estimates, the sign or direction of the statistic, which p-values are reported, and extra estimates such as the Hodges-Lehmann shift. Do not assume that segue should copy SAS. Work through these steps in order and stop at the first one that applies:

1. **The defaults match.** Say so. There is nothing to decide.
2. **They differ, and the SAS configuration can be accommodated.** For example, apply SAS's correction to the engine's output, pass engine arguments that reproduce the SAS behavior, or add an argument that switches between the two behaviors.
3. **They differ, and the SAS configuration needs its own method.** For example, `"exact_sas"` next to `"exact"` in `bin_prop_test()`.

For each difference, show the user the options (match as is, accommodate, new method, or keep the R behavior and document the difference), what each one costs, and your recommendation. Then let the user decide. Record every difference and the decision in the plan.

**SAS code**

For each method, use `AskUserQuestion` to ask how to get the SAS code, with these choices:

- **Draft it for me** (recommended): you write the SAS code.
- **I'll provide it**: the user pastes SAS code or `sas_proc()` arguments.
- **No SAS equivalent**: the helper uses `sas_code <- list()` and `translate()` says so.

When you draft it:

1. Pick the proc and statements that give the same result as the R engine, for example `proc freq` with `tables &var / binomial(p=0.3 level="b"); exact binomial;` for an exact test of one proportion. Be specific about options that change the numbers, such as continuity corrections, the interval type (`cl=wilson`, `cl=exact`), or the variance estimate.
2. Map every segue option to SAS syntax where SAS has one (`null_value` to `p=`, `event_level` to `level=`, `conf_level` to `alpha=` as `1 - conf_level`). If an option has no SAS equivalent, or SAS reports something different (for example, `proc freq` reports both one-sided p-values instead of taking an `alternative`), add a code comment in the helper and note it in the plan.
3. Use `sas_proc()` placeholders (`&dsn`, `&var`, `&row`, `&group`, ...) for the data set and variable names, as in the `sas_proc()` defaults.
4. Express the code as `sas_proc()` arguments, and render it for the user to review: `Rscript -e 'devtools::load_all(quiet = TRUE); print(sas_proc(...))'`. Show the rendered code for every method in one message and ask the user to confirm or correct it.
5. Say which parts you are not sure about. Add a plan item "Verify the SAS code for `method = '{method}'` against SAS output" for each drafted method, because nobody has run it in SAS yet.

**Round 3: arguments**

Propose arguments based on the analysis kind and confirm them. Use snake case on the segue side and convert to the engine's names in `call_opts`.

| Kind       | Usual arguments                                                                                               |
|------------|---------------------------------------------------------------------------------------------------------------|
| `test`     | `null_value`, `alternative = "two_sided"` (validated and converted by `convert_alternative()`)               |
| `interval` | `conf_level = 0.95`                                                                                            |
| `model`    | `data`, a formula, model-specific options                                                                     |
| any        | `event_level = NULL` for factor outcomes (validated by `check_level()`), and other method-specific options    |

For each argument, get the default, the validation rule (prefer existing helpers in `R/checks.R` and the standalone `check_*()` functions in `R/import-standalone-types-check.R`), and the engine argument name it maps to. Read the `arg-checking` tidy skill (`usethis::learn_tidy_skill("arg-checking")`) if the validation is not obvious.

**Round 4: data and output**

1. Data summaries computed after removing missing values (for `bin_prop_test()`: `events`, `total`). These go to the engine helpers and to `data_characteristics`. `missing` is always recorded so `report_missing_simple()` works. For data frame inputs, use complete-case removal over the columns the analysis uses.
2. The print header, such as "Testing a Single Binomial Proportion via {method} Test".
3. Title and short description for the roxygen block.
4. `@seealso` links and `@references` (accept "none for now").
5. Example data for `@examplesIf` (default: a column from `modeldata`).

### 2. Write the plan file

Per the user's global instructions, write `plans/{yyyy-mm-dd}-{hhmm}-{name}-scaffold.md` (get the time with `date +%Y-%m-%d-%H%M`). Include an overview, a checklist of the steps below, and a details section that records every answer from the interview, including the open `TODO` items. Do not hard-wrap prose. Check items off as you go.

### 3. Write `R/{name}.R`

Start from `templates/function.R.tmpl` in this skill directory. Replace every `{{placeholder}}`; repeat the per-method blocks once per method; delete blocks that do not apply (for example `alternative` for an interval). Rules:

- One exported generic, one `.default` method that aborts, and one exported method per input class. Each method has `@rdname {name}`, `@export`, and `@export {name}.{class}`, as in `bin_prop_test()`.
- The class method validates every argument first, then ends with `rlang::check_dots_empty()`.
- `opts` records the segue-side option values. `call_opts` holds the engine-side names and values that are spliced into the engine call.
- One internal helper per method, named `{name}_{method}` (no roxygen). It builds the call with `rlang::call2(..., .ns = ...)`, evaluates it inside `try()`, rethrows failures with `cli::cli_abort(..., call = NULL)`, and returns `list(results = res, code = list(r = cl, sas = sas_code))`.
- The helper also takes `settings`, the segue-side `opts`, and builds `sas_code` from it so the SAS code shows the option values the user chose. Build option-dependent pieces in temporary variables with `paste0()` or `cli::format_inline()`, never with an inline `if`/`else`.
- The dispatching `switch()` calls these helpers.
- `print.{name}()` prints the header and calls `report_missing_simple(x)`, and returns `invisible(x)`.
- Follow the project style: base pipe, `\()` only for one-line lambdas, no inline `if`/`else` inside another expression (assign to a temporary first), roxygen wrapped at 80 characters.

### 4. Write `tests/testthat/test-{name}.R`

Start from `templates/test.R.tmpl`. Use small inline data. Include:

- a structure test for the default method (classes, element names, `options`, `data_characteristics`, `results` class, `code$r` compared to the expected `rlang::call2()`);
- one test per method that compares `results` with a direct call to the engine and checks `code$r` and `code$sas` with `expect_equal()`;
- a snapshot test (`expect_snapshot(error = TRUE, ...)`) for errors raised by each engine;
- a snapshot test for input validation, one `expect_snapshot(error = TRUE, ...)` per argument rule, including the `.default` method and non-empty dots;
- a snapshot test for the print method with zero, one, and several missing values;
- a snapshot test of the rendered SAS code (`rlang::exec("sas_proc", !!!res$code$sas)`) for each method that has SAS code, with the default options and with non-default options, so the reviewer can read the SAS code that each option setting gives.

Use snapshots, not `expect_error()` with a regex, unless the output contains stochastic content. Do not use `expect_true()` or `expect_false()`. Put no code outside `test_that()` blocks; shared data builders go in `tests/testthat/helper-{name}.R`. For methods whose engine does not exist yet, keep the test but start it with `skip("until {engine} is available")`.

### 5. Update supporting files

- Add a `NEWS.md` bullet under the development heading: "* New `{name}()` {short description}." Keep it on one line and keep bullets ordered as described in `AGENTS.md`.
- If `_pkgdown.yml` exists, add `{name}` to the reference index (the `@family` section if there is one), then run `pkgdown::check_pkgdown()`.
- Add any new words that the spell checker will flag to `inst/WORDLIST`.

### 6. Build and check

Run these and fix what they report:

```sh
air format .
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test(filter = "^{name}")'
```

The first test run writes new snapshots to `tests/testthat/_snaps/{name}.md`. Read that file and check that every message reads well; tell the user to review it too. Run `Rscript -e 'devtools::check()'` at the end if the user wants a full check.

### 7. Report

Tell the user which files you created or changed, list every remaining `TODO` (from the code and the plan), and point them to the new snapshot file. Do not commit, push, or open a PR unless the user asks for that specific action.
