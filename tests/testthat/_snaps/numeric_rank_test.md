# van_elteren_trafo() computes van Elteren's scores

    Code
      van_elteren_trafo(df$s)
    Condition
      Error in `van_elteren_trafo()`:
      ! `strata` must be a factor, not a character vector.

# numeric_rank_test() reports errors from the underlying engine

    Code
      numeric_rank_test_wilcoxon(bad, settings = settings)
    Condition
      Error:
      ! `numeric_rank_test()` with `method = 'wilcoxon'` failed with error: Error in trafo(data, numeric_trafo = rank_trafo) : data class "character" is not supported

---

    Code
      numeric_rank_test_wilcoxon_stratified(bad, settings = settings)
    Condition
      Error:
      ! `numeric_rank_test()` with `method = 'wilcoxon_stratified'` failed with error: Error in .local(.Object, ...) : 'block' is not a factor

---

    Code
      numeric_rank_test_kruskal_wallis(bad, settings = settings)
    Condition
      Error:
      ! `numeric_rank_test()` with `method = 'kruskal_wallis'` failed with error: Error in trafo(data, numeric_trafo = rank_trafo) : data class "character" is not supported

# numeric_rank_test() validates inputs

    Code
      numeric_rank_test(y, g, data = as.matrix(df))
    Condition
      Error in `numeric_rank_test()`:
      ! `data` must be a data frame, not a character matrix.

---

    Code
      numeric_rank_test(c(y, num), g, data = df)
    Condition
      Error in `numeric_rank_test()`:
      ! `outcome` must select exactly one column of `data`, not 2.

---

    Code
      numeric_rank_test(y, g, data = df, method = "t")
    Condition
      Error in `numeric_rank_test()`:
      ! `method` must be one of "wilcoxon", "wilcoxon_stratified", or "kruskal_wallis", not "t".

---

    Code
      numeric_rank_test(y, g, data = df, alternative = "less")
    Condition
      Error in `numeric_rank_test()`:
      ! `alternative` must be one of "two_sided", "lesser", or "greater", not "less".
      i Did you mean "lesser"?

---

    Code
      numeric_rank_test(y, g, data = df, distribution = "normal")
    Condition
      Error in `numeric_rank_test()`:
      ! `distribution` must be one of "asymptotic", "exact", or "approximate", not "normal".

---

    Code
      numeric_rank_test(chr, g, data = df)
    Condition
      Error in `numeric_rank_test()`:
      ! The `outcome` column chr must be numeric, not a character vector.

---

    Code
      numeric_rank_test(y, num, data = df)
    Condition
      Error in `numeric_rank_test()`:
      ! The `group` column num must be a factor or character vector, not an integer vector.

---

    Code
      numeric_rank_test(y, g, data = df, strata = num, method = "wilcoxon_stratified")
    Condition
      Error in `numeric_rank_test()`:
      ! The `strata` column num must be a factor or character vector, not an integer vector.

---

    Code
      numeric_rank_test(y, g, data = df, method = "wilcoxon_stratified")
    Condition
      Error in `numeric_rank_test()`:
      ! `method = 'wilcoxon_stratified'` requires a `strata` column.

---

    Code
      numeric_rank_test(y, g, data = df, strata = s)
    Condition
      Error in `numeric_rank_test()`:
      ! `strata` can only be used with `method = 'wilcoxon_stratified'`, not `method = 'wilcoxon'`.

---

    Code
      numeric_rank_test(y, k, data = df, method = "kruskal_wallis", alternative = "greater")
    Condition
      Error in `numeric_rank_test()`:
      ! `method = 'kruskal_wallis'` requires `alternative = 'two_sided'`, not "greater".

---

    Code
      numeric_rank_test(y, k, data = df, method = "kruskal_wallis", distribution = "exact")
    Condition
      Error in `numeric_rank_test()`:
      ! `method = 'kruskal_wallis'` does not support `distribution = 'exact'`.
      i Use `distribution = 'approximate'` instead.

---

    Code
      numeric_rank_test(y, g, data = df, reference_choice = "largest")
    Condition
      Error in `numeric_rank_test()`:
      ! `reference_choice` must be one of "smaller" or "first", not "largest".

---

    Code
      numeric_rank_test(y, k, data = df, method = "kruskal_wallis", reference_choice = "smaller")
    Condition
      Error in `numeric_rank_test()`:
      ! `reference_choice` can only be used with `method = 'wilcoxon'`, not `method = 'kruskal_wallis'`.

---

    Code
      numeric_rank_test(y, k, data = df)
    Condition
      Error in `numeric_rank_test()`:
      ! `method = 'wilcoxon'` requires exactly two levels in the `group` column k after removing missing values, not 3.

---

    Code
      numeric_rank_test(y, g, data = df[df$g == "a", ], method = "kruskal_wallis")
    Condition
      Error in `numeric_rank_test()`:
      ! `method = 'kruskal_wallis'` requires at least two levels in the `group` column g after removing missing values, not 1.

---

    Code
      numeric_rank_test(y, g, data = df, not_an_arg = 1)
    Condition
      Error in `numeric_rank_test()`:
      ! `...` must be empty.
      x Problematic argument:
      * not_an_arg = 1

# numeric_rank_test() creates SAS code

    Code
      sas_code(y, g)
    Output
      proc npar1way data=&dsn wilcoxon;
        class &group;
        var &var;
      run;

---

    Code
      sas_code(y, g, alternative = "greater", distribution = "exact")
    Output
      proc npar1way data=&dsn wilcoxon;
        class &group;
        var &var;
        exact wilcoxon;
      run;

---

    Code
      sas_code(y, g, distribution = "approximate")
    Output
      proc npar1way data=&dsn wilcoxon;
        class &group;
        var &var;
        exact wilcoxon / mc;
      run;

---

    Code
      sas_code(y, g, strata = s, method = "wilcoxon_stratified")
    Output
      proc freq data=&dsn;
        tables &strata*&group*&var / cmh2 scores=modridit noprint;
      run;

---

    Code
      sas_code(y, k, method = "kruskal_wallis")
    Output
      proc npar1way data=&dsn wilcoxon;
        class &group;
        var &var;
      run;

---

    Code
      sas_code(y, k, method = "kruskal_wallis", distribution = "approximate")
    Output
      proc npar1way data=&dsn wilcoxon;
        class &group;
        var &var;
        exact wilcoxon / mc;
      run;

# numeric_rank_test() print method

    Code
      res <- print(numeric_rank_test(y, g, data = df_none))
    Message
      Testing for differences in y between g levels via the Wilcoxon rank-sum test
      No missing values

---

    Code
      print(numeric_rank_test(y, g, data = df))
    Message
      Testing for differences in y between g levels via the Wilcoxon rank-sum test
      i There was 1 missing case that was removed prior to analysis.

---

    Code
      print(numeric_rank_test(y, g, data = df_two))
    Message
      Testing for differences in y between g levels via the Wilcoxon rank-sum test
      i There were 2 missing cases that were removed prior to analysis.

---

    Code
      print(numeric_rank_test(y, k, data = df, method = "kruskal_wallis"))
    Message
      Testing for differences in y between k levels via the Kruskal-Wallis test
      i There was 1 missing case that was removed prior to analysis.

