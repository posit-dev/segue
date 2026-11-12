# bin_prop_test() reports errors from the underlying test

    Code
      bin_prop_test(all_missing, method = "exact")
    Condition
      Error:
      ! `bin_prop_test()` with `method = 'exact'` failed with error: Error in stats::binom.test(x = 0L, n = 0L, alternative = "two.sided", : 'n' must be a positive integer >= 'x'

---

    Code
      bin_prop_test(all_missing, method = "exact_sas")
    Condition
      Error:
      ! `bin_prop_test()` with `method = 'exact_sas'` failed with error: Error in segue::exact_binom_test_sas(x = 0L, n = 0L, alternative = "two.sided", : `n` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      bin_prop_test(all_missing, method = "score")
    Condition
      Error:
      ! `bin_prop_test()` with `method = 'score'` failed with error: Error in stats::prop.test(x = 0L, n = 0L, alternative = "two.sided", p = 0.5, : elements of 'n' must be positive

# bin_prop_test() validates inputs

    Code
      bin_prop_test(1:3)
    Condition
      Error in `bin_prop_test()`:
      ! There is no known method for `bin_prop_test()` for an integer vector.

---

    Code
      bin_prop_test(factor(c("a", "a")))
    Condition
      Error in `bin_prop_test()`:
      ! Argument `y` should be a <factor> with at least two levels.

---

    Code
      bin_prop_test(y, event_level = "c")
    Condition
      Error in `bin_prop_test()`:
      ! Argument `event_level` must be one of "a" and "b".

---

    Code
      bin_prop_test(y, alternative = "two.sided")
    Condition
      Error in `bin_prop_test()`:
      ! `alternative` must be one of "two_sided", "lesser", or "greater", not "two.sided".
      i Did you mean "two_sided"?

---

    Code
      bin_prop_test(y, null_value = 2)
    Condition
      Error in `bin_prop_test()`:
      ! `null_value` must be a number between 0 and 1, not the number 2.

---

    Code
      bin_prop_test(y, conf.level = 0.9)
    Condition
      Error in `bin_prop_test()`:
      ! `...` must be empty.
      x Problematic argument:
      * conf.level = 0.9

# bin_prop_test() print method

    Code
      res <- print(bin_prop_test(y))
    Message
      Testing a Single Binomial Proportion via exact Test
      No missing values

---

    Code
      print(bin_prop_test(y_miss))
    Message
      Testing a Single Binomial Proportion via exact Test
      i There were 2 missing cases that were removed prior to analysis.

---

    Code
      print(bin_prop_test(y_one))
    Message
      Testing a Single Binomial Proportion via exact Test
      i There was 1 missing case that was removed prior to analysis.

