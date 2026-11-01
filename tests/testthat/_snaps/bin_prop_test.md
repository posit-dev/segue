# bin_prop_test() creates SAS code

    Code
      cat(bin_prop_test(y)$code$sas, sep = "\n")
    Output
      
      proc freq data = {data set};
        table {data column} / binomial;
        exact binomial;
      run;
      

---

    Code
      cat(bin_prop_test(y, null_value = 0.2)$code$sas, sep = "\n")
    Output
      
      proc freq data = {data set};
        table {data column} / binomial;
        exact binomial;
      run;
      

---

    Code
      cat(bin_prop_test(y, event_level = "b")$code$sas, sep = "\n")
    Output
      
      proc freq data = {data set};
        table {data column} / binomial;
        exact binomial;
      run;
      

---

    Code
      cat(bin_prop_test(y, event_level = "b", null_value = 0.2)$code$sas, sep = "\n")
    Output
      
      proc freq data = {data set};
        table {data column} / binomial;
        exact binomial;
      run;
      

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
      ! The ellipses are not used for this function. The following options were passed to them: `conf.level`.

# bin_prop_test() print method

    Code
      res <- print(bin_prop_test(y))
    Message
      Testing a Single Binomial Proportion via exact Test
    Output
      
    Message
      No missing values

---

    Code
      print(bin_prop_test(y_miss))
    Message
      Testing a Single Binomial Proportion via exact Test
    Output
      
    Message
      i There were 2 missing cases that were removed prior to analysis.

---

    Code
      print(bin_prop_test(y_one))
    Message
      Testing a Single Binomial Proportion via exact Test
    Output
      
    Message
      i There was 1 missing case that was removed prior to analysis.

