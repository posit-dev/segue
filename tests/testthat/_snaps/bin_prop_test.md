# bin_prop_test() creates SAS code

    Code
      cat(bin_prop_test(y)$calls$sas, sep = "\n")
    Output
      proc freq data=df;
        table outcome_column / binomial;
        exact binomial;
      run;

---

    Code
      cat(bin_prop_test(y, p = 0.2)$calls$sas, sep = "\n")
    Output
      proc freq data=df;
        table outcome_column / binomial(p=0.2);
        exact binomial;
      run;

---

    Code
      cat(bin_prop_test(y, level = "b")$calls$sas, sep = "\n")
    Output
      proc freq data=df;
        table outcome_column / binomial(level='b');
        exact binomial;
      run;

---

    Code
      cat(bin_prop_test(y, level = "b", p = 0.2)$calls$sas, sep = "\n")
    Output
      proc freq data=df;
        table outcome_column / binomial(p=0.2 level='b');
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
      bin_prop_test(y, level = "c")
    Condition
      Error in `bin_prop_test()`:
      ! Argument `level` must be one of "a" and "b".

---

    Code
      bin_prop_test(y, alternative = "two")
    Condition
      Error in `bin_prop_test()`:
      ! `alternative` must be one of "two.sided", "less", or "greater", not "two".
      i Did you mean "two.sided"?

---

    Code
      bin_prop_test(y, p = 2)
    Condition
      Error in `bin_prop_test()`:
      ! `p` must be a number between 0 and 1, not the number 2.

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
      Testing a Single Binomial Proportion
    Output
      
    Message
      No missing values

---

    Code
      print(bin_prop_test(y_miss))
    Message
      Testing a Single Binomial Proportion
    Output
      
    Message
      i There were 2 missing values

---

    Code
      print(bin_prop_test(y_one))
    Message
      Testing a Single Binomial Proportion
    Output
      
    Message
      i There was 1 missing value

