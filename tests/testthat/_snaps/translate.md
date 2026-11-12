# translate() shows R and SAS code

    Code
      out <- translate(res)
    Message
      -- R Code template -------------------------------------------------------------
    Output
      stats::binom.test(x = 3L, n = 4L, alternative = "two.sided", 
          p = 0.5)
    Message
      
      -- SAS Code template -----------------------------------------------------------
      No known SAS equivalent

# translate() shows SAS code when it is available

    Code
      translate(bin_prop_test(y, method = "exact_sas"))
    Message
      -- R Code template -------------------------------------------------------------
    Output
      segue::exact_binom_test_sas(x = 15L, n = 25L, alternative = "two.sided", 
          p = 0.5)
    Message
      
      -- SAS Code template -----------------------------------------------------------
    Output
      proc freq data=&dsn;
        tables resp / binomial;
        exact binomial;
      run;

---

    Code
      translate(bin_prop_test(y, method = "score"))
    Message
      -- R Code template -------------------------------------------------------------
    Output
      stats::prop.test(x = 15L, n = 25L, alternative = "two.sided", 
          p = 0.5, correct = FALSE)
    Message
      
      -- SAS Code template -----------------------------------------------------------
    Output
      proc freq data={data set};
        tables {analysis column} / binomial(cl=score);
      run;

# translate() skips missing code

    Code
      translate(res)
    Message
      -- R Code template -------------------------------------------------------------
      No known R equivalent
      
      -- SAS Code template -----------------------------------------------------------
      No known SAS equivalent

# translate() validates inputs

    Code
      translate(1)
    Condition
      Error in `translate()`:
      ! There is no known method for `translate()` for a number.

---

    Code
      translate(res, extra = TRUE)
    Condition
      Error in `translate()`:
      ! `...` must be empty.
      x Problematic argument:
      * extra = TRUE

