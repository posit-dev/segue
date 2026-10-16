# translate() shows R and SAS code

    Code
      out <- translate(res)
    Message
      -- R Code template -------------------------------------------------------------
    Output
      binom.test(x = 3L, n = 4L, alternative = "two.sided", p = 0.5)
      
    Message
      -- SAS Code template -----------------------------------------------------------
    Output
      proc freq data=df;
        table outcome_column / binomial;
        exact binomial;
      run;
      

# translate() skips missing code

    Code
      translate(res)
    Output
      
      

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
      ! The ellipses are not used for this function. The following options were passed to them: `extra`.

