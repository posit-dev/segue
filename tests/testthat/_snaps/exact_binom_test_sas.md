# the result has the same elements as binom.test()

    Code
      res
    Output
      
      	Exact binomial test (SAS Proc Freq)
      
      data:  7 and 23
      number of successes = 7, number of trials = 23, p-value = 0.01155
      alternative hypothesis: true probability of success is not equal to 0.1
      sample estimates:
      probability of success 
                   0.3043478 
      

# bad input gives informative errors

    Code
      exact_binom_test_sas(2.5, 10)
    Condition
      Error in `exact_binom_test_sas()`:
      ! `x` must contain non-negative whole numbers.
    Code
      exact_binom_test_sas(-1, 10)
    Condition
      Error in `exact_binom_test_sas()`:
      ! `x` must contain non-negative whole numbers.
    Code
      exact_binom_test_sas(NA, 10)
    Condition
      Error in `exact_binom_test_sas()`:
      ! `x` must contain non-negative whole numbers.
    Code
      exact_binom_test_sas("a", 10)
    Condition
      Error in `exact_binom_test_sas()`:
      ! `x` must contain non-negative whole numbers.
    Code
      exact_binom_test_sas(c(1, 2, 3))
    Condition
      Error in `exact_binom_test_sas()`:
      ! `x` must have one or two values, not 3.
    Code
      exact_binom_test_sas(11, 10)
    Condition
      Error in `exact_binom_test_sas()`:
      ! `x` must be no larger than `n` (10), not 11.
    Code
      exact_binom_test_sas(1, 0)
    Condition
      Error in `exact_binom_test_sas()`:
      ! `n` must be a whole number larger than or equal to 1, not the number 0.
    Code
      exact_binom_test_sas(1, 10, p = 1.5)
    Condition
      Error in `exact_binom_test_sas()`:
      ! `p` must be a number between 0 and 1, not the number 1.5.
    Code
      exact_binom_test_sas(1, 10, p = c(0.1, 0.2))
    Condition
      Error in `exact_binom_test_sas()`:
      ! `p` must be a number, not a double vector.
    Code
      exact_binom_test_sas(1, 10, alternative = "lesser")
    Condition
      Error in `exact_binom_test_sas()`:
      ! `alternative` must be one of "two.sided", "less", or "greater", not "lesser".
      i Did you mean "less"?

