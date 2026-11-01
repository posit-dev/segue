# convert_alternative() validates inputs

    Code
      convert_alternative("less")
    Condition
      Error:
      ! `alternative` must be one of "two_sided", "lesser", or "greater", not "less".
      i Did you mean "lesser"?

---

    Code
      convert_alternative("two.sided")
    Condition
      Error:
      ! `alternative` must be one of "two_sided", "lesser", or "greater", not "two.sided".
      i Did you mean "two_sided"?

