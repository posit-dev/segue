# new_segue() validates inputs

    Code
      new_segue(calls = 1)
    Condition
      Error:
      ! The `calls` argument should be a list.

---

    Code
      new_segue(calls = list(r = NULL))
    Condition
      Error:
      ! The `calls` argument should be a list with elements named "r" and "sas".

---

    Code
      new_segue(calls = list(r = 1, sas = "run;"))
    Condition
      Error:
      ! The "r" element of `calls` should be a call or NULL, not a number.

---

    Code
      new_segue(calls = list(r = quote(f(x)), sas = 1))
    Condition
      Error in `new_segue()`:
      ! `calls$sas` must be a character vector or `NULL`, not the number 1.

---

    Code
      new_segue(calls = cls, options = 1)
    Condition
      Error:
      ! The `options` argument should be a list, not a number.

---

    Code
      new_segue(calls = cls, results = 1)
    Condition
      Error:
      ! The `results` argument should be a list, not a number.

---

    Code
      new_segue(calls = cls, data = 1)
    Condition
      Error:
      ! The `data` argument should be a list, not a number.

