# new_segue() validates inputs

    Code
      new_segue(code = 1)
    Condition
      Error:
      ! The `code` argument should be a list.

---

    Code
      new_segue(code = list(r = NULL))
    Condition
      Error:
      ! The `code` argument should be a list with elements named "r" and "sas".

---

    Code
      new_segue(code = list(r = 1, sas = "run;"))
    Condition
      Error:
      ! The "r" element of `code` should be a call or NULL, not a number.

---

    Code
      new_segue(code = list(r = quote(f(x)), sas = 1))
    Condition
      Error in `new_segue()`:
      ! `code$sas` must be a character vector or `NULL`, not the number 1.

---

    Code
      new_segue(code = cls, options = 1)
    Condition
      Error:
      ! The `options` argument should be a list, not a number.

---

    Code
      new_segue(code = cls, results = 1)
    Condition
      Error:
      ! The `results` argument should be a list, not a number.

---

    Code
      new_segue(code = cls, data_characteristics = 1)
    Condition
      Error:
      ! The `data_characteristics` argument should be a list, not a number.

