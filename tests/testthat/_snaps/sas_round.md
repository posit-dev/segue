# sas_round() validates inputs

    Code
      sas_round("a")
    Condition
      Error in `sas_round()`:
      ! `x` should be a numeric vector, not a string.

---

    Code
      sas_round(1, unit = 0)
    Condition
      Error in `sas_round()`:
      ! `unit` should be a positive number, not 0.

---

    Code
      sas_round(1, unit = -1)
    Condition
      Error in `sas_round()`:
      ! `unit` should be a positive number, not -1.

---

    Code
      sas_round(1, unit = c(1, 2))
    Condition
      Error in `sas_round()`:
      ! `unit` must be a number, not a double vector.

---

    Code
      sas_round(1, unit = Inf)
    Condition
      Error in `sas_round()`:
      ! `unit` must be a number, not `Inf`.

