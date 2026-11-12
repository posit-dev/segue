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

---

    Code
      sas_round(1, unit = .Machine$double.xmin)
    Condition
      Error in `sas_round()`:
      ! `unit` should be larger than `.Machine$double.xmin` (2.23e-308), not 2.2250738585072e-308.

---

    Code
      sas_round(1, unit = .Machine$double.xmin / 2)
    Condition
      Error in `sas_round()`:
      ! `unit` should be larger than `.Machine$double.xmin` (2.23e-308), not 1.1125369292536e-308.

---

    Code
      sas_round(1, digits = 1.5)
    Condition
      Error in `sas_round()`:
      ! `digits` must be a whole number, not the number 1.5.

---

    Code
      sas_round(1, digits = 16)
    Condition
      Error in `sas_round()`:
      ! `digits` must be a whole number between -15 and 15, not the number 16.

---

    Code
      sas_round(1, digits = 2, unit = 0.01)
    Condition
      Error in `sas_round()`:
      ! Supply only one of `digits` and `unit`.

# sas_format() validates inputs

    Code
      sas_format("a")
    Condition
      Error in `sas_format()`:
      ! `x` should be a numeric vector, not a string.

---

    Code
      sas_format(1, digits = 1.5)
    Condition
      Error in `sas_format()`:
      ! `digits` must be a whole number, not the number 1.5.

---

    Code
      sas_format(1, digits = 16)
    Condition
      Error in `sas_format()`:
      ! `digits` must be a whole number between 0 and 15, not the number 16.

---

    Code
      sas_format(1, digits = -1)
    Condition
      Error in `sas_format()`:
      ! `digits` must be a whole number between 0 and 15, not the number -1.

---

    Code
      sas_format(1, digits = 3, nsmall = 2)
    Condition
      Error in `sas_format()`:
      ! `nsmall` is set by `sas_format()` and cannot be passed to `format()`.

---

    Code
      sas_format(1, drop0trailing = TRUE)
    Condition
      Error in `sas_format()`:
      ! `sas_format()` does not use `drop0trailing`. It can use `big.mark`, `big.interval`, `small.mark`, `small.interval`, `decimal.mark`, `width`, `justify`, `trim`, and `scientific`.

