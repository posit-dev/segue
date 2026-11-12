# check_factor() validates factors

    Code
      check_factor(letters)
    Condition
      Error:
      ! `letters` must be a factor, not a character vector.

---

    Code
      check_factor(factor("a"))
    Condition
      Error:
      ! Argument `factor("a")` should be a <factor> with at least two levels.

---

    Code
      check_factor(1:2, arg = "outcome")
    Condition
      Error:
      ! `outcome` must be a factor, not an integer vector.

# check_level() validates levels

    Code
      check_level(c("a", "b"), y)
    Condition
      Error:
      ! `c("a", "b")` must be a single character value, not a character vector.

---

    Code
      check_level(NA_character_, y)
    Condition
      Error:
      ! `NA_character_` must be a single character value, not a character `NA`.

---

    Code
      check_level("c", y)
    Condition
      Error:
      ! Argument `"c"` must be one of "b" and "a".

