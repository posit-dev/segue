# check_factor() validates factors

    Code
      check_factor(letters)
    Condition
      Error:
      ! Argument `y` should be a <factor>, not a character vector.

---

    Code
      check_factor(factor("a"))
    Condition
      Error:
      ! Argument `y` should be a <factor> with at least two levels.

---

    Code
      check_factor(1:2, nm = "outcome")
    Condition
      Error:
      ! Argument `outcome` should be a <factor>, not an integer vector.

# check_level() validates levels

    Code
      check_level(c("a", "b"), y)
    Condition
      Error:
      ! Argument `event_level` should be a single character value, not a character vector.

---

    Code
      check_level(NA_character_, y)
    Condition
      Error:
      ! Argument `event_level` should be a single character value, not a character `NA`.

---

    Code
      check_level("c", y)
    Condition
      Error:
      ! Argument `event_level` must be one of "b" and "a".

# check_empty_ellipse() catches extra arguments

    Code
      check_empty_ellipse(a = 1, b = 2)
    Condition
      Error:
      ! The ellipses are not used for this function. The following options were passed to them: `a` and `b`.

