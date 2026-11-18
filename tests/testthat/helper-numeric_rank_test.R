rank_test_data <- function() {
  data.frame(
    y = c(1.2, 3.4, 2.2, 5.1, 4.4, 6.0, 8.1, 7.3, 2.9, NA, 5.5, 3.6),
    g = factor(rep(c("a", "b"), 6)),
    s = rep(c("s1", "s2", "s3"), each = 4),
    k = rep(c("x", "y", "z"), 4)
  )
}

# van Elteren Z computed by hand: within-stratum rank sums for the first group
# level, weighted by 1 / (n_k + 1).
van_elteren_z <- function(y, g, s) {
  parts <- lapply(split(seq_along(y), s), function(i) {
    r <- rank(y[i])
    n <- length(i)
    in_first <- g[i] == levels(g)[1]
    n1 <- sum(in_first)
    w <- 1 / (n + 1)
    c(
      linear = w * sum(r[in_first]),
      expected = w * n1 * (n + 1) / 2,
      variance = w^2 * n1 * (n - n1) / (n * (n - 1)) * sum((r - mean(r))^2)
    )
  })
  totals <- Reduce(`+`, parts)
  unname((totals["linear"] - totals["expected"]) / sqrt(totals["variance"]))
}
