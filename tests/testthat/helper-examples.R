# Reproductions of the three simulated examples from Section 3.1 of the
# Boonstra et al. (2025) manuscript. The probability vectors below preserve
# the qualitative behaviour described in the paper (Example 1 saturates on
# wave 1; Example 2 has no response bias; Example 3 has strong response
# bias).

example1_data <- function(seed = 123) {
  set.seed(seed)
  simulate_mult(
    n = 1,
    size = 350,
    prob = rep(0.2, 5),
    categories = LETTERS[1:5]
  )
}

example2_data <- function(seed = 123) {
  set.seed(seed)
  simulate_mult(
    n = 1,
    size = c(175, 175),
    prob = rep(0.2, 10),
    categories = LETTERS[1:5]
  )
}

example3_prob <- function() {
  matrix(
    c(
      0.40, 0.30, 0.20, 0.05, 0.05,
      0.05, 0.05, 0.20, 0.30, 0.40
    ),
    nrow = 2, byrow = TRUE
  )
}

example3_data <- function(seed = 123) {
  set.seed(seed)
  simulate_mult(
    n = 1,
    size = c(175, 175),
    prob = example3_prob(),
    categories = LETTERS[1:5]
  )
}
