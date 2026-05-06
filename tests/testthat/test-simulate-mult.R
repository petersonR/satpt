test_that("simulate returns the documented shape for a single wave", {
  set.seed(1)
  d <- simulate_mult(n = 1, size = 50, prob = c(0.5, 0.5))
  expect_s3_class(d, "data.frame")
  expect_equal(nrow(d), 50L)
  expect_named(d, c("period", "responses1"))
  expect_true(all(d$period == 1L))
})

test_that("simulate stacks waves with sequential period indicators", {
  set.seed(1)
  d <- simulate_mult(n = 1, size = c(20, 30), prob = rep(0.5, 4))
  expect_equal(nrow(d), 50L)
  expect_equal(sum(d$period == 1L), 20L)
  expect_equal(sum(d$period == 2L), 30L)
})

test_that("simulate honors user-supplied category labels", {
  set.seed(1)
  d <- simulate_mult(
    n = 1, size = 30,
    prob = c(0.5, 0.5),
    categories = c("yes", "no")
  )
  expect_setequal(unique(d$responses1), c("yes", "no"))
})

test_that("simulate emits multiple response columns when n > 1", {
  set.seed(1)
  d <- simulate_mult(n = 3, size = 20, prob = c(0.5, 0.5))
  expect_named(d, c("period", "responses1", "responses2", "responses3"))
})

test_that("simulate matrix prob with one row per wave is honored", {
  set.seed(1)
  prob <- matrix(c(0.8, 0.2, 0.2, 0.8), nrow = 2L, byrow = TRUE)
  d <- simulate_mult(
    n = 1, size = c(500, 500),
    prob = prob, categories = c("hit", "miss")
  )
  prop_w1 <- mean(d$responses1[d$period == 1L] == "hit")
  prop_w2 <- mean(d$responses1[d$period == 2L] == "hit")
  expect_gt(prop_w1, 0.7)
  expect_lt(prop_w2, 0.3)
})

test_that("simulate rejects probabilities that do not sum to 1", {
  expect_error(
    simulate_mult(n = 1, size = 10, prob = c(0.3, 0.3)),
    "sum to one"
  )
})

test_that("simulate rejects mismatched category vector length", {
  expect_error(
    simulate_mult(
      n = 1, size = 10,
      prob = c(0.5, 0.5),
      categories = c("a", "b", "c")
    ),
    "categories"
  )
})

test_that("simulate accepts probabilities that sum to 1 within FP tolerance", {
  # Regression: c(0.4, 0.3, 0.1, 0.1, 0.1) does not sum to exactly 1 in
  # double-precision arithmetic. The earlier strict `==` check rejected
  # otherwise valid inputs; the rewritten check uses all.equal().
  set.seed(1)
  expect_silent(
    simulate_mult(n = 1, size = 50, prob = c(0.4, 0.3, 0.1, 0.1, 0.1))
  )
})

test_that("simulate rejects negative probabilities", {
  # Regression: the prior `!(all(prob >= 0) || all(prob <= 1))` short-circuits
  # to FALSE when any single clause is TRUE, so negative probs slipped past
  # validation as long as every value was <= 1.
  expect_error(
    simulate_mult(n = 1, size = 10, prob = c(-0.1, 0.6, 0.5)),
    "between 0 and 1"
  )
})

test_that("simulate rejects size when any element is non-integer", {
  # Regression: prior `all(size %% floor(size) != 0)` only complained when
  # *every* size was non-integer. Now any non-integer entry triggers the
  # error.
  expect_error(
    simulate_mult(n = 1, size = c(50, 50.5), prob = c(0.5, 0.5)),
    "integer"
  )
})
