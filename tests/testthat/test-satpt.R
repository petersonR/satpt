test_that("satpt without by leaves test, hindex, and pooled_se NULL", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 200, prob = rep(0.25, 4))
  res <- satpt::satpt(y = d$responses1)
  expect_s3_class(res, "satpt")
  expect_null(res$test)
  expect_null(res$hindex)
  expect_null(res$pooled_se)
})

test_that("satpt without by has a single-row phat that sums to 1", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 200, prob = rep(0.25, 4))
  res <- satpt::satpt(y = d$responses1)
  expect_equal(nrow(res$phat), 1L)
  expect_equal(sum(res$phat), 1)
})

test_that("standard errors equal sqrt(p*(1-p)/n) per category", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 400, prob = rep(0.25, 4))
  res <- satpt::satpt(y = d$responses1)
  p <- as.vector(res$phat)
  expected_se <- sqrt(p * (1 - p) / res$n)
  expect_equal(as.vector(res$se), expected_se, tolerance = 1e-12)
})

test_that("counts column sum equals the reported sample size n", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 200, prob = rep(0.25, 4))
  res <- satpt::satpt(y = d$responses1)
  expect_equal(sum(res$counts), res$n)
})

test_that("which_saturation echoes the user-supplied y variable name", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 100, prob = c(0.5, 0.5))
  res <- satpt::satpt(y = d$responses1)
  expect_equal(res$which_saturation, "responses1")
})

test_that("loosening the threshold flips a non-saturated case to saturated", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 100, prob = c(0.5, 0.5))
  # With N=100 at p=0.5, SE = sqrt(0.25/100) = 0.05, well above 0.025.
  res_strict <- satpt::satpt(y = d$responses1)
  res_loose <- satpt::satpt(y = d$responses1, threshold = 0.06)
  expect_false(res_strict$saturation)
  expect_true(res_loose$saturation)
})

test_that("two waves with identical generating probabilities show no bias", {
  d <- example2_data()
  res <- satpt::satpt(y = d$responses1, by = d$period)
  expect_false(res$pooled_se)
  expect_gt(res$test$p.value, 0.05)
  # With no bias, overall SE follows the unpooled binomial formula.
  expect_equal(
    res$total$se,
    sqrt(res$total$phat * (1 - res$total$phat) / res$n),
    tolerance = 1e-12
  )
})

test_that("two waves with disparate probabilities trigger pooled SE", {
  d <- example3_data()
  res <- satpt::satpt(y = d$responses1, by = d$period)
  expect_true(res$pooled_se)
  expect_lt(res$test$p.value, 0.05)
  # Pooled overall SE = sqrt(sum(w_i^2 * SE_i^2)).
  weights <- rowSums(res$counts) / sum(rowSums(res$counts))
  expected <- vapply(
    seq_len(ncol(res$se)),
    function(j) sqrt(sum(weights^2 * res$se[, j]^2)),
    numeric(1)
  )
  expect_equal(res$total$se, expected, tolerance = 1e-12)
})

test_that("heterogeneity index is an order of magnitude larger under bias", {
  res_clean <- satpt::satpt(
    y = example2_data()$responses1,
    by = example2_data()$period
  )
  res_biased <- satpt::satpt(
    y = example3_data()$responses1,
    by = example3_data()$period
  )
  expect_gt(max(res_biased$hindex), 10 * max(res_clean$hindex))
})

test_that("Fisher's exact test fires when expected counts are sparse", {
  d <- data.frame(
    y = c("a", "a", "b", "b", "c", "c"),
    by = c("w1", "w2", "w1", "w2", "w1", "w2"),
    stringsAsFactors = FALSE
  )
  set.seed(1)
  res <- suppressWarnings(satpt::satpt(y = d$y, by = d$by))
  expect_match(res$test$method, "Fisher", ignore.case = TRUE)
})

test_that("Pearson's chi-squared test fires when expected counts are dense", {
  d <- example2_data()
  res <- satpt::satpt(y = d$responses1, by = d$period)
  expect_match(res$test$method, "Pearson", ignore.case = TRUE)
})

test_that("extra ... arguments are forwarded to the independence test", {
  # Regression: prior code attempted `new_test_args[[-w_args]]` (double-
  # bracket negative index) when `x` or `y` appeared in `...`, which errors
  # in R for any negative index. The replacement uses single-bracket
  # `setdiff` filtering. Verify forwarding by passing simulate.p.value to
  # chisq.test and observing the changed method label.
  d <- example2_data()
  set.seed(1)
  res <- satpt::satpt(
    y = d$responses1, by = d$period, simulate.p.value = TRUE
  )
  expect_match(res$test$method, "simulated p-value")
})

test_that("explicit x in ... is dropped, not error", {
  # Regression for the `[[ -w_args ]]` bug specifically: any negative
  # index in `[[` errors in base R, so the prior code crashed whenever a
  # caller passed `x = ...` via `...`. Now the helper drops `x` and `y`
  # via setdiff before forwarding.
  d <- example2_data()
  expect_silent(
    satpt::satpt(y = d$responses1, by = d$period, x = "ignored")
  )
})
