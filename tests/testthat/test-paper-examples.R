# Reproductions of the three simulated examples from Section 3.1 of
# Boonstra, Cavanaugh, Miller and Polgreen (2025). The published draft
# omits the exact generating probabilities; helper-examples.R uses
# illustrative values that preserve the qualitative behaviour the paper
# reports for each example.

test_that("Example 1: a single wave of N=350 reaches saturation on its own", {
  res <- satpt::satpt(y = example1_data()$responses1)
  expect_true(res$saturation)
  expect_null(res$test)
  expect_null(res$hindex)
  expect_lt(max(res$se), res$threshold)
})

test_that("Example 2: two equal waves without bias use unpooled SE", {
  d <- example2_data()
  res <- satpt::satpt(y = d$responses1, by = d$period)
  expect_true(res$saturation)
  expect_false(res$pooled_se)
  expect_match(res$test$method, "Pearson", ignore.case = TRUE)
  # Heterogeneity index is small when the two waves come from one DGP.
  expect_lt(max(res$hindex), 0.05)
})

test_that("Example 3: two equal waves with bias use pooled SE", {
  d <- example3_data()
  res <- satpt::satpt(y = d$responses1, by = d$period)
  expect_true(res$pooled_se)
  expect_lt(res$test$p.value, 0.05)
  # The deliberately disparate waves produce a much larger hindex.
  expect_gt(max(res$hindex), 0.10)
})

test_that("threshold of 0.025 maps to a 95% CI half-width near 0.05", {
  # Section 2.2: threshold = 0.025 -> 95% CI of width ~0.10 (half-width ~0.05).
  # The paper rounds 1.96 to 2 in deriving the 0.025 threshold.
  half_width <- qnorm(0.975) * 0.025
  expect_lt(abs(half_width - 0.05), 0.005)
})
