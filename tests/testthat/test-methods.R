test_that("summary.satpt always exposes the 10 documented slots", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 200, prob = rep(0.25, 4))
  res <- satpt::satpt(y = d$responses1)
  s <- summary(res)
  expect_s3_class(s, "summary.satpt")
  expect_equal(s$saturation, ifelse(res$saturation, "Yes", "No"))
  expect_equal(s$threshold, res$threshold)
  expect_equal(s$n, res$n)
  expect_setequal(
    names(s),
    c(
      "threshold", "saturation", "which_saturation", "n",
      "phat", "se", "pooled_se", "alpha", "test", "hindex"
    )
  )
  # Slots that are not meaningful without `by` are kept but set to NULL.
  expect_null(s$test)
  expect_null(s$hindex)
  expect_null(s$pooled_se)
})

test_that("summary.satpt with by appends an Overall row to phat and se", {
  d <- example2_data()
  res <- satpt::satpt(y = d$responses1, by = d$period)
  s <- summary(res)
  expect_true("Overall" %in% rownames(s$phat))
  expect_true("Overall" %in% rownames(s$se))
  expect_s3_class(s$hindex, "data.frame")
  expect_named(s$hindex, c("categories", "index"))
})

test_that("summary.satpt rejects non-satpt input", {
  expect_error(summary.satpt(list()), "satpt")
})

test_that("print.satpt reports saturation status", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 200, prob = rep(0.25, 4))
  res <- satpt::satpt(y = d$responses1)
  out <- capture.output(print(res))
  expect_true(any(grepl("Saturation achieved", out)))
})

test_that("print.satpt headline reports CI half-width when saturated", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 400, prob = c(0.5, 0.5))
  res <- satpt::satpt(y = d$responses1)
  out <- capture.output(print(res))
  expect_true(any(grepl("Saturation achieved for", out)))
  expect_true(any(grepl("percentage points", out)))
  expect_true(any(grepl("within the", out)))
})

test_that("print.satpt headline reports n_to_saturation when not saturated", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 100, prob = c(0.5, 0.5))
  res <- satpt::satpt(y = d$responses1)
  expect_gt(res$n_to_saturation, 0L)
  out <- capture.output(print(res))
  expect_true(any(grepl("Saturation not yet achieved", out)))
  expect_true(any(grepl(
    paste0("About ", res$n_to_saturation, " more responses needed"),
    out
  )))
})

test_that("print.summary.satpt prints hindex when by is supplied", {
  d <- example3_data()
  res <- satpt::satpt(y = d$responses1, by = d$period)
  s <- summary(res)
  out <- capture.output(print(s))
  expect_true(any(grepl("Heterogeneity index", out)))
})

test_that("print.satpt rejects non-satpt input", {
  expect_error(print.satpt(list()), "satpt")
})

test_that("plot.satpt draws without error on a valid satpt object", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 200, prob = rep(0.25, 4))
  res <- satpt::satpt(y = d$responses1)
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)
  expect_silent(plot(res))
})

test_that("plot.satpt rejects non-satpt input", {
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)
  expect_error(plot.satpt(list()), "satpt")
})
