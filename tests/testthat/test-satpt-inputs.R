test_that("y as a factor produces the same result as y as character", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 200, prob = rep(0.25, 4))
  res_char <- satpt::satpt(y = d$responses1)
  res_fac <- satpt::satpt(y = factor(d$responses1))
  expect_equal(res_char$saturation, res_fac$saturation)
  expect_equal(
    as.numeric(res_char$se), as.numeric(res_fac$se),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(res_char$phat), as.numeric(res_fac$phat),
    tolerance = 1e-12
  )
  expect_equal(res_char$n, res_fac$n)
})

test_that("which_saturation is clean when y is wrapped in a function call", {
  # Regression: the prior `gsub('.*\\$', '', deparse(substitute(y)))` left a
  # trailing ')' behind for inputs like `factor(d$col)`, producing names like
  # "responses1)". The replacement extractor strips through the last `$` and
  # then keeps only the leading R identifier.
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 100, prob = c(0.5, 0.5))
  res <- satpt::satpt(y = factor(d$responses1))
  expect_equal(res$which_saturation, "responses1")
})

test_that("y as a 1-col data.frame puts column name in which_saturation", {
  set.seed(1)
  d <- data.frame(
    my_q = sample(c("yes", "no"), 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
  res <- satpt::satpt(y = d)
  expect_equal(res$which_saturation, "my_q")
})

test_that("y as an unequal-length list is padded and dispatched", {
  set.seed(1)
  d <- list(
    item_a = sample(c("yes", "no"), 100, replace = TRUE),
    item_b = sample(c("yes", "no"), 80, replace = TRUE)
  )
  res <- satpt::satpt(y = d, select_all_apply = TRUE)
  expect_s3_class(res, "satpt")
  expect_true(res$which_saturation %in% c("item_a", "item_b"))
})

test_that("which_saturation picks the column nearest p = 0.5", {
  d <- data.frame(
    item_loose = c(rep("yes", 50), rep("no", 50)),
    item_split = c(rep("yes", 80), rep("no", 20)),
    stringsAsFactors = FALSE
  )
  res <- satpt::satpt(y = d, select_all_apply = TRUE)
  expect_equal(res$which_saturation, "item_loose")
})

test_that("which_saturation can shift between items as more data arrives", {
  # Wave 1 only: item_a near 0.6, item_b at 0.5 -> item_b drives saturation.
  d_small <- data.frame(
    item_a = c(rep("yes", 60), rep("no", 40)),
    item_b = c(rep("yes", 50), rep("no", 50)),
    stringsAsFactors = FALSE
  )
  res_small <- satpt::satpt(y = d_small, select_all_apply = TRUE)
  expect_equal(res_small$which_saturation, "item_b")

  # After wave 2: item_a holds at 0.6 while item_b drifts to 0.75 ->
  # item_a is now closer to 0.5 and takes over.
  d_full <- data.frame(
    item_a = c(rep("yes", 60), rep("no", 40), rep("yes", 60), rep("no", 40)),
    item_b = c(rep("yes", 50), rep("no", 50), rep("yes", 100)),
    stringsAsFactors = FALSE
  )
  res_full <- satpt::satpt(y = d_full, select_all_apply = TRUE)
  expect_equal(res_full$which_saturation, "item_a")
})

test_that("dimnames as an unnamed length-2 vector applies y first, by second", {
  d <- example2_data()
  res <- satpt::satpt(
    y = d$responses1, by = d$period,
    dimnames = c("Response", "Wave")
  )
  expect_match(names(dimnames(res$counts))[1L], "Wave")
  expect_match(names(dimnames(res$counts))[2L], "Response")
})

test_that("dimnames as a named c(y, by) vector is order-insensitive", {
  d <- example2_data()
  res <- satpt::satpt(
    y = d$responses1, by = d$period,
    dimnames = c(by = "Wave", y = "Response")
  )
  expect_match(names(dimnames(res$counts))[1L], "Wave")
  expect_match(names(dimnames(res$counts))[2L], "Response")
})

test_that("multi-column y warns by default", {
  d <- data.frame(
    q1 = c(rep("yes", 50), rep("no", 50)),
    q2 = c(rep("yes", 50), rep("no", 50)),
    stringsAsFactors = FALSE
  )
  expect_warning(
    satpt::satpt(y = d),
    "select-all-that-apply"
  )
})

test_that("select_all_apply = TRUE silences the multi-column warning", {
  d <- data.frame(
    q1 = c(rep("yes", 50), rep("no", 50)),
    q2 = c(rep("yes", 50), rep("no", 50)),
    stringsAsFactors = FALSE
  )
  expect_silent(satpt::satpt(y = d, select_all_apply = TRUE))
})

test_that("select_all_apply = FALSE errors on multi-column y", {
  d <- data.frame(
    q1 = c(rep("yes", 50), rep("no", 50)),
    q2 = c(rep("yes", 50), rep("no", 50)),
    stringsAsFactors = FALSE
  )
  expect_error(
    satpt::satpt(y = d, select_all_apply = FALSE),
    "satpt_survey"
  )
})

test_that("single-column y is unaffected by the guard", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 100, prob = c(0.5, 0.5))
  expect_silent(satpt::satpt(y = d$responses1))
  expect_silent(satpt::satpt(y = d$responses1, select_all_apply = FALSE))
})
