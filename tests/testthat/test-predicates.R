test_that("is_saturated returns TRUE when saturation has been achieved", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 400, prob = c(0.5, 0.5))
  res <- satpt::satpt(y = d$responses1)
  expect_true(satpt::is_saturated(res))
  expect_equal(satpt::is_saturated(res), res$saturation)
})

test_that("is_saturated returns FALSE when saturation has not been achieved", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 100, prob = c(0.5, 0.5))
  res <- satpt::satpt(y = d$responses1)
  expect_false(satpt::is_saturated(res))
  expect_equal(satpt::is_saturated(res), res$saturation)
})

test_that("is_saturated rejects non-satpt input", {
  expect_error(satpt::is_saturated(list()), "satpt")
  expect_error(satpt::is_saturated(NULL), "satpt")
})

test_that("limiting_item mirrors which_saturation", {
  set.seed(1)
  # Single-question case.
  d <- satpt::simulate(n = 1, size = 100, prob = c(0.5, 0.5))
  res <- satpt::satpt(y = d$responses1)
  expect_equal(res$limiting_item, res$which_saturation)
  expect_equal(res$limiting_item, "responses1")

  # Select-all-apply case (multi-column y).
  d_saa <- data.frame(
    item_a = c(rep("yes", 60), rep("no", 40)),
    item_b = c(rep("yes", 50), rep("no", 50)),
    stringsAsFactors = FALSE
  )
  res_saa <- satpt::satpt(y = d_saa, select_all_apply = TRUE)
  expect_equal(res_saa$limiting_item, res_saa$which_saturation)
})
