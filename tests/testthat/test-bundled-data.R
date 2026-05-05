test_that("the diagnoses data set loads with the documented top-level names", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  expect_setequal(names(e$diagnoses), c("wave", "q1", "q2"))
  expect_length(e$diagnoses$q2, 643L)
})

test_that("satpt reproduces the README result for diagnoses$q2", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res <- satpt::satpt(y = e$diagnoses$q2, by = e$diagnoses$wave)
  expect_true(res$saturation)
  expect_setequal(
    res$total$categories,
    c("Not at all", "Often", "Once", "Rarely", "Sometimes")
  )
  # README: Rarely proportion shown as 0.3688.
  rarely <- res$total$phat[res$total$categories == "Rarely"]
  expect_equal(rarely, 0.3688, tolerance = 1e-3)
})

test_that("the bacteremia data set loads with the documented length", {
  e <- new.env()
  data(bacteremia, package = "satpt", envir = e)
  expect_true("wave" %in% names(e$bacteremia))
  expect_length(e$bacteremia$q1, 669L)
})
