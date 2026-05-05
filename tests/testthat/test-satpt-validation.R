test_that("alpha must lie strictly inside (0, 1)", {
  y <- c("a", "b", "a", "b")
  expect_error(satpt::satpt(y = y, alpha = 0), "alpha")
  expect_error(satpt::satpt(y = y, alpha = 1), "alpha")
  expect_error(satpt::satpt(y = y, alpha = "0.05"), "alpha")
})

test_that("threshold must lie strictly inside (0, 0.25)", {
  y <- c("a", "b", "a", "b")
  expect_error(satpt::satpt(y = y, threshold = 0), "threshold")
  expect_error(satpt::satpt(y = y, threshold = 0.25), "threshold")
  expect_error(satpt::satpt(y = y, threshold = -0.01), "threshold")
})

test_that("y and by must have matching observation counts", {
  expect_error(
    satpt::satpt(y = letters[1:5], by = letters[1:3]),
    "same number of observations"
  )
})

test_that("dimnames must have length 2 when by is supplied", {
  d <- example2_data()
  expect_error(
    satpt::satpt(y = d$responses1, by = d$period, dimnames = "only-one"),
    "length two"
  )
})

test_that("named dimnames must contain both 'y' and 'by'", {
  d <- example2_data()
  expect_error(
    satpt::satpt(
      y = d$responses1, by = d$period,
      dimnames = c(y = "Response", wave = "Wave")
    ),
    "y.*by"
  )
})

test_that("dimnames must be a character vector", {
  set.seed(1)
  d <- satpt::simulate(n = 1, size = 50, prob = c(0.5, 0.5))
  expect_error(
    satpt::satpt(y = d$responses1, dimnames = 1L),
    "character"
  )
})

test_that("y of an unsupported class is rejected with a list of valid types", {
  expect_error(
    satpt::satpt(y = function(x) x),
    "vector, matrix"
  )
})
