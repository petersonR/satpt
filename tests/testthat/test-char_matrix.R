test_that("char_matrix.default coerces atomic input to a character matrix", {
  out <- char_matrix(1:3)
  expect_true(is.matrix(out))
  expect_type(out, "character")
  expect_equal(dim(out), c(3L, 1L))
  expect_equal(as.vector(out), c("1", "2", "3"))
  expect_null(colnames(out))
})

test_that("char_matrix.default applies cname when length 1", {
  out <- char_matrix(1:3, cname = "x")
  expect_equal(colnames(out), "x")
})

test_that("char_matrix.factor uses labels and ignores level order", {
  f <- factor(c("b", "a", "c"), levels = c("c", "b", "a"))
  out <- char_matrix(f)
  expect_equal(as.vector(out), c("b", "a", "c"))
  expect_type(out, "character")
})

test_that("char_matrix.list pads shorter elements with NA up to the longest", {
  out <- char_matrix(list(a = 1:3, b = 1:2))
  expect_equal(dim(out), c(3L, 2L))
  expect_true(is.na(out[3L, "b"]))
  expect_equal(colnames(out), c("a", "b"))
})

test_that("char_matrix.list synthesizes V1, V2... for unnamed lists", {
  out <- char_matrix(list(1:2, 3:4))
  expect_equal(colnames(out), c("V1", "V2"))
})

test_that("char_matrix.data.frame uses existing colnames by default", {
  d <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  out <- char_matrix(d)
  expect_equal(colnames(out), c("a", "b"))
  expect_equal(out[, "a"], c("1", "2"))
})

test_that("char_matrix.data.frame applies cname when length matches ncol", {
  d <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  out <- char_matrix(d, cname = c("foo", "bar"))
  expect_equal(colnames(out), c("foo", "bar"))
})

test_that("char_matrix.matrix preserves shape and converts to character", {
  m <- matrix(1:6, nrow = 2L)
  out <- char_matrix(m)
  expect_equal(dim(out), c(2L, 3L))
  expect_type(out, "character")
})

test_that("char_matrix dispatches data.table through the data.frame method", {
  skip_if_not_installed("data.table")
  d <- data.table::data.table(a = 1:2, b = c("x", "y"))
  out <- char_matrix(d)
  expect_equal(colnames(out), c("a", "b"))
  expect_type(out, "character")
})

test_that("char_matrix errors on cname whose length doesn't match ncol", {
  # Regression: methods previously fell back to existing colnames or
  # synthesized names, silently ignoring a wrongly sized cname.
  expect_error(char_matrix(1:3, cname = c("a", "b")), "length")
  expect_error(
    char_matrix(data.frame(a = 1:2, b = 1:2), cname = "only-one"),
    "length"
  )
  expect_error(
    char_matrix(list(a = 1:2, b = 1:2), cname = c("x", "y", "z")),
    "length"
  )
  expect_error(
    char_matrix(matrix(1:6, nrow = 2L), cname = c("only", "two")),
    "length"
  )
})
