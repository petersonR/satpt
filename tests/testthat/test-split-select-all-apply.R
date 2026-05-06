test_that("split_select_all_apply produces one column per unique token", {
  x <- c("a|b", "b|c", "a", "c")
  out <- satpt::split_select_all_apply(x, sep = "|")
  expect_s3_class(out, "data.frame")
  expect_setequal(colnames(out), c("a", "b", "c"))
  expect_equal(nrow(out), 4L)
})

test_that("split_select_all_apply maps presence/absence to Yes/No", {
  x <- c("a|b", "b", "a")
  out <- satpt::split_select_all_apply(x, sep = "|")
  expect_equal(out$a, c("Yes", "No", "Yes"))
  expect_equal(out$b, c("Yes", "Yes", "No"))
})

test_that("split_select_all_apply keeps NA rows as NA in every column", {
  x <- c("a|b", NA, "b")
  out <- satpt::split_select_all_apply(x, sep = "|")
  expect_true(all(is.na(out[2L, ])))
  expect_equal(out$a, c("Yes", NA, "No"))
})

test_that("split_select_all_apply treats empty/whitespace entries as NA", {
  x <- c("a", "", "   ", "b")
  out <- satpt::split_select_all_apply(x, sep = "|")
  expect_true(all(is.na(out[2L, ])))
  expect_true(all(is.na(out[3L, ])))
})

test_that("split_select_all_apply trims whitespace around tokens", {
  x <- c("a | b", "  b  | c ")
  out <- satpt::split_select_all_apply(x, sep = "|")
  expect_setequal(colnames(out), c("a", "b", "c"))
  expect_equal(out$a, c("Yes", "No"))
  expect_equal(out$b, c("Yes", "Yes"))
})

test_that("split_select_all_apply respects an explicit values vector", {
  x <- c("a", "b", "a|b")
  out <- satpt::split_select_all_apply(
    x,
    sep = "|",
    values = c("a", "b", "c")
  )
  # 'c' never appears in x but is still a column.
  expect_setequal(colnames(out), c("a", "b", "c"))
  expect_equal(out$c, c("No", "No", "No"))
})

test_that("split_select_all_apply accepts factor input", {
  f <- factor(c("a|b", "b", "a"))
  out <- satpt::split_select_all_apply(f, sep = "|")
  expect_setequal(colnames(out), c("a", "b"))
  expect_equal(out$a, c("Yes", "No", "Yes"))
})

test_that("split_select_all_apply works with non-pipe separators", {
  out <- satpt::split_select_all_apply(c("a,b", "b,c"), sep = ",")
  expect_setequal(colnames(out), c("a", "b", "c"))
})

test_that("split_select_all_apply rejects bad inputs", {
  expect_error(satpt::split_select_all_apply(1:3), "character or factor")
  expect_error(
    satpt::split_select_all_apply(c("a"), sep = ""),
    "non-empty"
  )
  expect_error(
    satpt::split_select_all_apply(c("a"), sep = c("|", ",")),
    "single"
  )
  expect_error(
    satpt::split_select_all_apply(c("a"), values = 1L),
    "values"
  )
})

test_that("split_select_all_apply on diagnoses$q1 gives the 5 tokens", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  q1 <- satpt::split_select_all_apply(e$diagnoses$q1, sep = "|")
  expect_setequal(
    colnames(q1),
    c("Broadrange", "Wholegenome", "MNGS", "None", "Other")
  )
  expect_equal(nrow(q1), length(e$diagnoses$q1))
})

test_that("split output feeds straight into satpt(select_all_apply=TRUE)", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  q1 <- satpt::split_select_all_apply(e$diagnoses$q1, sep = "|")
  res <- satpt::satpt(y = q1, by = e$diagnoses$wave, select_all_apply = TRUE)
  expect_s3_class(res, "satpt")
  expect_true(res$saturation)
  # The limiting item is the column with sample proportion closest to 0.5.
  expect_true(res$limiting_item %in% colnames(q1))
})
