test_that("satpt_survey returns a satpt_survey object with summary + results", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res <- satpt::satpt_survey(e$diagnoses, by = "wave")
  expect_s3_class(res, "satpt_survey")
  expect_named(res, c("summary", "results"))
  expect_s3_class(res$summary, "data.frame")
  expect_named(
    res$summary,
    c("question", "n", "max_se", "saturation", "n_to_saturation")
  )
})

test_that("satpt_survey runs every column other than `by` by default", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res <- satpt::satpt_survey(e$diagnoses, by = "wave")
  expect_setequal(res$summary$question, c("q1", "q2"))
  expect_equal(nrow(res$summary), 2L)
})

test_that("satpt_survey respects the `questions` subset", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res <- satpt::satpt_survey(e$diagnoses, by = "wave", questions = "q2")
  expect_equal(res$summary$question, "q2")
  expect_named(res$results, "q2")
})

test_that("satpt_survey accepts an external `by` vector", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res_a <- satpt::satpt_survey(e$diagnoses, by = "wave", questions = "q2")
  res_b <- satpt::satpt_survey(
    e$diagnoses[c("q1", "q2")],
    by = e$diagnoses$wave, questions = "q2"
  )
  expect_equal(res_a$summary$saturation, res_b$summary$saturation)
  expect_equal(res_a$summary$max_se, res_b$summary$max_se)
})

test_that("satpt_survey results match per-question satpt() calls", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res_survey <- satpt::satpt_survey(e$diagnoses, by = "wave")
  res_q2 <- satpt::satpt(y = e$diagnoses$q2, by = e$diagnoses$wave)
  expect_equal(res_survey$results$q2$n, res_q2$n)
  expect_equal(res_survey$results$q2$saturation, res_q2$saturation)
  expect_equal(
    max(res_survey$results$q2$total$se),
    max(res_q2$total$se)
  )
})

test_that("satpt_survey forwards `...` to satpt() (e.g. threshold)", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  # Tightening threshold below the actual max SE should flip saturation off
  # for every question.
  res <- satpt::satpt_survey(e$diagnoses, by = "wave", threshold = 0.001)
  expect_true(all(!res$summary$saturation))
  expect_true(all(res$summary$n_to_saturation > 0L))
})

test_that("satpt_survey errors on a missing question name", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  expect_error(
    satpt::satpt_survey(e$diagnoses, by = "wave", questions = "q42"),
    "Column"
  )
})

test_that("satpt_survey errors on a missing by column name", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  expect_error(
    satpt::satpt_survey(e$diagnoses, by = "missing_col"),
    "by"
  )
})

test_that("satpt_survey rejects non-data.frame, non-list inputs", {
  expect_error(
    satpt::satpt_survey(1:10),
    "data.frame or named list"
  )
})

test_that("satpt_survey rejects unnamed list elements", {
  expect_error(
    satpt::satpt_survey(list(1:3, 4:6)),
    "named list"
  )
})

test_that("print.satpt_survey shows headline and outstanding-question list", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res <- satpt::satpt_survey(e$diagnoses, by = "wave", threshold = 0.001)
  out <- capture.output(print(res))
  expect_true(any(grepl("0 of 2 questions saturated", out)))
  expect_true(any(grepl("Outstanding", out)))
  # Both questions should appear in the outstanding list.
  expect_true(any(grepl("q1", out)))
  expect_true(any(grepl("q2", out)))
})

test_that("print.satpt_survey omits the outstanding line when all saturate", {
  e <- new.env()
  data(diagnoses, package = "satpt", envir = e)
  res <- satpt::satpt_survey(e$diagnoses, by = "wave")
  out <- capture.output(print(res))
  expect_true(any(grepl("2 of 2 questions saturated", out)))
  expect_false(any(grepl("Outstanding", out)))
})

test_that("satpt_survey forwards select-all-apply columns without warning", {
  e <- new.env()
  data(bacteremia, package = "satpt", envir = e)
  # bacteremia$q8 is itself a data.frame (select-all-apply). Running over
  # the whole survey should not emit the wide-y warning.
  expect_silent(
    satpt::satpt_survey(
      list(q4 = e$bacteremia$q4, q8 = e$bacteremia$q8),
      by = e$bacteremia$wave
    )
  )
})
