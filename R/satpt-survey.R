#' @title Saturation point analysis across an entire survey
#'
#' @description Run [satpt::satpt()] on every question in a survey and
#' return a tidy summary that highlights which questions have reached
#' saturation, which haven't, and how many additional responses each
#' un-saturated question is projected to need.
#'
#' @param data A `data.frame` (or named `list`) of survey responses with
#' one column per question. Columns may be `vector`s, `factor`s, or nested
#' `data.frame`s/`matrix` objects (the latter are treated as
#' select-all-that-apply questions).
#' @param by Either the name of a column in `data` whose values indicate
#' the data collection wave for each row, **or** an external vector of
#' wave indicators with one entry per row of `data`. `NULL` (the default)
#' suppresses the test for response bias and the heterogeneity index, the
#' same way [satpt::satpt()] behaves when `by` is omitted.
#' @param questions Optional character vector of column names from `data`
#' to analyze. Default analyzes every column other than the column named
#' in `by` (when `by` is a column name).
#' @param split Optional named list (or named character vector) of
#' separators for select-all-that-apply questions encoded as a single
#' delimited string per response. For example,
#' `split = list(q1 = "|")` tells `satpt_survey()` to run
#' [satpt::split_select_all_apply()] on `data$q1` with `sep = "|"` before
#' the analysis, so each unique response token becomes its own indicator
#' column. Names not present in `data` are ignored with a warning.
#' @param ... Additional arguments forwarded to [satpt::satpt()] for every
#' question (e.g. `threshold`, `alpha`, `exclude`).
#'
#' @details `satpt_survey()` is a convenience wrapper for the common
#' workflow of "run satpt on each question of my survey and tell me which
#' ones are still collecting data." Multi-column inputs (nested
#' `data.frame`/`matrix` columns, typical of select-all-that-apply
#' questions) are forwarded to `satpt()` with `select_all_apply = TRUE` so
#' you don't see the wide-`y` warning.
#'
#' The returned object is an `S3` object of class `"satpt_survey"` with
#' two top-level slots: `summary` (a tidy `data.frame`, one row per
#' question) and `results` (a named `list` of the underlying `satpt`
#' objects, available for drill-down).
#'
#' @return An object with `S3` class `"satpt_survey"` containing:
#' \describe{
#'   \item{`summary`}{A `data.frame` with one row per question and the
#'   columns `question`, `n` (responses observed), `max_se` (largest
#'   standard error driving the saturation decision), `saturation`
#'   (logical), and `n_to_saturation` (projected additional responses
#'   needed; `0` when already saturated).}
#'   \item{`results`}{A named `list` of `satpt` objects, one per question,
#'   in the same order as `summary$question`.}
#' }
#'
#' @seealso [satpt::satpt()]
#'
#' @examples
#' data(diagnoses)
#'
#' # Analyze every question in the diagnoses survey, using `wave` to
#' # detect response bias across data collection periods. q1 is a
#' # select-all-that-apply question encoded as pipe-delimited strings,
#' # so `split` is used to convert it before analysis.
#' satpt_survey(diagnoses, by = "wave", split = list(q1 = "|"))
#'
#' # Analyze a subset of questions; drill down into one.
#' res <- satpt_survey(diagnoses, by = "wave", questions = "q2")
#' res$results$q2
#'
#' @export
satpt_survey <- function(data, by = NULL, questions = NULL,
                         split = NULL, ...) {
  cols <- list_columns(data)

  parsed <- resolve_by(data = data, by = by, cols = cols)
  by_vec <- parsed$by_vec
  by_col_name <- parsed$by_col_name

  if (!is.null(split)) {
    data <- apply_split(data = data, split = split, cols = cols)
  }

  if (is.null(questions)) {
    questions <- setdiff(cols, by_col_name)
  } else {
    if (!is.character(questions)) {
      stop("questions must be a character vector of column names.")
    }
    missing_q <- setdiff(questions, cols)
    if (length(missing_q) > 0L) {
      stop(
        "Column(s) not found in data: ",
        paste(missing_q, collapse = ", ")
      )
    }
  }

  if (length(questions) == 0L) {
    stop("No questions to analyze.")
  }

  results <- lapply(questions, run_one_question,
    data = data, by_vec = by_vec, ...
  )
  names(results) <- questions

  summary_df <- data.frame(
    question = questions,
    n = vapply(results, function(r) r$n, integer(1)),
    max_se = vapply(results, function(r) max(r$total$se), numeric(1)),
    saturation = vapply(results, function(r) r$saturation, logical(1)),
    n_to_saturation = vapply(
      results,
      function(r) r$n_to_saturation,
      integer(1)
    ),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  structure(
    list(summary = summary_df, results = results),
    class = "satpt_survey"
  )
}

# Internal: enumerate the column / element names of a data.frame or list.
list_columns <- function(data) {
  if (is.data.frame(data)) {
    return(colnames(data))
  }
  if (is.list(data)) {
    nm <- names(data)
    if (is.null(nm) || any(!nzchar(nm))) {
      stop("data must be a named list (every element needs a name).")
    }
    return(nm)
  }
  stop("data must be a data.frame or named list.")
}

# Internal: validate the split argument and apply
# split_select_all_apply() to each named question column. Returns the
# data with the targeted columns replaced by indicator data.frames.
apply_split <- function(data, split, cols) {
  if (!is.list(split)) {
    if (is.character(split) && !is.null(names(split))) {
      split <- as.list(split)
    } else {
      stop("split must be a named list or named character vector.")
    }
  }
  if (is.null(names(split)) || any(!nzchar(names(split)))) {
    stop("Every element of split must be named with a question column.")
  }
  unknown <- setdiff(names(split), cols)
  if (length(unknown) > 0L) {
    warning(
      "Ignoring split entries for columns not in data: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
    split <- split[setdiff(names(split), unknown)]
  }
  for (q in names(split)) {
    data[[q]] <- split_select_all_apply(
      x = data[[q]],
      sep = split[[q]]
    )
  }
  data
}

# Internal: resolve the `by` argument to an external vector and the name
# of the column inside `data` (when applicable, so it can be excluded
# from the question loop).
resolve_by <- function(data, by, cols) {
  if (is.null(by)) {
    return(list(by_vec = NULL, by_col_name = NULL))
  }
  if (is.character(by) && length(by) == 1L) {
    if (!by %in% cols) {
      stop("by '", by, "' not found in data column names.")
    }
    return(list(by_vec = data[[by]], by_col_name = by))
  }
  list(by_vec = by, by_col_name = NULL)
}

# Internal: run satpt() on a single question column. Wraps atomic
# columns in a 1-column data.frame so satpt() sees the question's column
# name — without this, satpt()'s deparse-based name extraction lands on
# the do.call literal (e.g. "c") rather than the question name. Always
# passes select_all_apply = TRUE so genuinely multi-column items (select-
# all-that-apply questions stored as nested data.frames) don't trigger
# the wide-y warning here.
run_one_question <- function(q, data, by_vec, ...) {
  y_q <- data[[q]]
  if (is.atomic(y_q) || is.factor(y_q)) {
    y_q <- data.frame(
      stats::setNames(list(y_q), q),
      stringsAsFactors = FALSE
    )
  }
  call_args <- list(...)
  call_args$y <- y_q
  call_args$select_all_apply <- TRUE
  if (!is.null(by_vec)) {
    call_args$by <- by_vec
  }
  do.call(what = satpt, args = call_args)
}

#' @rdname satpt_survey
#' @param x A `satpt_survey` object to be printed.
#' @export
print.satpt_survey <- function(x, ...) {
  if (!methods::is(object = x, class2 = "satpt_survey")) {
    stop("x must be of satpt_survey type.")
  }
  total <- nrow(x$summary)
  saturated <- sum(x$summary$saturation)
  unsat <- x$summary[!x$summary$saturation, , drop = FALSE]

  cat(
    sprintf(
      "Saturation analysis: %d of %d question%s saturated.\n",
      saturated, total, if (total == 1L) "" else "s"
    )
  )
  if (nrow(unsat) > 0L) {
    cat(
      sprintf(
        "Outstanding: %s (need ~%d more response%s).\n",
        paste(unsat$question, collapse = ", "),
        sum(unsat$n_to_saturation),
        if (sum(unsat$n_to_saturation) == 1L) "" else "s"
      )
    )
  }
  cat(strrep("=", 56), "\n", sep = "")
  print(x$summary, row.names = FALSE, ...)
  invisible(x)
}
