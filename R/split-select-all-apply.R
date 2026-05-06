#' @title Split a select-all-that-apply response column into binary indicators
#'
#' @description Converts a character vector whose entries hold one or more
#' response tokens separated by `sep` (e.g. `"Broadrange|MNGS|Other"`) into a
#' `data.frame` with one column per token, each entry `"Yes"` or `"No"`
#' depending on whether the token was selected. NA entries (and entries that
#' contain only whitespace or empty tokens) become a row of `NA`s in the
#' output.
#'
#' @param x Character vector (or factor) of separator-delimited responses.
#' @param sep Separator string. Default `"|"`, the convention used by the
#' bundled [satpt::diagnoses] data set. Matched literally (not as a regular
#' expression).
#' @param values Optional character vector of tokens to use as the column
#' set. When `NULL` (the default), the unique non-empty tokens observed in
#' `x` are used and sorted alphabetically.
#'
#' @details This is the explicit, exported version of the boilerplate shown
#' in the *select-all-apply* vignette. It produces a `data.frame` shaped
#' exactly the way [satpt::satpt()] expects for select-all-that-apply
#' analysis: one indicator column per response item.
#'
#' Pass the result directly to [satpt::satpt()] (with `select_all_apply =
#' TRUE`) or include it as a question column inside a survey passed to
#' [satpt::satpt_survey()].
#'
#' @return A `data.frame` with one column per distinct token in `x` (or per
#' element of `values`, when supplied). All columns are character vectors
#' with values `"Yes"`, `"No"`, or `NA`.
#'
#' @examples
#' data(diagnoses)
#' q1 <- split_select_all_apply(x = diagnoses$q1, sep = "|")
#' head(q1)
#'
#' # Feed straight into satpt():
#' satpt(
#'   y = q1, by = diagnoses$wave, select_all_apply = TRUE
#' )
#'
#' @export
split_select_all_apply <- function(x, sep = "|", values = NULL) {
  if (!is.character(x) && !is.factor(x)) {
    stop("x must be a character or factor vector.")
  }
  if (!is.character(sep) || length(sep) != 1L || !nzchar(sep)) {
    stop("sep must be a single non-empty string.")
  }

  x <- as.character(x = x)
  parts <- strsplit(x = x, split = sep, fixed = TRUE)
  parts <- lapply(
    X = parts,
    FUN = function(p) {
      p <- trimws(x = p)
      p[!is.na(p) & nzchar(p)]
    }
  )

  if (is.null(values)) {
    values <- sort(unique(unlist(parts)))
  } else {
    if (!is.character(values)) {
      stop("values must be a character vector.")
    }
  }

  if (length(values) == 0L) {
    stop("No tokens found in x; nothing to split.")
  }

  na_rows <- vapply(
    X = parts,
    FUN = function(p) length(p) == 0L,
    FUN.VALUE = logical(1)
  )

  out <- vapply(
    X = values,
    FUN = function(v) {
      ifelse(
        test = vapply(
          X = parts,
          FUN = function(p) v %in% p,
          FUN.VALUE = logical(1)
        ),
        yes = "Yes",
        no = "No"
      )
    },
    FUN.VALUE = character(length(x))
  )
  if (!is.matrix(out)) {
    out <- matrix(data = out, ncol = length(values))
  }
  colnames(out) <- values
  out[na_rows, ] <- NA_character_

  as.data.frame(x = out, stringsAsFactors = FALSE)
}
