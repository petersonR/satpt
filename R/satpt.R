#' @title Saturation point analysis
#'
#' @description Saturation point analysis of multinomial responses from a
#' survey using standard errors of the sample proportions for the responses.
#'
#' @param y Multinomial responses collected and being examined for saturation.
#' See the *Details* section for valid *R* data objects and how
#' they are handled.
#' @param by Values indicating when the multinomial responses (`y`) were
#' collected. See the *Details* section for when to specify this argument.
#' @param exclude Vector of values that should be excluded in `y` and
#' `by`. Generally, this should be used to denote missing values. Default
#' is `NA` and `NaN`.
#' @param alpha Significance level for test for independence by `y` and `by`.
#' Default is `0.05`.
#' @param threshold Saturation threshold applied to the maximum standard error
#' of the sample proportions. Default is `0.025` and the threshold must be less
#' or equal to 0.25.
#' @param dimnames Character vector of names for `y` and `by` when
#' displaying the contingency table, sample proportions, and standard error
#' matrices. When `dimnames` is an unnamed vector the first entry should be name
#' of `y` variable and the second entry should be name of `by` variable. If
#' `dimnames` is a named vector, then order of the values does NOT matter, as
#' long the elements are named with `"y"` and `"by"`. Default is `NULL`.
#' @param select_all_apply Logical that disambiguates how a multi-column `y`
#' is interpreted. `TRUE` says you intend the columns to be the response items
#' of one *select-all-that-apply* question; `FALSE` says they should be a
#' single multiple-choice question (and a multi-column `y` is therefore an
#' error). The default `NULL` warns when `y` has more than one column,
#' because a wide data.frame of separate questions is a common mistake.
#' Pass [satpt::satpt_survey()] when you want to analyze each column as its
#' own question instead. Single-column `y` is unaffected.
#' @param ... Additional arguments passed to [stats::chisq.test()] or
#' [stats::fisher.test()] for more control over the test for independence.
#' `x` and `y` arguments from [stats::chisq.test()] or [stats::fisher.test()]
#' should **not** be spectified because the `by` and `y` from arguments above
#' will create a contingency matrix to perform the test of independence on.
#'
#' @details The `by` argument should be specified when the responses collected
#' in `y` are a by-product of the data collection mechanism defined by the `by`
#' variable. When there is no a priori data collection mechanism defined, the
#' `by` argument should not be defined. Generally, when the data is collected
#' randomly or collected during the first data collection period, there is "no
#' a priori data collection mechanism."
#'
#' The parameters `y` and `by` maybe a `vector`, `factor`, `matrix`,
#' `data.frame`, `data.table`, `tibble`, or `list`. When the parameters are a
#' `list` object, each element of the object should be of equal length.
#' Otherwise missing values will be appended to the end of each element based on
#' the element that has the longest length. If `y` or `by` are `factor`s the
#' underlying order of the factor will be ignored when the contingency table is
#' created. The [satpt::char_matrix()] function is used within `satpt()` to
#' coerce the values of `y` and `by` to be character values to have consistency
#' of value types.
#'
#' Generally, `y` should only have more than 1 column when select all apply
#' questions are being examined. See the `select-all-apply` vignette for more
#' information.
#'
#' Functionality of `satpt()` depends on the limited use of [stats::ftable()].
#' More specifically, `y`, `by`, and `exclude` are directly used with
#' [stats::ftable()] to create the contingency table of the collected data. The
#' contingency table created by [stats::ftable()] is converted to a `matrix` for
#' easier use with other functions. When the `dimnames` argument is used, it
#' manipulates the attributes of the created `ftable`, which impacts the
#' dimension names of the resulting `matrix`.
#'
#' Specification of the `by` argument automatically calls for a test for
#' independence and the heterogeneity index of the sample proportions to be
#' calculated. The test for independence is conducted to determine if response
#' bias is present within the responses in `y` are due to the data collection
#' periods (`by`). Fisher's Exact Test or the \eqn{I \times J} variant is
#' implemented when the more than 20% of the expected cell counts are less than
#' 5. Otherwise Pearsons' \eqn{\chi^2} Test for Independence is implemented.
#'
#' When response bias is present the pooled standard errors of the overall
#' sample proportions for each response item is reported, the pooled
#' standard errors account for the response bias. The heterogeneity index is
#' defined as the mean absoluted difference of the sample proportions for each
#' response item within each data collection period (`by`) relative to the
#' overall sample proportions for each response item. This index reflects
#' the average deviation of the data collection period proportions from the
#' overall sample proportions. Smaller values indicate the sample proportions of
#' the data collection periods are less dissimilar. This measure is of
#' importance when response bias is present. When `by` is not specified the test
#' for independence will not be conducted and the heterogeneity index will not
#' be calculated. `satpt` assumes response bias is only possible when `by` is
#' specified. Thus, there is no need to check for response bias when `by` is
#' not specified.
#'
#' Determination of saturation is based on the the response item and/or
#' collection of responses that has the largest standard error. If this largest
#' standard error achieves saturation then all other categories or responses
#' will achieve saturation. For select all apply questions, the collection of
#' responses that have the largest standard error (i.e., a sample proportion
#' closest to 0.5) will be used to determine saturation of all responses.
#'
#' @return An object with `S3` class `"satpt"` containing 13 elements. The
#' return elements in a `"satpt"` object are based on the response item that
#' had the largest standard error. The `which_saturation` returned value
#' indicates which response item had the largest standard error. This nature
#' of `satpt` is of most important when determining saturation for select all
#' apply questions.
#' \describe{
#'  \item{`threshold`}{Saturation threshold applied to the standard errors of
#'  thesample proportions.}
#'  \item{`saturation`}{A logical value indicating whether response item
#'  with the largest standard error has achieved saturation given the defined
#' `threshold`. The value of `TRUE` indicates that saturation has been achieved
#'  while a value of `FALSE` indicates that saturation was not achieved and more
#'  data is needed to achieve saturation.}
#'  \item{`which_saturation`}{A character value indicating which collection of
#'  responses within `y` determined saturation achievement. Generally, this is
#'  only of importance when examining select all apply questions. For
#'  multiple choice type of question, the returned value should just be the
#'  object name.}
#'  \item{`counts`}{A `matrix` object containing the observed cell
#'  counts of the contigency table created by `y` and `by` if provided.}
#'  \item{`phat`}{A `matrix` object containing the row-wise sample
#'  proportions for the observed contigency table (`counts`).}
#'  \item{`se`}{A `matrix` object containing the standard errors for
#'  the calculated sample proportions (`phat`).}
#'  \item{`pooled_se`}{A logical value indicating whether pooled standard errors
#'  were calculated due to the presence of response bias. `NULL` when `by` is
#'  not specified.}
#'  \item{`alpha`}{Significance level for the test for independence.}
#'  \item{`test`}{A `htest` object produced by [stats::chisq.test()]
#'  or [stats::fisher.test()] containing the results from the test for
#'  independence. `NULL` when `by` is not specified.}
#'  \item{`n`}{Total number of observations with a response provided.}
#'  \item{`n_to_saturation`}{Approximate number of additional responses
#'  required for the largest standard error to fall to or below `threshold`,
#'  assuming the current sample proportions and (for pooled SE) the wave
#'  structure are preserved as more responses arrive. `0` when saturation
#'  has already been achieved.}
#'  \item{`total`}{A `data.frame` object with 4 variables describing
#'  the overall collected sample. The `categories` variable provides the unique
#'  categories listed in `y`. While `counts`, `phat`, and `se` provide the
#'  overall cell counts, sample proportions, and standard errors for the
#'  categories, respectively. The standard errors reported for the overall
#'  sample proportions are calculated based on the presence of response bias,
#'  which is detailed above.}
#'  \item{`hindex`}{A vector of heterogeneity index values for the sample
#'  proportions calculated by mean absolute deviation. `NULL` when `by` is
#'  not specified.}
#' }
#'
#' @note The returned `satpt` object will contain `NULL` values for `test`,
#' `pooled_se`, and `hindex` when `by` is not specified. This is done because
#' `satpt` assumes `by` is only specified when the data is collected in
#' intervals.
#'
#' @seealso [stats::ftable()] [stats::chisq.test()]
#'
#' @examples
#' data(diagnoses)
#'
#' # Assuming response bias is not a possiblity
#' satpt::satpt(y = diagnoses$q2)
#'
#' # Examining saturation given data collected at different times and
#' # response bias is possible. For this example, response bias is not present,
#' # so the standard errors will be the same.
#' satpt::satpt(y = diagnoses$q2, by = diagnoses$wave)
#'
#' # Creating an example, where response bias is present.
#'
#' ## Simulating data
#' prob <- matrix(
#'   data = c(0.4, 0.4, 0.2, 0.1, 0.1, 0.8),
#'   nrow = 2, ncol = 3, byrow = TRUE
#' )
#' catg <- LETTERS[1:3]
#' set.seed(123)
#' dat <- satpt::simulate(
#'   n = 1, size = c(250, 100), prob = prob, categories = catg
#' )
#'
#' ## Determining saturation with response bias
#' res <- satpt::satpt(y = dat$responses1, by = dat$period)
#' summary(res)
#'
#' @rdname satpt
#' @export
satpt <- function(
  y,
  by,
  exclude = c(NA, NaN),
  alpha = 0.05,
  threshold = 0.025,
  dimnames = NULL,
  select_all_apply = NULL,
  ...
) {
  # Capture variable names from unevaluated arguments ####
  var_name <- extract_var_name(substitute(expr = y), default = "y")
  if (!missing(by)) {
    var_name_by <- extract_var_name(substitute(expr = by), default = "by")
  } else {
    var_name_by <- "by"
  }

  # Coerce y and by to consistent types ####
  y <- coerce_responses(y, var_name = var_name)
  by <- if (!missing(by)) coerce_grouping(by) else NULL

  # Guard against accidental select-all-apply interpretation ####
  check_select_all_apply(y = y, select_all_apply = select_all_apply)

  # Validate scalar arguments ####
  validate_args(
    y = y, by = by,
    alpha = alpha, threshold = threshold,
    dimnames = dimnames
  )

  # Per-column analysis ####
  dots <- list(...)
  counts <- lapply(
    X = seq_len(ncol(y)),
    FUN = function(j) {
      build_counts(
        y_col = y[, j],
        by = by,
        exclude = exclude,
        dimnames = dimnames,
        var_name_by = var_name_by,
        col_name = colnames(y)[j]
      )
    }
  )
  names(counts) <- colnames(y)
  phat <- lapply(X = counts, FUN = base::proportions, margin = 1)
  se <- lapply(X = counts, FUN = calc_se)

  # Independence tests when each contingency table has >= 2 rows ####
  has_test <- all(vapply(
    X = counts,
    FUN = function(z) all(dim(z) >= 2L),
    FUN.VALUE = logical(1)
  ))
  if (has_test) {
    test <- lapply(X = counts, FUN = run_independence_test, dots = dots)
    pooled_se <- lapply(X = test, FUN = function(tt) tt$p.value <= alpha)
  } else {
    test <- vector(mode = "list", length = length(counts))
    pooled_se <- vector(mode = "list", length = length(counts))
    names(test) <- names(counts)
    names(pooled_se) <- names(counts)
  }

  # Overall sample statistics per column ####
  total <- Map(
    f = calc_total,
    counts_mat = counts,
    test = test,
    se_mat = se,
    pooled = pooled_se
  )

  # Saturation decision and the column that drives it ####
  max_se <- vapply(
    X = total,
    FUN = function(tt) max(tt$se),
    FUN.VALUE = numeric(1)
  )
  saturation <- max(max_se) <= threshold
  which_saturation <- names(max_se)[which.max(max_se)]

  # Heterogeneity index ####
  if (has_test) {
    hindex <- Map(
      f = calc_hindex,
      counts_mat = counts,
      phat_mat = phat,
      total_df = total
    )
  } else {
    hindex <- NULL
  }

  # Assemble output preserving every documented slot ####
  total_obs <- vapply(
    X = total,
    FUN = function(tt) sum(tt$counts),
    FUN.VALUE = integer(1)
  )
  n_picked <- unname(total_obs[which_saturation])
  out <- list(
    threshold = threshold,
    saturation = saturation,
    which_saturation = which_saturation,
    counts = counts[[which_saturation]],
    phat = phat[[which_saturation]],
    se = se[[which_saturation]],
    pooled_se = if (has_test) pooled_se[[which_saturation]] else NULL,
    alpha = alpha,
    test = if (has_test) test[[which_saturation]] else NULL,
    n = n_picked,
    n_to_saturation = calc_n_to_saturation(
      max_se = max(max_se),
      n = n_picked,
      threshold = threshold
    ),
    total = total[[which_saturation]],
    hindex = if (!is.null(hindex)) hindex[[which_saturation]] else NULL
  )
  structure(out, class = "satpt")
}
