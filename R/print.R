#' @title Print saturation point analysis
#'
#' @description `print` prints `satpt` objects created by [satpt::satpt()].
#'
#' @param x `satpt` object to be printed.
#' @param digits Minimal number of *significant digits*.
#' Default is `max(3, getOption("digits") - 3)`.
#' @inheritDotParams base::print
#'
#' @examples
#' data(diagnoses)
#'
#' # Examining saturation given data collected at different times and
#' # response bias is possible. For this example, response bias is not present,
#' # so the standard errors will be the same.
#'
#' # Saving analysis as R object and printing
#' res <- satpt::satpt(y = diagnoses$q2, by = diagnoses$wave)
#' print(x = res, digits = 3)
#'
#' @export
print.satpt <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  # Check object type ####
  if (!methods::is(object = x, class2 = "satpt")) {
    stop("x must be of satpt type.")
  }

  # Creating printing object ####
  phat <- x$total$phat
  phat <- format(round(x = phat, digits = digits), nsmall = digits)
  se <- x$total$se
  se <- format(round(x = se, digits = digits), nsmall = digits)
  print_table <- matrix(
    data = as.numeric(c(phat, se)),
    nrow = 2,
    ncol = length(phat),
    byrow = TRUE,
    dimnames = list(
      c("Proportion", "SE"),
      x$total$categories
    )
  )
  names(dimnames(print_table)) <- c(
    "Statistics",
    names(dimnames(x$phat))[2]
  )

  # Printing results ####
  cat(saturation_headline(x), "\n\n", sep = "")
  cat("Overall Sample Proportions and Standard Errors\n")
  cat("==============================================\n")
  print(x = print_table, ...)
}

# Headline sentence(s) explaining the result in CI half-width / percentage-
# point terms a non-statistician can act on. Used at the top of print.satpt
# and print.satpt_survey output.
saturation_headline <- function(x) {
  pp <- function(v) format(round(v * 100, digits = 1L), nsmall = 1L)
  half_width <- stats::qnorm(p = 0.975) * max(x$total$se)
  threshold_hw <- stats::qnorm(p = 0.975) * x$threshold
  if (x$saturation) {
    paste0(
      "Saturation achieved for ", x$which_saturation, ".\n",
      "With ", x$n, " responses, the largest 95% CI half-width is ",
      "±", pp(half_width), " percentage points (within the ",
      "±", pp(threshold_hw), " pp threshold)."
    )
  } else {
    paste0(
      "Saturation not yet achieved for ", x$which_saturation, ".\n",
      "With ", x$n, " responses, the largest 95% CI half-width is ",
      "±", pp(half_width), " percentage points (threshold ",
      "±", pp(threshold_hw), " pp).\n",
      "About ", x$n_to_saturation,
      " more responses needed (assuming current proportions hold)."
    )
  }
}
