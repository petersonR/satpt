#' @title Summarizing the saturation point analysis
#'
#' @description Creates a conditional summary of the saturation point analysis
#' performed by [satpt::satpt()].
#'
#' @param object `satpt` object to be summarized.
#' @param x `summary.satpt` object to be printed.
#' @param digits Minimal number of *significant digits*.
#' Default is `max(3, getOption("digits") - 3)`.
#' @param ... Additionally, arguments passed to [base::print()].
#'
#' @details For printing `summary.satpt` objects, the `digits` parameter does
#' not need to be specified in `print` as [satpt::summary()] already takes
#' care of the formatting of *significant digits*.
#'
#' @return An object with `S3` class `"summary.satpt"` containing 10 elements
#' created by [satpt::summary()].
#' \describe{
#'  \item{`threshold`}{Saturation threshold applied to the standard errors of
#'  the sample proportions.}
#'  \item{`saturation`}{A character value indicating whether all response
#'  categories have achieved saturation given the defined `threshold`. The value
#'  of `"Yes"` indicates that saturation has been achieved while a value of
#'  `"No"` indicates that saturation was not achieved and more data is needed
#'  to achieve saturation.}
#'  \item{`which_saturation`}{A character value indicating which collection of
#'  responses within `y` determined saturation achievement. Generally, this is
#'  only of importance when examining select all apply questions.}
#'  \item{`n`}{Total number of observations with a response provided.}
#'  \item{`phat`}{A `matrix` containing the row-wise sample proportions for the
#'  observed contigency table. The values are formatted by `digits`.}
#'  \item{`se`}{A `matrix` containing the standard errors for the calculated
#'  sample proportions (`phat`). The values are formatted by `digits`.}
#'  \item{`pooled_se`}{A logical value indicating whether pooled standard errors
#'  were calculated due to the presence of response bias.}
#'  \item{`alpha`}{Significance level for test forindependence.}
#'  \item{`test`}{A `htest` object produced by [stats::chisq.test()] containing
#'  the results from the test for independence.}
#'  \item{`hindex`}{Heterogeneity index for the sample proportions calculated by
#'  mean absolute deviation. The values are formatted by `digits` if `hindex`
#'  is not `NULL` in `object`.}
#' }
#'
#' @seealso [satpt::satpt()]
#'
#' @examples
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
#'   n = 1, size = c(200, 100), prob = prob, categories = catg
#' )
#'
#' ## Determining saturation with response bias and summarizing analysis
#' res <- satpt::satpt(y = dat$responses1, by = dat$period)
#' summary(object = res, digits = 3)
#'
#' @rdname summary.satpt
#' @export
summary.satpt <- function(
  object,
  digits = max(3, getOption("digits") - 3),
  ...
) {
  # Check object type ####
  if (!methods::is(object, "satpt")) {
    stop("object must be of satpt type.")
  }

  # Sample proportions ####
  if (nrow(object$phat) != 1) {
    phat <- object$phat

    ## Combining overall sample proportion with interval sample proportions ####
    phat <- rbind(phat, object$total[, 3])

    ## Rounding ####
    phat <- apply(
      X = phat,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ### Adjusting dimension names ####
    row.names(phat) <- c(
      row.names(object$phat),
      "Overall"
    )
    colnames(phat) <- colnames(object$phat)
    names(dimnames(phat)) <- names(
      x = dimnames(object$phat)
    )
  } else {
    phat <- object$phat

    ## Rounding
    phat <- apply(
      X = phat,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ## Converting back to matrix ####
    phat <- matrix(data = phat, nrow = 1, ncol = length(phat), byrow = TRUE)

    ### Adjusting dimension names ####
    row.names(phat) <- "Overall"
    colnames(phat) <- colnames(object$phat)
    names(dimnames(phat)) <- c(
      "",
      names(dimnames(object$phat))[2]
    )
  }

  if (nrow(object$se) != 1) {
    # Sample standard errors ####
    se <- object$se

    ## Combining overall standard errors with interval standard errors ####
    se <- rbind(se, object$total[, 4])

    ## Rounding
    se <- apply(
      X = se,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ### Adjusting dimension names ####
    row.names(se) <- c(
      row.names(object$se),
      "Overall"
    )
    colnames(se) <- colnames(object$se)
    names(dimnames(se)) <- names(dimnames(object$se))
  } else {
    se <- object$se

    ## Rounding
    se <- apply(
      X = se,
      MARGIN = 2,
      FUN = function(x) {
        as.numeric(format(round(x = x, digits = digits), nsmall = digits))
      }
    )

    ## Converting back to matrix ####
    se <- matrix(data = se, nrow = 1, ncol = length(se), byrow = TRUE)

    ### Adjusting dimension names ####
    row.names(se) <- "Overall"
    colnames(se) <- colnames(object$se)
    names(dimnames(se)) <- c(
      "",
      names(dimnames(object$se))[2]
    )
  }

  # Heterogeneity index ####
  if (!is.null(object$hindex)) {
    hindex <- data.frame(
      categories = names(object$hindex),
      index = as.numeric(
        format(
          round(x = object$hindex, digits = digits),
          nsmall = digits
        )
      )
    )
  } else {
    hindex <- NULL
  }

  # Output ####
  out <- list(
    threshold = object$threshold,
    saturation = ifelse(test = object$saturation, yes = "Yes", no = "No"),
    which_saturation = object$which_saturation,
    n = object$n,
    phat = phat,
    se = se,
    pooled_se = object$pooled_se,
    alpha = object$alpha,
    test = object$test,
    hindex = hindex
  )
  return(structure(out, class = "summary.satpt"))
}

#' @rdname summary.satpt
#' @export
print.summary.satpt <- function(x, ...) {
  # Check object type ####
  if (!methods::is(object = x, class2 = "summary.satpt")) {
    stop("x must be of summary.satpt type.")
  }

  # Printing saturation information ####
  cat("\nSaturation point analysis of sample proportions\n")
  cat("===============================================\n")
  cat("\nAnalysis based on:", x$which_saturation, "\n")
  cat(
    "Saturation achieved? ",
    x$saturation,
    "\nSaturation threshold of ",
    x$threshold,
    "\nResponses collected from a sample size of ",
    paste0(x$n, "\n\n"),
    sep = ""
  )

  # Sample proportions ####
  if (nrow(x$phat) != 1) {
    cat("Data interval and overall sample proportions\n")
  } else {
    cat("Overall sample proportions\n")
  }
  cat("============================================\n")
  print(x$phat, ...)

  # Standard errors ####
  if (nrow(x$se) != 1) {
    cat("\n\nData interval and overall standard errors\n")
  } else {
    cat("\n\nOverall standard errors\n")
  }
  cat("=========================================\n")
  print(x$se, ...)

  # Test for independence ####
  if (!is.null(x$test)) {
    cat(
      "\nPooled standard errors? ",
      ifelse(
        test = x$pooled_se,
        yes = "Yes",
        no = "No"
      ),
      "\n"
    )
    print(x$test, ...)
    cat(
      "Response bias present? ",
      ifelse(
        test = x$pooled_se,
        yes = "Yes",
        no = "No"
      ),
      "\nSignificance level: ",
      paste0(x$alpha, "\n"),
      sep = ""
    )
  }

  # Heterogeneity index ####
  if (!is.null(x$hindex)) {
    cat("\n\nHeterogeneity index\n")
    cat("====================\n")
    names(x$hindex) <- c("Categories", "Index")
    print(x$hindex, row.names = FALSE, ...)
  }
}
