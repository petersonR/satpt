#' @title Plot of saturation point analysis
#'
#' @description Plotting of the overall standard errors calculated during the
#' saturation point anlaysis performed by [satpt::satpt()].
#'
#' @param x `satpt` object to be plotted.
#' @param threshold logical; if `TRUE`, display saturation threshold.
#' Default is `TRUE`.
#' @param \dots Further arguments to be passed to [graphics::barplot()]
#'
#' @seealso [graphics::barplot()]
#'
#' @examples
#' # loading data
#' data(diagnoses)
#'
#' # performing saturation point analysis
#' fit1 <- satpt::satpt(y = diagnoses$q2)
#' fit2 <- satpt::satpt(
#'   y = diagnoses$q2,
#'   by = diagnoses$wave,
#'   dimnames = c("Response collected", "Collection period"),
#'   threshold = 0.015
#' )
#'
#' # plotting standard errors
#' plot(fit1)
#' plot(fit2)
#'
#' @export
#'
plot.satpt <- function(x, threshold = TRUE, ...) {
  # Check object type ####
  if (!methods::is(object = x, class2 = "satpt")) {
    stop("x must be of satpt type.")
  }

  # Specifying default plotting parameters ####

  ## Determining limit of y-axis ####
  se_max <- signif(
    abs(
      signif(max(x$total$se), digits = 1) -
        max(x$total$se)
    ) +
      signif(max(x$total$se), digits = 1),
    digits = 1
  )
  ylim_max <- max(x$threshold, se_max)

  ## Sepcifying plotting parameter ####
  barplot_args <- list(
    height = x$total$se,
    names = x$total$categories,
    xlab = names(dimnames(x$phat))[2],
    ylab = "Standard errors",
    ylim = c(0, ylim_max),
    main = "Standard errors of overall sample proportions",
    border = NA
  )

  # Pulling additional plotting parameters from user ####
  new_args <- list(...)

  ## Updating default plotting parameters ####
  if (length(new_args)) {
    barplot_args[names(new_args)] <- new_args
  }

  # Plotting standard errors ####
  do.call(what = "barplot", args = barplot_args)

  ## Adding horizontal line for saturationa threshold ####
  if (threshold) {
    graphics::abline(h = x$threshold, col = "firebrick", lty = 3, lwd = 2)
  }
}
