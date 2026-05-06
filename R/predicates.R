#' @title Has saturation been achieved?
#'
#' @description Convenience predicate for branching on a [satpt::satpt()]
#' result without reaching into the `$saturation` slot.
#'
#' @param res A `satpt` object returned by [satpt::satpt()].
#'
#' @return A length-1 logical: `TRUE` if saturation has been achieved at
#' the threshold used in `res`, `FALSE` otherwise.
#'
#' @examples
#' data(diagnoses)
#' res <- satpt::satpt(y = diagnoses$q2, by = diagnoses$wave)
#' if (satpt::is_saturated(res)) {
#'   message("done collecting q2")
#' } else {
#'   message("collect ~", res$n_to_saturation, " more responses")
#' }
#'
#' @export
is_saturated <- function(res) {
  if (!methods::is(object = res, class2 = "satpt")) {
    stop("res must be of class 'satpt'.")
  }
  isTRUE(res$saturation)
}
