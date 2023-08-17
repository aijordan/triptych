#' Accessing original forecast and observation data for triptych objects
#'
#' @param x An object from which the relevant information should be extracted.
#' @param ... Additional arguments passed to other methods.
#'
#' @return
#' For `forecasts()`: A tibble of the original forecasts in long format.
#'
#' For `observations()`: A vector of the observations.
#'
#' @seealso [estimates()], [regions()]
#' @name accessors
NULL

#' Accessing diagnostic estimate data
#'
#' @param x An object from which the estimate information should be extracted.
#' @param at A vector of thresholds where `x` should be evaluated.
#' @param ... Additional arguments passed to other methods.
#'
#' @return
#' A tibble with the relevant information describing
#'   the diagnostic estimate
#'   (Murphy curve, reliability curve, ROC curve, score decomposition)
#'   for all supplied prediction methods.
#'
#' @seealso [regions()], [forecasts()], [observations()]
#' @export
estimates <- function(x, at, ...) {
  UseMethod("estimates")
}

#' Accessing confidence/consistency region data
#'
#' @param x An object from which the region information should be extracted.
#' @param ... Additional arguments passed to other methods.
#'
#' @return
#' A tibble with the relevant information for the
#'   uncertainty quantification of the chosen diagnostic
#'   (Murphy curve, reliability curve, ROC curve, score decomposition)
#'   for all supplied prediction methods.
#'
#' @seealso [estimates()], [forecasts()], [observations()]
#' @export
regions <- function(x, ...) {
  UseMethod("regions")
}
#' @rdname accessors
#' @export
forecasts <- function(x, ...) {
  UseMethod("forecasts")
}
#' @rdname accessors
#' @export
observations <- function(x, ...) {
  UseMethod("observations")
}

has_regions <- function(x, ...) {
  UseMethod("has_regions")
}

eval_diag <- function(x, at, ...) {
  UseMethod("eval_diag")
}

#' Adding confidence regions
#'
#' @param x An object to which a confidence region should be added.
#' @param level A single value for the level of confidence.
#' @param ... Additional arguments passed to methods.
#'
#' @export
add_confidence <- function(x, level, ...) {
  UseMethod("add_confidence")
}

#' Adding consistency regions
#'
#' @param x An object to which a consistency region should be added.
#' @param level A single value for the level of confidence.
#' @param ... Additional arguments passed to methods.
#'
#' @export
add_consistency <- function(x, level, ...) {
  UseMethod("add_consistency")
}

has_compatible_observations <- function(x, y) {
  isTRUE(all.equal(observations(x), observations(y)))
}
