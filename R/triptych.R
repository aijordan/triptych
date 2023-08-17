#' Evaluation of forecasts using a Triptych
#'
#' A triptych visualizes three important aspects of predictive performance:
#' Economic utility via Murphy curves, miscalibration via reliability curves,
#' and discrimination ability via ROC curves.
#' The `triptych` S3 class has plotting methods for `ggplot2`.
#'
#' @param x A data frame, list, matrix, or other object that can be coerced to a tibble. Contains numeric predictions.
#' @param y A numeric vector of observations. If missing, a column with name "y" must be present in the tibble coerced from `x`.
#' @param ... Additional arguments passed to [murphy()], [reliability()], [roc()], and [mcbdsc()].
#'
#' @return A `triptych` object, that is a tibble subclass, and contains five columns:
#'   * `forecast`: Contains the names.
#'   * `murphy`: Contains a `vctrs_vctr` subclass of Murphy curves.
#'   * `reliability`: Contains a `vctrs_vctr` subclass of reliability curves.
#'   * `roc`: Contains a `vctrs_vctr` subclass of ROC curves.
#'   * `mcbdsc`: Contains a `vctrs_vctr` subclass of score decompositions.
#'
#' @seealso Vector class constructors: [murphy()], [reliability()], [roc()], [mcbdsc()]
#'
#'   Adding uncertainty quantification: [add_consistency()], [add_confidence()]
#'
#'   Visualization: [plot.triptych()], [autoplot.triptych()]
#'
#' @examples
#' # Construction
#' predictions <- matrix(runif(300), ncol = 3)
#' colnames(predictions) <- c("Method_1", "Method_2", "Method_3")
#' observations <- rbinom(100, 1, predictions[, 1])
#' tr <- triptych(predictions, observations)
#'
#' pred_obs <- tibble::tibble(M1 = runif(100), y = rbinom(100, 1, M1))
#' tr <- triptych(pred_obs)
#'
#' # Visualization
#' autoplot(tr)
#'
#' # Consistency bands (for reliability curves)
#' tr <- add_consistency(tr, level = 0.9, n_boot = 100)
#' autoplot(tr)
#'
#' @export
triptych <- function(x, y = NULL, ...) {
  x <- tibble::as_tibble(x)
  if (is.null(y)) {
    stopifnot("y" %in% names(x))
    y <- x$y
    x <- dplyr::select(x, !y)
  }
  y <- vec_cast(y, to = double())
  new_triptych(tibble::tibble(
    forecast = names(x),
    murphy = murphy(x, y, ...),
    reliability = reliability(x, y, ...),
    roc = roc(x, y, ...),
    mcbdsc = mcbdsc(x, y, ...)
  ), y)
}

new_triptych <- function(x, y) {
  stopifnot(is.data.frame(x))
  tibble::new_tibble(
    x,
    y = y,
    class = "triptych",
    nrow = nrow(x)
  )
}

# vec_cast.triptych <- function(x, to, ...) {
#   UseMethod("vec_cast.triptych")
# }
vec_cast.triptych.triptych <- function(x, to, ...) {
  triptych_cast(x, to, ...)
}
triptych_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
  new_triptych(out, y = attr(to, "y"))
}
vec_cast.triptych.double <- function(x, to, ...) {
  triptych(x, y = attr(to, "y"))
}
# vec_cast.triptych.data.frame <- function(x, to, ...) {
#   triptych(x, y = attr(to, "y"))
# }

vec_ptype2.triptych.triptych <- function(x, y, ...) {
  triptych_ptype2(x, y, ...)
}
triptych_ptype2 <- function(x, y, ...) {
  out <- tib_ptype2(x, y)
  new_triptych(out, y = attr(x, "y"))
}


add_forecast <- function(x, ...) {
  z <- vec_cast_common(..., .to = x)
  f <- \(...) vec_rbind(..., .names_to = "forecast")
  print(z)
  vec_rbind(x, do.call(f, z))
}

#' @export
observations.triptych <- function(x, ...) {
  attr(x, "y")
}

#' @export
forecasts.triptych <- function(x, ...) {
  f <- function(o) tibble::tibble(x = o$x)
  g <- function(...) vec_rbind(..., .names_to = "forecast")
  purrr::map(x$murphy, f) |>
    do.call(g, args = _)
}

#' @export
add_confidence.triptych <- function(x, level = 0.9, ...) {
  x$murphy <- add_confidence(x$murphy, level = level, ...)
  x$reliability <- add_confidence(x$reliability, level = level, ...)
  x$roc <- add_confidence(x$roc, level = level, ...)
  x
}

#' @export
add_consistency.triptych <- function(x, level = 0.9, ...) {
  x$reliability <- add_consistency(x$reliability, level = level, ...)
  x
}
