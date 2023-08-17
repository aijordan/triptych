#' Evaluation of forecasts using ROC curves
#'
#' A ROC curve visualizes discrimination ability by displaying the hit rate against
#' the false alarm rate for all threshold values.
#'
#' @param convex A boolean value indicating whether to calculate the convex hull
#'   or the raw ROC diagnostic.
#' @param ... Unused.
#' @inheritParams triptych
#'
#' @return A `triptych_roc` object, that is a `vctrs_vctr` subclass, and has
#'   a length equal to number of prediction methods supplied in `x`. Each entry
#'   is named according to the corresponding prediction method,
#'   and contains a list of named objects:
#'   * `estimate`: A data frame of hit rates and false rates.
#'   * `region`: Either an empty list, or a data frame of pointwise
#'       confidence intervals (along diagonal lines with slope \eqn{-\pi_0/\pi_1})
#'       added by [add_confidence()].
#'   * `x`: The numeric vector of original predictions.
#'   Access is most convenient through [estimates()], [regions()], and [forecasts()].
#'
#' @seealso Accessors: [estimates()], [regions()], [forecasts()]
#'
#'   Adding uncertainty quantification: [add_confidence()]
#'
#'   Visualization: [plot.triptych()], [autoplot.triptych()]
#'
#' @examples
#' # Construction
#' predictions <- matrix(runif(300), ncol = 3)
#' colnames(predictions) <- c("Method_1", "Method_2", "Method_3")
#' observations <- rbinom(100, 1, predictions[, 1])
#' roc1 <- roc(predictions, observations)
#'
#' pred_obs <- tibble::tibble(M1 = runif(100), y = rbinom(100, 1, M1))
#' roc2 <- roc(pred_obs)
#'
#' # Visualization
#' autoplot(roc1)
#'
#' library(ggplot2)
#' df_est <- estimates(roc1)
#' ggplot(df_est) +
#'   geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1)) +
#'   geom_path(aes(x = FAR, y = HR, col = forecast)) +
#'   labs(x = "False alarm rate", y = "Hit rate")
#'
#' @name roc
NULL

#' @rdname roc
#' @export
roc <- function(x, y = NULL, convex = TRUE, ...) {
  x <- tibble::as_tibble(x)
  if (is.null(y)) {
    stopifnot("y" %in% names(x))
    y <- x$y
    x <- dplyr::select(x, !y)
  }
  y <- vec_cast(y, to = double())
  convex <- vec_cast(convex, to = logical())
  stopifnot(identical(length(convex), 1L))
  x <- dplyr::mutate_all(x, vec_cast, to = double())
  vec_cast(x, to = new_triptych_roc(y = y, convex = convex))
}

new_triptych_roc <- function(x = list(), y = numeric(), convex = logical()) {
  new_vctr(x, y = y, convex = convex, class = "triptych_roc")
}

# formatting
#' @export
format.triptych_roc <- function(x, ...) {
  sprintf("<named list[%i]>", sapply(x, length))
}
#' @export
vec_ptype_abbr.triptych_roc <- function(x, ..., prefix_named = FALSE, suffix_shape = TRUE) {
  "trpt_roc"
}

# coercion

vec_ptype2.triptych_roc <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.triptych_roc")
}
#' @export
vec_ptype2.triptych_roc.triptych_roc <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!has_compatible_observations(x, y)) {
    stop_incompatible_type(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      details = "Observations are not compatible."
    )
  }
  new_triptych_roc(list(), observations(x), attr(x, "convex"))
}

# casting

#' @param r A reference triptych_mcbdsc object whose attributes are used for casting.
#'
#' @rdname roc
#' @export
as_roc <- function(x, r) {
  stopifnot(inherits(r, "triptych_roc"))
  x <- tibble::as_tibble(x)
  x <- dplyr::mutate_all(x, vec_cast, to = double())
  vec_cast(x, to = r)
}

vec_cast.triptych_roc <- function(x, to, ...) {
  UseMethod("vec_cast.triptych_roc")
}
#' @export
vec_cast.triptych_roc.triptych_roc <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (!has_compatible_observations(x, to)) {
    stop_incompatible_cast(
      x,
      to,
      x_arg = x_arg,
      to_arg = to_arg,
      details = "Observations are not compatible."
    )
  }
  x
}
#' @export
vec_cast.triptych_roc.list <- function(x, to, ...) {
  x <- lapply(x, vec_cast, to = to)
  f <- \(...) vec_c(..., .name_spec = "{outer}_{inner}")
  do.call(f, x)
}
#' @export
vec_cast.triptych_roc.data.frame <- function(x, to, ...) {
  x <- lapply(x, vec_cast, to = to)
  f <- \(...) vec_c(..., .name_spec = "{outer}_{inner}")
  do.call(f, x)
}
#' @export
vec_cast.triptych_roc.tbl_df <- function(x, to, ...) {
  x <- lapply(x, vec_cast, to = to)
  f <- \(...) vec_c(..., .name_spec = "{outer}_{inner}")
  do.call(f, x)
}
#' @export
vec_cast.triptych_roc.double <- function(x, to, ...) {
  y <- observations(to)
  convex <- attr(to, "convex")
  xr <- if (convex) recalibrate_mean(x, y) else x

  list(
    estimate = pROC::roc(y, xr, direction = "<", quiet = TRUE),
    region = list(),
    x = x
  ) |>
    list() |>
    new_triptych_roc(y = y, convex = convex)
}


eval_diag.triptych_roc <- function(x, at, p1 = mean(attr(x, "y")), ...) {
  purrr::map(x, at = at, p1 = p1, .f = \(o, at, p1) {
    pROC::coords(o$estimate, ret = c("tpr", "fpr")) |>
      with(approx(p1 * tpr + (1 - p1) * fpr, fpr, xout = at)$y)
  })
}

#' @export
observations.triptych_roc <- function(x, ...) {
  attr(x, "y")
}

#' @export
forecasts.triptych_roc <- function(x, ...) {
  f <- function(o) tibble::tibble(x = o$x)
  g <- function(...) vec_rbind(..., .names_to = "forecast")
  purrr::map(x, f) |>
    do.call(g, args = _)
}

#' @param p1 The unconditional event probability. Used in combination with `at`
#'   to determine the diagonal lines along which to determine the estimate.
#'
#' @rdname estimates
#' @export
estimates.triptych_roc <- function(
    x,
    at = NULL,
    p1 = mean(observations(x)),
    ...
) {
  f <- function(o) {
    tibble::tibble(
      FAR = 1 - o$estimate$specificities,
      HR = o$estimate$sensitivities
    )
  }
  g <- if (is.null(at)) {
    f
  } else {
    function(o) {
      tb <- dplyr::mutate(f(o), R = p1 * .data$HR + (1 - p1) * .data$FAR)
      r1 <- with(tb, approx(1 - R, FAR, xout = at, ties = list("ordered", mean))$y)
      r2 <- with(tb, approx(1 - R, HR, xout = at, ties = list("ordered", mean))$y)
      tibble::tibble(FAR = r1, HR = r2)
    }
  }
  h <- function(...) vec_rbind(..., .names_to = "forecast")
  purrr::map(x, g) |>
    do.call(h, args = _)
}

#' @rdname regions
#' @export
regions.triptych_roc <- function(x, ...) {
  if (!has_regions(x)) return(NULL)
  f <- function(o) o$region
  g <- function(...) vec_rbind(..., .names_to = "forecast")
  purrr::map(x, f) |>
    do.call(g, args = _)
}

#' @export
has_regions.triptych_roc <- function(x, ...) {
  any(sapply(x, \(o) tibble::is_tibble(o$region)))
}

#' @export
add_confidence.triptych_roc <- function(x, level = 0.9, method = "resampling_cases", ...) {
  m <- get(method)(x, level, ...)
  for (i in seq_along(x)) {
    x[[i]]$region <- m[[i]]
  }
  x
}

#' @rdname resampling_cases
#' @export
resampling_cases.triptych_roc <- function(x, level = 0.9, n_boot = 1000, ...) {
  saved_seed <- .Random.seed
  y <- observations(x)
  p1 <- mean(y)
  rates <- seq(0, 1, length.out = length(y) + 1)
  purrr::map(
    .x = x,
    level = level,
    n_boot = n_boot,
    .f = function(o, level, n_boot) {
      bounds <- bootstrap_sample_cases(o$x, y, n_boot, roc, rates, p1 = p1) |>
        bootstrap_quantile(probs = 0.5 + c(-0.5, 0.5) * level)
      tibble::tibble(
        FAR = c(bounds[2L, ], rev(bounds[1L, ])),
        HR = c(rates, rev(rates)) / p1 - .data$FAR * (1 - p1) / p1,
        method = paste0("resampling_cases_", n_boot),
        level = level
      )
    }
  )
}

#' @rdname resampling_Bernoulli
#' @export
resampling_Bernoulli.triptych_roc <- function(x, level = 0.9, n_boot = 1000, ...) {
  saved_seed <- .Random.seed
  y <- observations(x)
  p1 <- mean(y)
  rates <- seq(0, 1, length.out = length(y) + 1)
  purrr::map(
    .x = x,
    level = level,
    n_boot = n_boot,
    .f = function(o, level, n_boot) {
      xr <- recalibrate_mean2(o$x, y)
      bounds <- bootstrap_sample_Bernoulli(o$x, xr, n_boot, roc, rates, p1 = p1) |>
        bootstrap_quantile(probs = 0.5 + c(-0.5, 0.5) * level)
      tibble::tibble(
        FAR = c(bounds[2L, ], rev(bounds[1L, ])),
        HR = c(rates, rev(rates)) / p1 - .data$FAR * (1 - p1) / p1,
        method = paste0("resampling_Bernoulli_", n_boot),
        level = level
      )
    }
  )
}
