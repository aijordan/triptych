#' Evaluation of forecasts using reliability curves
#'
#' A reliability curve visualizes miscalibration by displaying the (isotonic)
#' conditional event probability against the forecast value.
#'
#' @param ... Unused.
#' @inheritParams triptych
#'
#' @return A `triptych_reliability` object, that is a `vctrs_vctr` subclass, and has
#'   a length equal to number of forecasting methods supplied in `x`. Each entry
#'   is named according to the corresponding forecasting method,
#'   and contains a list of named objects:
#'   * `estimate`: A data frame with the isotonic regression estimate.
#'   * `region`: Either an empty list, or a data frame of pointwise consistency
#'       or confidence intervals
#'       added by [add_consistency()] or [add_confidence()], respectively.
#'   * `x`: The numeric vector of original forecasts.
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
#' forecasts <- matrix(runif(300), ncol = 3)
#' colnames(forecasts) <- c("Method_1", "Method_2", "Method_3")
#' observations <- rbinom(100, 1, forecasts[, 1])
#' rel1 <- reliability(forecasts, observations)
#'
#' pred_obs <- tibble::tibble(M1 = runif(100), y = rbinom(100, 1, M1))
#' rel2 <- reliability(pred_obs)
#'
#' # Visualization
#' autoplot(rel1)
#'
#' library(ggplot2)
#' df_est <- estimates(rel1)
#' ggplot(df_est) +
#'   geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1)) +
#'   geom_path(aes(x = x, y = CEP, col = forecast)) +
#'   labs(x = "Forecast value", y = "Cond. event probability")
#'
#' @name reliability
NULL

#' @rdname reliability
#' @export
reliability <- function(x, y = NULL, ...) {
  x <- tibble::as_tibble(x)
  if (is.null(y)) {
    stopifnot("y" %in% names(x))
    y <- x$y
    x <- dplyr::select(x, !y)
  }
  y <- vec_cast(y, to = double())
  x <- dplyr::mutate_all(x, vec_cast, to = double())
  vec_cast(x, to = new_triptych_reliability(y = y))
}

new_triptych_reliability <- function(x = list(), y = double()) {
  new_vctr(x, y = y, class = "triptych_reliability")
}

# formatting
#' @export
format.triptych_reliability <- function(x, ...) {
  sprintf("<named list[%i]>", sapply(x, length))
}
#' @export
vec_ptype_abbr.triptych_reliability <- function(x, ..., prefix_named = FALSE, suffix_shape = TRUE) {
  "trpt_rel"
}

# coercion

vec_ptype2.triptych_reliability <- function(x, y, ..., x_arg = "", y_arg = "") {
  UseMethod("vec_ptype2.triptych_reliability")
}
#' @export
vec_ptype2.triptych_reliability.triptych_reliability <- function(x, y, ..., x_arg = "", y_arg = "") {
  if (!has_compatible_observations(x, y)) {
    stop_incompatible_type(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      details = "Observations are not compatible."
    )
  }
  new_triptych_reliability(list(), observations(x))
}

# casting

#' @param r A reference triptych_mcbdsc object whose attributes are used for casting.
#'
#' @rdname reliability
#' @export
as_reliability <- function(x, r) {
  stopifnot(inherits(r, "triptych_reliability"))
  x <- tibble::as_tibble(x)
  x <- dplyr::mutate_all(x, vec_cast, to = double())
  vec_cast(x, to = r)
}

vec_cast.triptych_reliability <- function(x, to, ...) {
  UseMethod("vec_cast.triptych_reliability")
}
#' @export
vec_cast.triptych_reliability.triptych_reliability <- function(x, to, ..., x_arg = "", to_arg = "") {
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
vec_cast.triptych_reliability.list <- function(x, to, ...) {
  x <- lapply(x, vec_cast, to = to)
  f <- \(...) vec_c(..., .name_spec = "{outer}_{inner}")
  do.call(f, x)
}
#' @export
vec_cast.triptych_reliability.data.frame <- function(x, to, ...) {
  x <- lapply(x, vec_cast, to = to)
  f <- \(...) vec_c(..., .name_spec = "{outer}_{inner}")
  do.call(f, x)
}
#' @export
vec_cast.triptych_reliability.tbl_df <- function(x, to, ...) {
  x <- lapply(x, vec_cast, to = to)
  f <- \(...) vec_c(..., .name_spec = "{outer}_{inner}")
  do.call(f, x)
}
#' @export
vec_cast.triptych_reliability.double <- function(x, to, ...) {
  y <- observations(to)
  ord <- order(x, -y)
  xo <- x[ord]
  xr <- monotone::monotone(y[ord])
  bins <- rle(xr)
  red_iKnots <- cumsum(bins$lengths)

  list(
    estimate = tibble::tibble(
      x_min = xo[c(0, utils::head(red_iKnots, -1)) + 1],
      x_max = xo[red_iKnots],
      CEP = bins$values
    ),
    region = list(),
    x = x
  ) |>
    list() |>
    new_triptych_reliability(y = y)
}

eval_diag.triptych_reliability <- function(x, at, ...) {
  purrr::map(x, at = at, .f = \(o, at) {
    pivot_longer(
      o$estimate,
      cols = dplyr::starts_with("x_"),
      values_to = "x"
    ) |>
      with(approx(x, CEP, xout = at, ties = list("ordered", mean))$y)
  })
}

#' @export
observations.triptych_reliability <- function(x, ...) {
  attr(x, "y")
}

#' @export
forecasts.triptych_reliability <- function(x, ...) {
  f <- function(o) tibble::tibble(x = o$x)
  g <- function(...) vec_rbind(..., .names_to = "forecast")
  purrr::map(x, f) |>
    do.call(g, args = _)
}

#' @importFrom tidyr pivot_longer
#'
#' @rdname estimates
#' @export
estimates.triptych_reliability <- function(x, at = NULL, ...) {
  f <- function(o) {
    tidyr::pivot_longer(
      data = o$estimate,
      cols = dplyr::starts_with("x_"),
      names_to = NULL,
      values_to = "x")
  }
  g <- if (is.null(at)) {
    f
  } else {
    function(o) {
      r <- with(f(o), approx(x, CEP, xout = at, ties = list("ordered", mean))$y)
      tibble::tibble(CEP = r, x = at)
    }
  }
  h <- function(...) vec_rbind(..., .names_to = "forecast")
  purrr::map(x, g) |>
    do.call(h, args = _)
}

#' @rdname regions
#' @export
regions.triptych_reliability <- function(x, ...) {
  if (!has_regions(x)) return(NULL)
  f <- function(o) o$region
  g <- function(...) vec_rbind(..., .names_to = "forecast")
  purrr::map(x, f) |>
    do.call(g, args = _)
}

#' @export
has_regions.triptych_reliability <- function(x, ...) {
  any(sapply(x, \(o) tibble::is_tibble(o$region)))
}

#' @export
add_confidence.triptych_reliability <- function(x, level = 0.9, method = "resampling_cases", ...) {
  m <- get(method)(x, level, ...)
  for (i in seq_along(x)) {
    x[[i]]$region <- m[[i]]
  }
  x
}

#' @export
add_consistency.triptych_reliability <- function(x, level = 0.9, method = "resampling_Bernoulli", ...) {
  m <- get(method)(x, level = level, position = "diagonal", ...)
  for (i in seq_along(x)) {
    x[[i]]$region <- m[[i]]
  }
  x
}

#' @rdname resampling_cases
#' @export
resampling_cases.triptych_reliability <- function(x, level = 0.9, n_boot = 1000, ...) {
  saved_seed <- .Random.seed
  y <- observations(x)
  purrr::map(
    .x = x,
    level = level,
    n_boot = n_boot,
    .f = function(o, level, n_boot) {
      xo <- unique(sort(o$x))
      bounds <- bootstrap_sample_cases(o$x, y, n_boot, reliability, xo) |>
        bootstrap_quantile(probs = 0.5 + c(-0.5, 0.5) * level)
      tibble::tibble(
        x = xo,
        lower = bounds[1L, ],
        upper = bounds[2L, ],
        method = paste0("resampling_cases_", n_boot),
        level = level
      )
    }
  )
}

#' @param position Either `"estimate"` for confidence regions, or `"diagonal"`
#'   for consistency regions.
#'
#' @rdname resampling_Bernoulli
#' @export
resampling_Bernoulli.triptych_reliability <- function(x, level = 0.9, n_boot = 1000, position = c("diagonal", "estimate"), ...) {
  saved_seed <- .Random.seed
  y <- observations(x)
  position <- match.arg(position)
  x0 <- switch(position, diagonal = expression(o$x), estimate = expression(recalibrate_mean2(o$x, y)))
  purrr::map(
    .x = x,
    level = level,
    n_boot = n_boot,
    .f = function(o, level, n_boot) {
      xo <- unique(sort(o$x))
      prob <- eval(x0)
      bounds <- bootstrap_sample_Bernoulli(o$x, prob, n_boot, reliability, xo) |>
        bootstrap_quantile(probs = 0.5 + c(-0.5, 0.5) * level)
      tibble::tibble(
        x = xo,
        lower = bounds[1L, ],
        upper = bounds[2L, ],
        method = paste0("resampling_Bernoulli_", n_boot),
        level = level
      )
    }
  )
}
