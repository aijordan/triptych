#' Bootstrap case resampling for triptych objects
#'
#' @param x One of the triptych objects.
#' @param level A single value that determines which quantiles of
#'   the bootstrap sample to return. These quantiles envelop `level * n_boot`
#'   bootstrap draws.
#' @param n_boot The number of bootstrap samples.
#' @param ... Additional arguments passed to other methods.
#'
#' @return
#' A list of tibbles that contain the information to draw confidence regions.
#' The length is equal to the number of prediction methods in `x`.
#'
#' @examples
#' # Triptych construction
#' predictions <- matrix(runif(300), ncol = 3)
#' colnames(predictions) <- c("Method_1", "Method_2", "Method_3")
#' observations <- rbinom(100, 1, predictions[, 1])
#' tr <- triptych(predictions, observations)
#'
#' resampling_cases(tr$murphy, n_boot = 50)
#' resampling_cases(tr$reliability, n_boot = 50)
#' resampling_cases(tr$roc, n_boot = 50)
#'
#' @export
resampling_cases <- function(x, level = 0.9, n_boot = 1000, ...) {
  UseMethod("resampling_cases")
}

#' Bootstrap observation resampling for triptych objects
#'
#' @param x One of the triptych objects.
#' @param level A single value that determines which quantiles of
#'   the bootstrap sample to return. These quantiles envelop `level * n_boot`
#'   bootstrap draws.
#' @param n_boot The number of bootstrap samples.
#' @param ... Additional arguments passed to other methods.
#'
#' @return
#' A list of tibbles that contain the information to draw confidence regions.
#' The length is equal to the number of prediction methods in `x`.
#'
#' @examples
#' # Triptych construction
#' predictions <- matrix(runif(300), ncol = 3)
#' colnames(predictions) <- c("Method_1", "Method_2", "Method_3")
#' observations <- rbinom(100, 1, predictions[, 1])
#' tr <- triptych(predictions, observations)
#'
#' resampling_cases(tr$murphy, n_boot = 50)
#' resampling_cases(tr$reliability, position = "estimate", n_boot = 50)
#' resampling_cases(tr$roc, n_boot = 50)
#'
#' @export
resampling_Bernoulli <- function(x, level = 0.9, n_boot = 1000, ...) {
  UseMethod("resampling_Bernoulli")
}

# todo
resampling_residuals <- function(x, ...) {
  UseMethod("resampling_residuals")
}

bootstrap_sample_cases <- function(x, y, n_boot, diagram, at, ...) {
  stopifnot(length(x) == length(y))
  n <- length(x)
  evl <- \(x) eval_diag(x, at = at, ...)
  replicate(n_boot, simplify = FALSE, {
    s <- sample(n, replace = TRUE)
    diagram(x[s], y = y[s]) |> evl()
  })
}

#' @importFrom stats rbinom
bootstrap_sample_Bernoulli <- function(x, prob, n_boot, diagram, at, ..., resample_x = TRUE) {
  stopifnot(length(x) == length(prob))
  n <- length(x)
  evl <- \(x) eval_diag(x, at = at, ...)
  if (isTRUE(resample_x)) {
    return(replicate(n_boot, simplify = FALSE, {
      s <- sample(n, replace = TRUE)
      diagram(x[s], y = stats::rbinom(n, 1, prob[s])) |> evl()
    }))
  }
  replicate(n_boot, simplify = FALSE, {
    diagram(x, y = stats::rbinom(n, 1, prob)) |> evl()
  })
}

bootstrap_quantile <- function(l, probs) {
  # l is a list from 'replicate' with 'simplify = FALSE'
  # each list element comes from 'eval_diag' which is usually also a list
  unlist(l, recursive = TRUE) |>
    matrix(ncol = length(l)) |>
    apply(MARGIN = 1L, FUN = stats::quantile, probs = probs, na.rm = TRUE, type = 1)
}
