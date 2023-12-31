% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/triptych_reliability.R
\name{reliability}
\alias{reliability}
\alias{as_reliability}
\title{Evaluation of forecasts using reliability curves}
\usage{
reliability(x, y_var = "y", ..., y = NULL)

as_reliability(x, r)
}
\arguments{
\item{x}{A data frame, list, matrix, or other object that can be coerced to a tibble. Contains numeric forecasts, and observations (optional).}

\item{y_var}{A variable in \code{x} that contains observations. Specified as the argument \code{var}in \code{\link[dplyr:pull]{dplyr::pull()}}.}

\item{...}{Unused.}

\item{y}{A numeric vector of observations. If supplied, overrides \code{y_var}. Otherwise, defaults to \code{dplyr::pull(x, y_var)}.}

\item{r}{A reference triptych_mcbdsc object whose attributes are used for casting.}
}
\value{
A \code{triptych_reliability} object, that is a \code{vctrs_vctr} subclass, and has
a length equal to number of forecasting methods supplied in \code{x}. Each entry
is named according to the corresponding forecasting method,
and contains a list of named objects:
\itemize{
\item \code{estimate}: A data frame with the isotonic regression estimate.
\item \code{region}: Either an empty list, or a data frame of pointwise consistency
or confidence intervals
added by \code{\link[=add_consistency]{add_consistency()}} or \code{\link[=add_confidence]{add_confidence()}}, respectively.
\item \code{x}: The numeric vector of original forecasts.
}
Access is most convenient through \code{\link[=estimates]{estimates()}}, \code{\link[=regions]{regions()}}, and \code{\link[=forecasts]{forecasts()}}.
}
\description{
A reliability curve visualizes miscalibration by displaying the (isotonic)
conditional event probability against the forecast value.
}
\examples{
data(ex_binary, package = "triptych")

rel <- reliability(ex_binary)
rel

# 1. Choose 4 predictions
# 2. Visualize
# 3. Adjust the title of the legend
rel[c(1, 3, 6, 9)] |>
  autoplot() +
  ggplot2::guides(colour = ggplot2::guide_legend("Forecast"))
  
# Build yourself using accessors
library(ggplot2)
df_est <- estimates(rel[c(1, 3, 6, 9)])
ggplot(df_est, aes(x = x, y = CEP, col = forecast)) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1)) +
  geom_path()

}
\seealso{
Accessors: \code{\link[=estimates]{estimates()}}, \code{\link[=regions]{regions()}}, \code{\link[=forecasts]{forecasts()}}, \code{\link[=observations]{observations()}}

Adding uncertainty quantification: \code{\link[=add_confidence]{add_confidence()}}

Visualization: \code{\link[=plot.triptych_reliability]{plot.triptych_reliability()}}, \code{\link[=autoplot.triptych_reliability]{autoplot.triptych_reliability()}}
}
