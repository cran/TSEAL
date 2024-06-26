% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MultiWaveAnalisysParallel.R
\name{chooseLevel}
\alias{chooseLevel}
\title{Select the DWT level of decomposition based on wavelet filter, data series
length and a user choice}
\usage{
chooseLevel(choice, filter, N)
}
\arguments{
\item{choice}{Valid values:
\itemize{
\item "Conservative" : \eqn{J < log_2 ( N / (L - 1) + 1)}{J < log2(N / (L - 1) + 1)}
\item "Max" : \eqn{J \leq log_2(N)}{J <= log2(N) }
\item "Supermax" : \eqn{ J \leq log_2(1.5 * N)}{J <= log2(1.5 * N)}
}}

\item{filter}{Wavelet transform filter name. To see the available filters use
the function \code{\link{availableFilters}}}

\item{N}{Number of observations. Must be a positive integer}
}
\value{
Number of level of decomposition based in selection criteria
}
\description{
Select the DWT level of decomposition based on wavelet filter, data series
length and a user choice
}
\examples{
lev <- chooseLevel("conservative", "haar", 8)
}
\references{
Percival, D. B. and A. T. Walden (2000) Wavelet Methods for
Time Series Analysis. Cambridge: Cambridge University Press.
}
