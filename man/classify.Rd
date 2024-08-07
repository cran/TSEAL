% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Discriminant.R
\name{classify}
\alias{classify}
\title{Classifies observations based on a pretrained model.}
\usage{
classify(data, ...)
}
\arguments{
\item{data}{The data to be classified. This  data can be either the raw data
, or a MultiWaveAnalysis object generated earlier.}

\item{...}{Additional arguments}
}
\value{
A factor with predicted class of each observation
}
\description{
This function allows to classify observations based on a pretrained model
that could have been obtained in several ways (such as using the train model
function). T
}
\seealso{
\itemize{
\item \code{\link{trainModel}}
\item \code{\link{classify.array}}
\item \code{\link{classify.MultiWaveAnalysis}}
}
}
