% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{naOP}
\alias{naOP}
\alias{naOP.default}
\alias{fillNA}
\alias{locfNA}
\alias{interpNA}
\title{Automatically decide how to best fill in NA sections}
\usage{
naOP(d, ...)

\method{naOP}{default}(d, fill = 0, verbose = TRUE, ...)

fillNA(d, fill = 0, ...)

locfNA(d, ...)

interpNA(d, ...)
}
\arguments{
\item{d}{data}

\item{...}{additional arguments}

\item{fill}{value to fill}

\item{verbose}{logical; should messages be given?}
}
\description{
Automatically decide how to best fill in NA sections
}
\seealso{
\code{\link{unavco-methods}}
}
