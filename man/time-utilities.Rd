\name{unavco_temp_toPOS}
\alias{unavco-time}
\alias{unavco_temp_setTZ}
\alias{unavco_temp_toDate}
\alias{unavco_temp_toPOS}
\title{Various time conversion functions}
\usage{
unavco_temp_toPOS(tstr, tz = "UTC")

unavco_temp_toDate(year, day, tz = "UTC")

unavco_temp_setTZ(df, tz = "UTC")
}
\arguments{
  \item{tstr}{character; time string}

  \item{tz}{character; time zone}

  \item{year}{numeric; the year}

  \item{day}{numeric; the day of \code{year}}

  \item{df}{data.frame to alter}
}
\description{
Various time conversion functions
}
\note{
These are \emph{very much} legacy functions, and will
probably evaporate in the future.
}

