\name{station_data}
\alias{sta16_from_sta4}
\alias{station_data}
\title{Station information from metadata}
\usage{
  station_data(name = NULL, meta = "bsm",
    use.regexp = FALSE)

  sta16_from_sta4(sta4 = NULL, meta = "bsm",
    use.regexp = FALSE)
}
\arguments{
  \item{name}{character; the string or \code{\link{regexp}}
  to use}

  \item{meta}{character; the metadata source}

  \item{use.regexp}{logical; should \code{name} be
  considered to be a \code{\link{regexp}}?}
}
\description{
  Station information from metadata
}
\examples{
station_data()
station_data("B084")
station_data("bcs", use.regexp=TRUE)
station_data(c("B082","B084"), use.regexp=TRUE) # will use only the first
station_data("B08[24]", use.regexp=TRUE) # gets what you want
#
# Get the 16-character codes
sta16_from_sta4(c("B082","B084"))
}

