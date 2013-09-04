\name{unavco_path}
\alias{unavco_path}
\title{Generate the appropriate path for UNAVCO data retrieval}
\usage{
  unavco_path(sta4, year, jday, type = c("pp", "pt"),
    url.check = FALSE)
}
\arguments{
  \item{sta4}{character; 4-character station name (e.g.
  B084).}

  \item{year}{numeric; year of data}

  \item{jday}{numeric; Julian date (day of year)}

  \item{type}{character; the type of data to generate
  download data for}

  \item{url.check}{logical; should the urls be tested for
  existence?}
}
\description{
  Returns the appropriate url to download a .txt.gz data
  file from the UNAVCO data archive.
}
\details{
  If \code{url.check==TRUE} the function \code{url.exists}
  is used to test the status of each link; this can add a
  substantial amount of processing time for large numbers
  of urls.
}
\examples{
unavco_path("B084", 2010, 1)
unavco_path("B084", 2010, 1:10) # use vectors for multiple urls
unavco_path("B084",2010, 1:10, "pt")
# Also try:
# unavco_path("B084",2010, 1:10, "pt", TRUE)
}
\seealso{
  Other data-retrieval: \code{\link{dlCatcher}},
  \code{\link{unavco_dataload}},
  \code{\link{unavco_dataload.default}},
  \code{\link{unavco_downloader}}
}

