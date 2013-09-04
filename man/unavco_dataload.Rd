\name{unavco_dataload}
\alias{unavco_dataload}
\alias{unavco_dataload.default}
\title{Retrieve and load data from the UNAVCO archive}
\usage{
  unavco_dataload(sta, year, day, dattype = c("pp", "pt"),
    default.sec.samp = 1, localfile = NULL, quiet = FALSE,
    impute = TRUE)

  \method{unavco_dataload}{default} (sta, year, day,
    dattype = c("pp", "pt"), default.sec.samp = 1,
    localfile = NULL, quiet = FALSE, impute = TRUE)
}
\arguments{
  \item{sta}{character;}

  \item{year}{numeric;}

  \item{day}{numeric;}

  \item{dattype}{character;}

  \item{default.sec.samp}{numeric;}

  \item{localfile}{character;}

  \item{quiet}{logical;}

  \item{impute}{logical;}
}
\description{
  Retrieve and load data from the UNAVCO archive
}
\details{
  \code{\link{unavco_path}} is used, \code{url.check=TRUE}
  enabled, to generate the file path(s)

  \code{\link{unavco_downloader}} is used to download the
  file(s)
}
\seealso{
  Other data-retrieval: \code{\link{dlCatcher}},
  \code{\link{unavco_downloader}},
  \code{\link{unavco_downloader.default}},
  \code{\link{unavco_path}},
  \code{\link{unavco_temploader}},
  \code{\link{unavco_temploader.default}}
}

