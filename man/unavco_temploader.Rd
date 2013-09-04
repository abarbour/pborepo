\name{unavco_temploader}
\alias{unavco_temploader}
\alias{unavco_temploader.default}
\title{Load and process data downloaded from the UNAVCO archive}
\usage{
  unavco_temploader(file, dattype = c("pp", "pt"), ...)

  \method{unavco_temploader}{default} (file,
    dattype = c("pp", "pt"), ...)
}
\arguments{
  \item{file}{character; the file to be loaded}

  \item{dattype}{character;}

  \item{...}{additional parameters}
}
\description{
  Load and process data downloaded from the UNAVCO archive
}
\seealso{
  Other data-retrieval: \code{\link{dlCatcher}},
  \code{\link{unavco_dataload}},
  \code{\link{unavco_dataload.default}},
  \code{\link{unavco_downloader}},
  \code{\link{unavco_downloader.default}},
  \code{\link{unavco_path}}
}

