\name{unavco_downloader}
\alias{dlCatcher}
\alias{unavco_downloader}
\alias{unavco_downloader.default}
\title{Downloader for the UNAVCO data archive}
\usage{
  unavco_downloader(url.toget, temp, PLUGIN, quiet = FALSE,
    remove.temp = FALSE, ...)

  \method{unavco_downloader}{default} (url.toget, temp,
    PLUGIN, quiet = FALSE, remove.temp = FALSE, ...)

  dlCatcher(FUN)
}
\arguments{
  \item{url.toget}{character; the URL to download}

  \item{temp}{character; file for temporary writing}

  \item{quiet}{logical; should the downloader be verbose?}

  \item{...}{additional parameters to
  \code{\link{download.file}}}

  \item{FUN}{A function to try}
}
\description{
  Downloader for the UNAVCO data archive
}
\details{
  \code{\link{download.file}} is used with
  \code{cacheOK==FALSE}, and \code{mode=="w"}.

  Exception handling is performed with
  \code{\link{dlCatcher}}.
}
\seealso{
  Other data-retrieval: \code{\link{unavco_dataload}},
  \code{\link{unavco_dataload.default}},
  \code{\link{unavco_path}},
  \code{\link{unavco_temploader}},
  \code{\link{unavco_temploader.default}}
}

