\name{unavco_downloader}
\alias{dlCatcher}
\alias{unavco_downloader}
\title{Downloader for the UNAVCO data archive}
\usage{
  unavco_downloader(url.toget, temp = tempfile(),
    quiet = FALSE, ...)

  dlCatcher(FUN)
}
\arguments{
  \item{url.toget}{character; the URL to download}

  \item{temp}{character; file for temporary writing}

  \item{quiet}{logical; should the downloader be verbose?}

  \item{...}{additional parameters}

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
  \code{\link{unavco_path}}
}

