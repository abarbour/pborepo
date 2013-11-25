\name{norms}
\alias{norms}
\title{Calculate \emph{all} norms of a matrix}
\usage{
  norms(M, types = NULL)
}
\arguments{
  \item{M}{matrix}

  \item{types}{character; optionally specify the norms you
  wish to calculate e.g., O, I, F, M, 2}
}
\description{
  Calculate \emph{all} norms of a matrix
}
\examples{
M <- matrix(1:12,4)
norms(M)
#
# or manually specify
norms(M, c("O","M"))
}
\seealso{
  \code{\link{norm}}

  Other utilities: \code{\link{colRms}},
  \code{\link{list.txtgz}}, \code{\link{pbo_message}},
  \code{\link{rms}}, \code{\link{rowRms}}
}

