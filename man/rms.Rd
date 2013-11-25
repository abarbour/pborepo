\name{rms}
\alias{colRms}
\alias{rms}
\alias{rowRms}
\title{Trimmed RMS (root mean square)}
\usage{
  rms(x, trim = 0, na.rm = TRUE)

  colRms(x, na.rm = TRUE, ...)

  rowRms(x, na.rm = TRUE, ...)
}
\arguments{
  \item{x}{[from \code{\link{mean}}:]\emph{ An R object.
  Complex vectors are allowed for trim = 0, only.}}

  \item{trim}{numeric; [from \code{\link{mean}}:]\emph{ the
  fraction (0 to 0.5) of observations to be trimmed from
  each end of x before the mean is computed. Values of trim
  outside that range are taken as the nearest endpoint.}}

  \item{na.rm}{[from \code{\link{mean}}:]\emph{ a logical
  value indicating whether NA values should be stripped
  before the computation proceeds.}}

  \item{...}{additional parameters}
}
\description{
  Trimmed RMS (root mean square)
}
\details{
  The functions \code{\link{rowRms}} and
  \code{\link{colRms}} cannot accept \code{trim} arguments
  because they use \code{\link{rowMeans}} and
  \code{\link{colMeans}}, respectively.
}
\examples{
x <- sin(seq(-pi,pi,by=0.1))
xr <- rms(x)
#
# The expected value for a default \\code{sin} curve is sqrt(2)/2
plot(x, type="l")
lines(abs(x), col="red")
abline(h=c(mean(x), mean(abs(x)), sqrt(2)/2, xr), lty=4:1, col=4:1)
#
M <- matrix(1:12,3)
colRms(M)
rowRms(M)
}
\references{
  [1] \url{http://en.wikipedia.org/wiki/Root_mean_square}
}
\seealso{
  Other utilities: \code{\link{list.txtgz}},
  \code{\link{norms}}, \code{\link{pbo_message}}
}

