\docType{data}
\name{pborepo-constants}
\alias{constants}
\alias{pbo_constants}
\alias{pborepo-constants}
\title{Constants used as defaults}
\format{\preformatted{List of 3
 $ unavco     :List of 5
  ..$ general: chr "http://borehole.unavco.org/"
  ..$ bsm    : chr "http://bsm.unavco.org/bsm/"
  ..$ pp     : chr "http://pore.unavco.org/pore"
  ..$ pt     : chr "http://pore.unavco.org/pore"
  ..$ tilt   : chr "http://tilt.unavco.org/tilt/"
 $ conversions:List of 4
  ..$ hpa_to_pa   : num 100
  ..$ hpa_to_kpa  : num 0.1
  ..$ to_radians  : num 0.0175
  ..$ from_radians: num 57.3
 $ bsm        :List of 5
  ..$ R                    : num 1e+08
  ..$ diam                 : num 0.087
  ..$ gaps                 : num [1:2] 1e-04 2e-04
  ..$ relative_orientations: num [1:4] 0 -60 60 30
  ..$ gauge_names          :List of 3
  .. ..$ pbo : num [1:4] 0 1 2 3
  .. ..$ hodg: num [1:4] 1 2 3 4
  .. ..$ ext : num [1:4] 2 1 0 3
}}
\usage{
.pbo_constants

constants(do.str = TRUE)

pbo_constants()
}
\arguments{
  \item{do.str}{logical; should the structure be printed?}
}
\description{
Constants used as defaults
}
\details{
The helper function \code{\link{constants}} shows (the
structure of, optionally) and returns
\code{.pbo_constants}. \code{\link{pbo_constants}} simply
accesses \code{\link{constants}}
}
\examples{
str(pbo_constants())
}
\seealso{
\code{\link{pborepo}}
}
\keyword{datasets}

