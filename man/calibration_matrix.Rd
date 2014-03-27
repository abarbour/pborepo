\name{calibration_matrix}
\alias{all_calibrations}
\alias{all_calibrations.cal.hodg}
\alias{all_calibrations.cal.pbo}
\alias{any_calibration}
\alias{any_calibration.cal.hodg}
\alias{any_calibration.cal.pbo}
\alias{calibration_matrix}
\alias{calibration_matrix.cal.hodg}
\alias{calibration_matrix.cal.pbo}
\alias{calibration_matrix.cal.roel}
\alias{fortify_calibration_matrix}
\title{Calibration matrix}
\usage{
calibration_matrix(tbl, sta4, ...)

\method{calibration_matrix}{cal.pbo}(tbl, sta4, typ = c("free", "cdr", "cd"))

\method{calibration_matrix}{cal.hodg}(tbl, sta4, ...)

\method{calibration_matrix}{cal.roel}(tbl, sta4, ...)

fortify_calibration_matrix(Sij, sta4 = NA, typ = NA, needs.pinv = FALSE,
  byrow = TRUE, ...)

all_calibrations(tbl, sta4, ...)

\method{all_calibrations}{cal.hodg}(tbl, sta4, ...)

\method{all_calibrations}{cal.pbo}(tbl, sta4, ...)

any_calibration(tbl, sta4, preference = c("free", "cdr", "cd"), ...)

\method{any_calibration}{cal.hodg}(tbl, sta4, preference = c("free", "cdr",
  "cd"), ...)

\method{any_calibration}{cal.pbo}(tbl, sta4, preference = c("free", "cdr",
  "cd"), ...)
}
\arguments{
  \item{tbl}{x}

  \item{sta4}{character; an optional station name}

  \item{typ}{character; an optional method identifier}

  \item{Sij}{matrix, or an object to be coerced into a
  matrix}

  \item{needs.pinv}{logical; indicate whether the matrix
  should be pseudo-inverted}

  \item{byrow}{logical; if \code{Sij} needs to be coerced
  into a matrix, should it be filled `byrow'?}

  \item{preference}{character; a vector of preferred
  calibration types (decreasing in preference with
  increasing index number); the first valid calibration
  matrix is returned based on this preference}

  \item{...}{additional arguments}
}
\description{
Calibration matrix
}
\examples{
\dontrun{
#
# select a calibration table
#  data(bsmCalibrations)
#  pbotbl <- bsmCalibrations[['pbo']]
#  describe(pbotbl)
# or simply
pbotbl <- get_caltbl("pbo")
#
# indiviual tables
calibration_matrix(pbotbl, "B084")
calibration_matrix(pbotbl, "B082") # Full of NAs (no calib.)
#
# All of the available calibrations (note the loss of attributes)
all_calibrations(pbotbl, "B084") # all three are available
all_calibrations(pbotbl, "B082") # but there's at least one
#
}
}
\seealso{
Other Calibrations: \code{\link{describe}},
\code{\link{describe.default}},
\code{\link{describe.default}}; \code{\link{get_caltbl}},
\code{\link{get_caltbl.default}},
\code{\link{get_caltbl.default}}
}

