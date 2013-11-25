\name{calibration_matrix}
\alias{all_calibrations}
\alias{all_calibrations.cal.pbo}
\alias{calibration_matrix}
\alias{calibration_matrix.cal.hodg}
\alias{calibration_matrix.cal.pbo}
\alias{calibration_matrix.cal.roel}
\alias{fortify_calibration_matrix}
\title{Calibration matrix}
\usage{
  calibration_matrix(tbl, sta4, ...)

  \method{calibration_matrix}{cal.pbo} (tbl, sta4,
    typ = c("free", "cdr", "cd"))

  \method{calibration_matrix}{cal.hodg} ()

  \method{calibration_matrix}{cal.roel} ()

  fortify_calibration_matrix(Sij, sta4 = NA, typ = NA,
    needs.pinv = FALSE, byrow = TRUE, ...)

  all_calibrations(tbl, sta4, ...)

  \method{all_calibrations}{cal.pbo} (tbl, sta4, ...)
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

