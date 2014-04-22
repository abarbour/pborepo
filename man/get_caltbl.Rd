\name{get_caltbl}
\alias{get_caltbl}
\alias{get_caltbl.default}
\title{Get a specific calibration table}
\usage{
get_caltbl(tblname, describe.tbl = TRUE, ...)

\method{get_caltbl}{default}(tblname, describe.tbl = TRUE, ...)
}
\arguments{
  \item{tblname}{character; the name of the calibration
  table to import. See \code{\link{bsmCalibrations}}.
  Defaults to \code{"pbo"} if missing.}

  \item{describe.tbl}{logical; should
  \code{\link{describe}} be used to print information about
  the calibration table?}

  \item{...}{additional arguments}
}
\description{
Get a specific calibration table
}
\seealso{
\code{\link{bsmCalibrations}}

Other Calibrations: \code{\link{all_calibrations}},
\code{\link{all_calibrations.cal.hodg}},
\code{\link{all_calibrations.cal.hodg}},
\code{\link{all_calibrations.cal.pbo}},
\code{\link{all_calibrations.cal.pbo}},
\code{\link{any_calibration}},
\code{\link{any_calibration.cal.hodg}},
\code{\link{any_calibration.cal.hodg}},
\code{\link{any_calibration.cal.pbo}},
\code{\link{any_calibration.cal.pbo}},
\code{\link{calibration_matrix}},
\code{\link{calibration_matrix.cal.hodg}},
\code{\link{calibration_matrix.cal.hodg}},
\code{\link{calibration_matrix.cal.pbo}},
\code{\link{calibration_matrix.cal.pbo}},
\code{\link{calibration_matrix.cal.roel}},
\code{\link{calibration_matrix.cal.roel}},
\code{\link{fortify_calibration_matrix}};
\code{\link{describe}}, \code{\link{describe.default}},
\code{\link{describe.default}}
}

