#' @title Access to data-products and metadata from the Plate Boundary Observatory geodetic network
#'
#' @description
#' Some description
#'
#' @details
#' See the following pages for details on
#' 
#' @section Pore-pressure data:
#' \url{http://pore.unavco.org/pore}
#' 
#' @section Pore-temperature data:
#' \url{http://pore.unavco.org/pore}
#'
#' @docType package
#' @name pborepo-package
#' @aliases pborepo
#' 
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' 
#' @import corpcor abind tidyverse zoo
#' @importFrom plyr rbind.fill
#'
#' @seealso 
#' See \code{\link{data-retrieval}} for functions related to data retrieval
#' 
#' See \code{\link{station_data}} for example, for utility functions
#' 
#' See \code{\link{calibration_matrix}} for example, for
#' functions associated with strain
#' calibration coefficients.
#' 
#' @family overviews
#'
NULL
.pboEnvName = ".pboEnv"
.pboEnv = new.env()

#' Constants used as defaults
#' 
#' @details The helper function \code{\link{constants}}
#' shows (the structure of, optionally)
#' and returns \code{.pbo_constants}. 
#' \code{\link{pbo_constants}} simply accesses \code{\link{constants}}
#' 
#' @param do.str logical; should the structure be printed?
#' 
#' @name pborepo-constants
#' @export
#' 
#' @seealso \code{\link{pborepo}}
#' @examples
#' str(pbo_constants())
.pbo_constants = list(
  unavco=list(
    general="http://borehole.unavco.org/",
    bsm="http://bsm.unavco.org/bsm/",
    pp="http://pore.unavco.org/pore",
    pt="http://pore.unavco.org/pore",
    tilt="http://tilt.unavco.org/tilt/"
  ),
  conversions=list(
    hpa_to_pa=100/1,
    hpa_to_kpa=1/10,
    to_radians=pi/180,
    from_radians=180/pi
  ),
  bsm=list(R=1e8,
           diam=87e-3,
           gaps=c(100,200)*1e-6,
           relative_orientations=c(0,-60,60,30), # clockwise (convention?)
           gauge_names=list(pbo=c(0,1,2,3),
                            hodg=c(1,2,3,4),
                            ext=c(2,1,0,3)
                            )
  )
)

#' @rdname pborepo-constants
#' @export
constants <- function(do.str=TRUE){
  const <- pborepo::.pbo_constants
  if (do.str) str(const, comp.str = "++++++++\n\t", no.list=TRUE, digits.d = 9)
  return(invisible(const))
}

#' @rdname pborepo-constants
#' @export
pbo_constants <- function(){
  pborepo::constants(do.str=FALSE)
}

##
## Datasets
##

#' @title BSM metadata
#' @name bsmmeta
#' @docType data
NULL

#' @title BSM metadata 2
#' @name bsmmeta2
#' @docType data
NULL

##   calibration coeffs:
#
#' @title BSM calibration coefficients
#' @name bsmCalibrations
#' @docType data
#' @format A list with lists of dataframes with
#' calibration coefficients by station
#' @examples
#' data(bsmCalibrations)
#' summary(bsmCalibrations)
#' #      Length Class  Mode
#' #grant 2      -none- list
#' #hodg  4      -none- list
#' #pbo   1      -none- list
#' #roel  2      -none- list
NULL
