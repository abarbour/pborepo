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
# @import Matrix corpcor
# not needed since specified in DESCRIPTION-DEPENDS: stats utils graphics grDevices
# @useDynLib strain
#'
#' @seealso \code{\link{data-retrieval}}
#' \code{\link{utilities}}
#' 
#' @family overviews
#'
NULL
.pboEnvName = ".pboEnv"
.pboEnv = new.env()

#' Constants used as defaults
#' 
#' @details The helper function \code{\link{pbo_constants}}
#' shows (the structure of, optionally)
#' and returns \code{.constants}.
#' 
#' @name pborepo-constants
#' @seealso \code{\link{pborepo}}
#' @family overviews
.constants = list(
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
#' @param do.str logical; should the structure be printed?
#' @export
# @example
# pbo_constants()
pbo_constants <- function(do.str=TRUE){
  const <- pborepo:::.constants
  if (do.str) str(const, comp.str = "++++++++\n\t", no.list=TRUE, digits.d = 9)
  return(invisible(const))
}

##
## Datasets
## 
##   Filter weights
#
# @title Minimum phase lowpass filter weights for strain data
# @references
# D.C. Agnew and K. Hodgkinson (2007),
# Designing compact causal digital filters for low-frequency strainmeter data,
# Bulletin of the Seismological Society of America,
# vol 97, 1B, 1-99, doi: 10.1785/0120060088
# @name minphs
# @docType data
# @format A list with filter weights by decimation factor
#NULL

#' @title BSM metadata
#' @name bsmmeta
#' @docType data
NULL