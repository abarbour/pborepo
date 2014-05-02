#' Prints a description of the calibration table
#' 
#' @param tbl the calibration table to describe
#' @param ... additional arguments passed to \code{\link{cat}}
#' @export
#' @family Calibrations
describe <- function(tbl, ...) UseMethod("describe")

#' @rdname describe
#' @export
describe.default <- function(tbl, ...){
  cat(tbl$description, ...)
}

#' Get a specific calibration table
#' @param tblname character; the name of the calibration table
#' to import. See \code{\link{bsmCalibrations}}.  Defaults to
#' \code{"pbo"} if missing.
#' @param describe.tbl logical; should \code{\link{describe}}
#' be used to print information about the calibration table?
#' @param ... additional arguments
#' @export
#' @seealso \code{\link{bsmCalibrations}}
#' @family Calibrations
get_caltbl <- function(tblname, describe.tbl=TRUE, ...) UseMethod("get_caltbl")

#' @rdname get_caltbl
#' @aliases get_caltbl.default
#' @export
get_caltbl.default <- function(tblname, describe.tbl=TRUE, ...){
  if (missing(tblname)) tblname <- "pbo"
  alltbls <- 'bsmCalibrations'
  env <- new.env()
  do.call("data", list(alltbls, envir=env))
  TBLS <- env[[alltbls]]
  tbl <- TBLS[[tblname]]
  if (describe.tbl) describe(tbl)
  return(invisible(tbl))
}

#' Load or manipulate calibration matrices
#' 
#' @param tbl character; the name of the table to load; defaults to \code{"pbo"}
#' if missing.  The function will dispatch the appropriate method depending on the
#' class of the object 
#' @param sta4 character; the four-character station code, e.g. \code{'B084'}
#' @param typ  character; an optional method identifier
#' @param Sij matrix, or an object to be coerced into a matrix
#' @param needs.pinv logical; indicate whether the matrix should be pseudo-inverted
#' @param byrow logical; if \code{Sij} needs to be coerced into a matrix, should
#' it be filled `byrow'?
#' @param preference character; a vector of preferred calibration types (decreasing 
#' in preference with increasing index number); the first valid calibration matrix
#' is returned based on this preference
#' @param ... additional arguments
#' @export
#' @aliases calibrate calibration
#' 
#' @seealso \code{\link{pinv}} to calculate the pseudoinverse
#' 
#' \code{\link{station_data}} to find information about the PBO stations (including
#' \code{sta4})
#' 
#' @family Calibrations
#' @examples
#' \dontrun{
#' #
#' # select a calibration table
#' #  data(bsmCalibrations)
#' #  pbotbl <- bsmCalibrations[['pbo']]
#' #  describe(pbotbl)
#' # or simply
#' pbotbl <- get_caltbl("pbo")
#' # 
#' # indiviual tables
#' calibration_matrix(pbotbl, "B084")
#' calibration_matrix(pbotbl, "B082") # Full of NAs (no calib.)
#' #
#' # All of the available calibrations (note the loss of attributes)
#' all_calibrations(pbotbl, "B084") # all three are available
#' all_calibrations(pbotbl, "B082") # but there's at least one
#' #
#' # First available calibration
#' any_calibration(pbotbl, "B084")
#' #
#' }
calibration_matrix <- function(tbl, sta4, typ=c('free','cdr','cd'), ...) UseMethod("calibration_matrix")

#' @rdname calibration_matrix
#' @aliases calibration_matrix.cal.pbo
#' @export
calibration_matrix.cal.pbo <- function(tbl, sta4, typ=c('free','cdr','cd'), ...){
  #
  tblarr <- tbl[['caltbl.arr']]
  typ <- match.arg(typ)
  sta4 <- as.character(sta4)
  if (!(sta4 %in% dimnames(tblarr)[[3]])){
    stop(paste("station  --", sta4, "--  not found in this calibration table"))
  }
  Sij <- matrix(as.matrix(tblarr[ , typ, sta4]), nrow=3, byrow=TRUE)
  #
  fortify_calibration_matrix(Sij, sta4, typ, FALSE)
}

#' @rdname calibration_matrix
#' @aliases calibration_matrix.cal.hodg
#' @export
calibration_matrix.cal.hodg <- function(tbl, sta4, typ=NULL, ...) .NotYetImplemented()

#' @rdname calibration_matrix
#' @aliases calibration_matrix.cal.roel
#' @export
calibration_matrix.cal.roel <- function(tbl, sta4, typ=NULL, ...) .NotYetImplemented()

#' Pseudo-inverse of a matrix
#'
#' Uses singular value decomposition to calculate the pseudo-inverse of a matrix.  
#' 
#' @details
#' This can be useful when 
#' predictions of tensor strains are used to make
#' predictions on a gauge-by-gauge basis with strain data
#' 
#' The attribute \code{"dimnames"} is recycled; any other attribute will be lost.
#' 
#' @param Sij matrix, or an object to be coerced into a matrix
#' @param ... additional arguments
#' @export
#' 
#' @seealso 
#' \code{\link[corpcor]{pseudoinverse}} for details of the method
#' 
#' \code{\link{calibration_matrix}}
#' 
#' @examples
#' 
#' # get a calibration table
#' pbotbl <- get_caltbl("pbo")
#' 
#' # and the first available calibration matrix...
#' m <- any_calibration(pbotbl, "B082")
#' 
#' # Calculate the pseudoinverse
#' print(mi <- pinv(m))
#' 
#' # we can get back the original, less attributes of course
#' mo <- pinv(mi)
#' try( all.equal(m, mo, check.attributes=FALSE) )
pinv <- function(Sij, ...){
  if (!is.matrix(Sij)) Sij <- matrix(as.matrix(Sij))
  dn <- dimnames(Sij)
  dn.c <- dn[[1]]
  dn.r <- dn[[2]]
  iSij <- zapsmall( corpcor::pseudoinverse(Sij, ...) )
  dimnames(iSij) <- list(dn.r, dn.c)
  attr(iSij, "pseudoinverse") <- TRUE
  return(iSij)
}

#' @rdname calibration_matrix
#' @export
fortify_calibration_matrix <- function(Sij, sta4=NA, typ=NA, needs.pinv=FALSE, byrow=TRUE, ...){
  if (!is.matrix(Sij)) Sij <- matrix(as.matrix(Sij), nrow=3, byrow=byrow)
  dimnames(Sij) <- list(c("Ear","Gam1","Gam2"), paste0("CH",0:3))
  nfo <- rbind(means=rowMeans(Sij, na.rm=TRUE), rms=apply(Sij, 1, RMS))
  nfo <- rbind(nfo, ratio=nfo[2,]/nfo[1,])
  attr(Sij, "sta4") <- sta4
  attr(Sij, "typ") <- typ
  attr(Sij, "needs.pinv") <- needs.pinv
  attr(Sij, "norms") <- if (!all(is.na(Sij))){ norms(t(Sij)) } else { NA }
  attr(Sij, "sizes") <- nfo
  Sij
}

#' @rdname calibration_matrix
#' @export
all_calibrations <- function(tbl, sta4, ...) UseMethod("all_calibrations")
#' @rdname calibration_matrix
#' @aliases all_calibrations.cal.hodg
#' @export
all_calibrations.cal.hodg <- function(tbl, sta4, ...){
  .NotYetImplemented()
}
#' @rdname calibration_matrix
#' @aliases all_calibrations.cal.pbo
#' @export
all_calibrations.cal.pbo <- function(tbl, sta4, ...){
  caltypes <- eval(formals("calibration_matrix.cal.pbo")$typ)
  if (!is.null(caltypes)){
    ctyp <- caltypes[1]
    arr <- as.array(calibration_matrix(tbl, sta4, typ=ctyp, ...))
    for (ctyp in caltypes[-1]){
      arr <- abind(arr, as.array(calibration_matrix(tbl, sta4, typ=ctyp, ...)), along=3)
    }
    dimnames(arr)[[3]] <- caltypes
    attr(arr, "is.available") <- apply(!is.na(arr), 3, all)
    return(arr)
  }
}

#' @rdname calibration_matrix
#' @export
any_calibration <- function(tbl, sta4, preference=c("free","cdr","cd"), ...)  UseMethod("any_calibration")
 
#' @rdname calibration_matrix
#' @aliases any_calibration.cal.hodg
#' @export
any_calibration.cal.hodg <- function(tbl, sta4, preference=c("free","cdr","cd"), ...){
  .NotYetImplemented()
}
#' @rdname calibration_matrix
#' @aliases any_calibration.cal.pbo
#' @export
any_calibration.cal.pbo <- function(tbl, sta4, preference=c("free","cdr","cd"), ...){
  allcal <- all_calibrations(tbl, sta4)
  avail <- attr(allcal,"is.available")
  cal <- preference[preference %in% names(avail)[avail]][1]
  if (all(is.na(cal))){
    stop(paste("No valid calibration is available for station  --", sta4, "--"))
  } else {
    caltbl <- allcal[ , , cal]
    attr(caltbl,"cal.type") <- cal
    return(caltbl)
  }
}
