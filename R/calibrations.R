#' Prints a description of the calibration table
#' @param tbl x
#' @param ... additional arguments
#' @export
describe <- function(tbl, ...) UseMethod("describe")
#' @rdname describe
#' @aliases describe.cal.pbo
#' @S3method describe cal.pbo
#' @method describe cal.pbo
describe.cal.pbo <- function(tbl, ...){
  cat(tbl$description, ...)
}

#' Get a specific calibration table
#' @param tblname x
#' @param ... additional arguments
#' @export
get_caltbl <- function(tblname, describe.tbl=TRUE, ...) UseMethod("get_caltbl")
#' @rdname get_caltbl
#' @aliases get_caltbl.default
#' @S3method get_caltbl default
#' @method get_caltbl default
get_caltbl.default <- function(tblname, describe.tbl=TRUE, ...){
  alltbls <- 'bsmCalibrations'
  env <- new.env()
  do.call("data", list(alltbls, envir=env))
  TBLS <- env[[alltbls]]
  tbl <- TBLS[[tblname]]
  if (describe.tbl) describe(tbl)
  return(invisible(tbl))
}

#' Calibration matrix
#' @param tbl x
#' @param sta4 x
#' @param ... additional arguments
#' @export
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
#' }
calibration_matrix <- function(tbl, sta4, ...) UseMethod("calibration_matrix")
#' @rdname calibration_matrix
#' @aliases calibration_matrix.cal.pbo
#' @S3method calibration_matrix cal.pbo
#' @method calibration_matrix cal.pbo
calibration_matrix.cal.pbo <- function(tbl, sta4, typ=c('free','cdr','cd')){
  .rms <- function(x){
    sqrt(mean(x**2, na.rm=TRUE))
  }
  .norms <- function(M){
    sapply(eval(formals(base::norm)$type), norm, x=na.omit(M))
  }
  tblarr <- tbl[['caltbl.arr']]
  typ <- match.arg(typ)
  sta4 <- as.character(sta4)
  Sij <- matrix(as.matrix(tblarr[ , typ, sta4]), nrow=3, byrow=TRUE)
  dimnames(Sij) <- list(c("Ear","Gam1","Gam2"), paste0("CH",0:3))
  nfo <- rbind(means=rowMeans(Sij, na.rm=TRUE), RMS=apply(Sij, 1, .rms))
  nfo <- rbind(nfo, ratio=nfo[2,]/nfo[1,])
  attr(Sij, "typ") <- typ
  attr(Sij, "needs.pinv") <- FALSE
  attr(Sij, "norms") <- if (!all(is.na(Sij))){ .norms(t(Sij)) } else { NA }
  attr(Sij, "sizes") <- nfo
  Sij
}

#' @rdname calibration_matrix
#' @export
all_calibrations <- function(tbl, sta4, ...) UseMethod("all_calibrations")
#' @rdname calibration_matrix
#' @aliases all_calibrations.cal.pbo
#' @S3method all_calibrations cal.pbo
#' @method all_calibrations cal.pbo
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