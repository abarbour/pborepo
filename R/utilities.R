#' Station information from PBO metadata
#' 
#' PBO operates a large number of stations, and \code{\link{station_data}} is
#' a mechanism to search through a subset of their metadata; 
#' \code{\link{sta16_from_sta4}} is a simple way to convert from a station's
#' 4-character station code (e.g., B084) to its 
#' 16-character representation (e.g., pinyon084bcs2006).
#' 
#' @details This function searches internal metadata, which can be accessed
#' via \code{data(bsmmeta)}, for example.
#' 
#' @param name character; the search string; use \code{use.regexp=TRUE} to use it
#' as a regular expression
#' @param meta character; the metadata source; currently the only
#' option is \code{'bsm'}, which accesses
#' PBO-BSM station information (see \code{data('bsmmeta')})
#' @param use.regexp logical; should \code{name} be considered to be a regular expression
#' (c.f. \code{\link{regexp}})?
#' @param verbose logical; should messages be given?
#' @param sta4 character; an optional 4-character station code (e.g., \code{'B084'})
#' 
#' @author A. Barbour
#' @export
#' @family utilities
#' @examples
#' station_data()
#' station_data("B084")
#' station_data("bcs", use.regexp=TRUE)
#' station_data(c("B082","B084"), use.regexp=TRUE) # will use only the first
#' station_data("B08[24]", use.regexp=TRUE) # gets what you want
#' #
#' # Get all of the 16-character codes
#' sta16_from_sta4()
#' # or just a few
#' sta16_from_sta4(c("B082","B084"))
station_data <- function(name=NULL, meta="bsm", use.regexp=FALSE, verbose=TRUE){
  metao <- match.arg(meta)
  meta <- switch(metao, bsm="bsmmeta")
  env <- new.env()
  do.call("data", list(meta, package="pborepo", envir=env))
  metad <- env[[meta]]
  #print(metad)
  nms <- switch(metao, bsm=c("coords"))
  dat <- metad[[nms]]
  #> head(bsmmeta$coords)
  #sta4             sta16     nlat      elon elev.m    gham.geocode
  #1 B001  golbeck01bwa2005 48.04307 -123.1314 237.00    E4X4P1E2X3Y0
  #2 B003  floequarybwa2005 48.06236 -124.1409 284.66    E4T1T9U2V1W4
  if (!is.null(name)){
    name <- as.character(name)
    if (use.regexp){
      #dat[regexpr("bcs",dat$sta16)>0,]
      name <- name[1]
      logics <- sapply(dat, function(x, pat){regexpr(pat, x, ignore.case=TRUE) > 0}, pat=name)
      nrd <- nrow(dat)
      logics <- which(logics) %% nrd
      logics[logics==0] <- nrd
      dat <- dat[logics, ]
    } else {
      if (verbose) message("can also set 'use.regexp=TRUE'")
      dat <- dat[dat==name, ]      
    }
  }
  rownames(dat) <- NULL
  return(dat)
}
#' @rdname station_data
#' @export
sta16_from_sta4 <- function(sta4=NULL, meta="bsm", use.regexp=FALSE){
  s16 <- "sta16"
  stadat <- if (!is.null(sta4)){
    sapply(X=sta4, FUN=function(x, MET=meta, REG=use.regexp){station_data(name=x, meta=MET, use.regexp=REG)[[s16]]})
  } else {
    station_data(meta=meta, use.regexp=use.regexp)[[s16]]
  }
  return(as.vector(stadat))
}

#' List .txt.gz files in a directory
#' @details
#' Uses \code{\link{list.dirs}} with:
#' \code{full.names = TRUE, recursive = TRUE}
#' and \code{\link{list.files}} with:
#' \code{full.names = TRUE, ignore.case = FALSE}
#' 
#' @param datadir character; the location of the .txt.gz files
#' @param basedir character; the location of \code{datadir}
#' @param pattern character; the regexp (perl style) to use for the listing
#' @param ... additional parameters passed to \code{\link{list.files}}
#' 
#' @export
#' @family utilities
list.txtgz <- function(datadir=".", basedir=getwd(), pattern="txt.gz$", ...){
  #### GET FILES IN A DIRECTORY
  dl <- list.dirs(datadir, full.names=TRUE, recursive = TRUE)
  fla <- list.files(path=dl, pattern=pattern, full.names=TRUE, ignore.case = FALSE, ...)
  fulldir <- paste(basedir, datadir, sep="/")
  return(list(basedir=basedir, datadir=datadir, dir=fulldir, files=fla))
}

#' Consistent messaging for downloading and other functions
#' @param ... the messages to give to \code{\link{message}}
#' @param lead.char character; the leading character
#' @param n numeric; the number of \code{lead.char} to replicate
#' @export
#' @family utilities
pbo_message <- function(..., lead.char=c("+",">","!"), n=8){
  msg <- paste(...)
  lead.char <- match.arg(lead.char)
  fullmsg <- paste(paste(rep(lead.char, n), collapse=""), msg, collapse="\n")
  message(fullmsg)
  return(invisible(msg))
}

#### TIME CONVERSIONS
#' Various time conversion functions
#' 
#' @note
#' These are \emph{very much} legacy functions, and will
#' probably evaporate in the future.
#' 
#' @export
#' 
#' @param tstr character; time string
#' @param tz character; time zone
#' @param year numeric; the year
#' @param day numeric; the day of \code{year}
#' @param df data.frame to alter
#' 
#' @rdname time-utilities
#' @aliases unavco-time
#' @family utilities time-conversion
unavco_temp_toPOS <- function(tstr, tz="UTC"){
  #2008-12-31 21:00:00.81
  base::as.POSIXct(paste(tstr,tz),format="%Y-%m-%d %H:%M:%S",tz=tz)
}
#' @rdname time-utilities
#' @export
unavco_temp_toDate <- function(year, day, tz="UTC"){
  base::as.Date(paste(year,day),format="%Y %j",tz=tz)
}
#' @rdname time-utilities
#' @export
unavco_temp_setTZ <- function(df, tz="UTC"){
  #### some functions change the POSIX timezone, this returns it to normal
  df$Dt. <- base::as.POSIXlt(df$Dt., tz=tz)
  return(df)
}

### Various functions
#' Calculate \emph{all} norms of a matrix
#' 
#' @details
#' \code{\link{na.omit}} is used on \code{M} prior to calculation
#' 
#' @seealso \code{\link{norm}}
#' @export
#' 
#' @param M matrix
#' @param types character; optionally, specify the norms you wish to calculate
#' e.g., O, I, F, M, 2
#' 
#' @family utilities 
#' @examples
#' M <- matrix(1:12,4)
#' norms(M)
#' #
#' # or manually specify
#' norms(M, c("O","M"))
norms <- function(M, types=NULL){
  if (is.null(types)) types <- eval(formals(base::norm)$type)
  sapply(types, base::norm, x=na.omit(M))
}

#' Trimmed root mean square (RMS)
#' 
#' Calculates the root mean square of a sequence, which
#' is defined as the square-root of the squared arithmetic
#' mean.
#' 
#' @details
#' The functions
#' \code{\link{rowRMS}} and  \code{\link{colRMS}} cannot accept \code{trim}
#' arguments because they use  \code{\link{rowMeans}}
#' and  \code{\link{colMeans}}, respectively.
#' 
#' @export
#' @aliases rms
#' 
#' @param x [from \code{\link{mean}}:]\emph{ An R object. Complex vectors 
#' are allowed for trim = 0, only.}
#' @param trim numeric; [from \code{\link{mean}}:]\emph{
#' the fraction (0 to 0.5) of observations to be trimmed 
#' from each end of x before the mean is computed. 
#' Values of trim outside that range are taken as the nearest endpoint.}
#' Note that any trimming occurs prior to calculation of arithmetic mean.
#' @param na.rm  [from \code{\link{mean}}:]\emph{
#' a logical value indicating whether \code{NA} values should be stripped before 
#' the computation proceeds.}
#' @param ... additional parameters given to either
#' \code{\link{rowMeans}}, or \code{\link{colMeans}}
#' @references
#' [1] \url{http://en.wikipedia.org/wiki/Root_mean_square}
#' @family utilities 
#' @examples
#' x <- sin(seq(-pi,pi,by=0.05))
#' xr <- RMS(x)
#' # this:
#' RMS(c(4,9))
#' # should equal ~6.96
#' #
#' # The expected value for a default \code{sin} curve is sqrt(2)/2
#' plot(x, type="l")
#' lines(abs(x), col="red")
#' abline(h=c(mean(x), mean(abs(x)), sqrt(2)/2, xr), lty=4:1, col=4:1)
#' #
#' M <- matrix(1:12,3)
#' colRMS(M)
#' rowRMS(M)
RMS <- function(x, trim = 0, na.rm=TRUE){
  sqrt(mean(x*x, trim=trim, na.rm=na.rm))
}
#' @rdname RMS
#' @export
colRMS <- function(x, na.rm=TRUE, ...){
  x <- as.matrix(x)
  sqrt(colMeans(x*x, na.rm=na.rm, ...))
}
#' @rdname RMS
#' @export
rowRMS <- function(x, na.rm=TRUE, ...){
  x <- as.matrix(x)
  sqrt(rowMeans(x*x, na.rm=na.rm, ...))
}
