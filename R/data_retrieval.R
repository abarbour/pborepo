#/Users/abarbour/kook.processing/R/dev/timetasks/merge/funcs.R

#' Retrieve and load data from the UNAVCO archive
#'
#' @description
#' The user should be most interested in \code{\link{pore.pressure}} and
#' \code{\link{pore.temperature}} to get pore-fluid pressure and temperature
#' data.
#' 
#' @details
#' \code{\link{pore.pressure}} and \code{\link{pore.temperature}} both use
#' \code{\link{unavco_dataload}} and \code{\link{unavco_dataload}}  to
#' download, reads-in, and process data from the UNAVCO archive. 
#' 
#' These functions will return all data for the specified year and (Julian) day(s).
#'
#' @param sta character;
#' @param dattype character;
#' @param default.sec.samp numeric;
#' @param localfile character;
#' @param impute logical;
#' @param sta4 character; 4-character station name (e.g. B084).
#' @param year numeric; the year of data
#' @param jday numeric; Julian date (the day of \code{year})
#' @param start. xx;
#' @param end. xx;
#' @param type character; the type of data to generate download data for
#' @param url.check logical; should the urls be tested for existence?
#' @param url.toget character; the URL to download
#' @param temp character; file for temporary writing
#' @param quiet logical; should the functions not be verbose?
#' @param file character; the file to be loaded
#' @param PLUGIN function (not used)
#' @param FUN a function to try, and catch exceptions from
#' @param remove.temp logical; should temporary files be deleted?
#' @param ... additional parameters
#' 
#' @name data-retrieval
#' 
#' @seealso \code{\link{read.https}} and XXX to load data files from
#' the source (github)
#' 
#' \code{\link{consistent}}
#' 
#' \code{\link{unavco-methods}}
#' 
#' @examples
#' \dontrun{
#' # Download data, also makes it consistent
#' xpp <- pore.pressure("B084", 2010, 94)
#' xpt <- pore.temperature("B084", 2010, 94)
#' 
#' # window in on the El Mayor quake:
#' st <- as.POSIXct("2010-04-04", tz='UTC') + 79200
#' redo <- FALSE
#' if (!exists('xppw') | redo) xppw <- pore.pressure("B084", 2010, 94, start.=st)
#' if (!exists('xptw') | redo) xptw <- pore.temperature("B084", 2010, 94, start.=st)
#' 
#' layout(matrix(1:4,ncol=2))
#' plot(xpp, type='l')
#' plot(xpt, type='l')
#' plot(xppw, type='l')
#' plot(xptw, type='l')
#' layout(matrix(1))
#' 
#' # Can also manually subset:
#' unavco_window(as.data.frame(xpp), start. = st, end. = st+10)
#' 
#' }
NULL

#' @rdname data-retrieval
#' @export
pore.pressure <- function(sta4, year, jday, start.=NULL, end.=NULL, ...){
  dat <- unavco_dataload(sta4, year, jday, dattype="pp", ...)
  dat.w <- window(dat, start. = start., end. = end.)
  consistent(dat.w)
}
#' @rdname data-retrieval
#' @export
pore.temperature <- function(sta4, year, jday, start.=NULL, end.=NULL, ...){
  dat <- unavco_dataload(sta4, year, jday, dattype="pt", ...)
  dat.w <- window(dat, start. = start., end. = end.)
  consistent(dat.w)
}

#' @rdname data-retrieval
#' @export
unavco_dataload <- function(sta, year, jday, dattype = c("pp","pt"), 
                            localfile=NULL, quiet=FALSE) UseMethod("unavco_dataload")

#' @rdname data-retrieval
#' @export
unavco_dataload.default <- function(sta, year, jday, 
                                    dattype = c("pp","pt"), 
                                    localfile=NULL, quiet=FALSE){
  #### LOAD TEMPERATURE OR PORE PRESSURE DATA from the UNAVCO REPOSITORY
  dat.t <- match.arg(dattype)
  if (!quiet) pbo_message(sta, year, jday, dat.t)
  # set paths
  pth <- unavco_path(sta, year, jday, type=dat.t, url.check=TRUE)
  # select data
  toDL <- pth$wget
  canDL <- pth$wget.status
  nDL <- length(toDL)
  # download and read
  tempLOC <- if (is.null(localfile)){
    rep(tempfile(), nDL)
  } else {
    lf <- as.character(localfile)
    nlf <- length(lf)
    if (nlf==1 & nDL>1){
      paste(lf,seq_len(nDL),sep=".")
    } else{
      lf
    }
  }
  stopifnot(length(tempLOC) == nDL)
  # Function to assemble one day of data
  DLFUN <- function(dli){
    dl <- toDL[dli]
    tl <- tempLOC[dli]
    cdl <- canDL[dli]
    if (cdl){
      ec <- unavco_downloader(dl, temp=tl, quiet=TRUE)
      if (ec=="FAILED"){
        NULL
      } else {
        unavco_temploader(tl, dat.t)
      }
    } else {
      pbo_message(dl, "unavailable", lead.char="!")
      NULL
    }
  }
  # put together the files
  alldat <- plyr::rbind.fill(lapply(seq_len(nDL), DLFUN))
  if (!is.null(alldat)){
    class(alldat) <- c('unavco',dat.t)
    return(alldat)
  } else {
    if (!quiet) warning("Downloading failed.")
  }
}

#' @details \code{\link{unavco_temploader}} loads and process data downloaded from the UNAVCO archive
#' 
#' @rdname data-retrieval
#' @export
unavco_temploader <- function(file, dattype = c("pp","pt"), ...) UseMethod("unavco_temploader")
#' @rdname data-retrieval
#' @export
unavco_temploader.default <- function(file, dattype = c("pp","pt"), ...){
  #
  dat <- match.arg(dattype)
  dn <- switch(dat, pp="hPa", pt="degC")
  #
  dtmp <- read.table(file, header=FALSE, colClasses=c(rep("character",2),"numeric"), sep=" ")
  data <- base::as.data.frame.POSIXct( unavco_temp_toPOS(paste(dtmp$V1, dtmp$V2)) )
  data$V3 <- dtmp$V3
  #
  names(data) <- c("Dt.", dn)
  #
  return(data)
}

#' @details \code{\link{unavco_downloader}} is the downloader for the UNAVCO data archive
#' \code{\link{download.file}} is used with \code{cacheOK==FALSE}, and \code{mode=="w"}.
#' 
#' @rdname data-retrieval
#' @export
unavco_downloader <- function(url.toget, temp, PLUGIN, quiet=FALSE, remove.temp=FALSE, ...) UseMethod("unavco_downloader")

#' @rdname data-retrieval
#' @export
unavco_downloader.default <- function(url.toget, temp, PLUGIN, quiet=FALSE, remove.temp=FALSE, ...){
  if (missing(temp)) temp <- tempfile()
  if (!quiet) pbo_message(url.toget, "--to--", temp, lead.char=">")
  ec <- dlCatcher(FUN = function() {download.file(url.toget, temp, quiet=quiet, cacheOK=FALSE, mode="w", ...)})
  return(invisible(ec))
}

#' @details \code{\link{dlCatcher}} handles exceptions.
#' 
#' @rdname data-retrieval
#' @export
dlCatcher <- function(FUN){
  ### exception handler
  funcRes <- tryCatch(FUN(), error = function(e) e )
  status <- if (inherits(funcRes, "error")) {
    conditionMessage(funcRes)
    "FAILED"
  } else {
    "OK"
  }
  return(status)
}

#' @details \code{\link{unavco_path}} generates the appropriate path for 
#' UNAVCO data retrieval; it returns the appropriate url to download a .txt.gz data file
#' from the UNAVCO data archive.
#' If \code{url.check==TRUE} the function \code{url.exists} is used
#' to test the status of each link; this can add a substantial amount
#' of processing time for large numbers of urls.
#' 
#' @rdname data-retrieval
#' @export
#' 
#' @examples
#' unavco_path("B084", 2010, 1)
#' unavco_path("B084", 2010, 1:10) # use vectors for multiple urls
#' unavco_path("B084",2010, 1:10, "pt")
#' # Also try:
#' # unavco_path("B084",2010, 1:10, "pt", TRUE)
unavco_path <- function(sta4, year, jday, type=c("pp","pt"), url.check=FALSE){
  #### SETS PATHS TO UNAVCO REPOSITORY DATA
  type <- match.arg(type)
  longtype <- switch(type, pp="PorePressHPa", pt="PoreTempDegC")
  # format inputs
  sta <- toupper(sta4)
  if (nchar(sta) > 4){
    stop("need 4-char stations name")
  }
  yr4 <- sprintf("%04i",as.numeric(year))
  dy3 <- sprintf("%03i",as.numeric(jday))
  yr2 <- substr(yr4, 3, 4)
  # url to data
  repos <- pbo_constants()$unavco
  repos <- repos[[type]]
  # urls
  baserepos <- paste(repos, sta, sep="/")
  daturl <- paste(baserepos, yr4, dy3, sep="/")
  # data files
  fi <- paste0(sta, yr2, dy3, longtype)
  fitxt <- paste(fi, "txt", sep=".")
  fitxtgz <- paste(fitxt, "gz", sep=".")
  # full download links
  todl <- paste(daturl, fitxtgz, sep="/")
  #
  passed <- if (url.check){
    URLTEST <- RCurl::url.exists
    if (!URLTEST(baserepos)){
      # first check base repo, to save time
      pbo_message(baserepos, "non-existent", lead.char="!")
      rep(FALSE, length(todl))
    } else {
      unlist(lapply(X=todl, FUN=URLTEST))
    }
  } else {
    rep(NA, length(todl))
  }
  # return
  toret <- list(daturl=daturl, bot=fi, txt=fitxt, txtgz=fitxtgz, wget=todl, wget.status=passed)
  # pbodl class  [ ]
  # class(toret) <- "pbodl"
  return(toret)
}