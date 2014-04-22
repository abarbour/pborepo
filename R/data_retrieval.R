#/Users/abarbour/kook.processing/R/dev/timetasks/merge/funcs.R

#' Retrieve and load data from the UNAVCO archive
#'  
#' @param sta character;
#' @param dattype character;
#' @param default.sec.samp numeric;
#' @param localfile character;
#' @param impute logical;
#' @param sta4 character; 4-character station name (e.g. B084).
#' @param year numeric; the year of data
#' @param jday numeric; Julian date (the day of \code{year})
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
NULL

#' @rdname data-retrieval
#' @export
unavco_dataload <- function(sta, year, jday, 
                            dattype = c("pp","pt"), 
                            default.sec.samp=1, localfile=NULL, quiet=FALSE,
                            impute=TRUE) UseMethod("unavco_dataload")

#' @rdname data-retrieval
#' @export
unavco_dataload.default <- function(sta, year, jday, 
                                    dattype = c("pp","pt"), 
                                    default.sec.samp=1, localfile=NULL, quiet=FALSE,
                                    impute=TRUE){
  #### LOAD TEMPERATURE OR PORE PRESSURE DATA from the UNAVCO REPOSITORY
  dat <- match.arg(dattype)
  #if (!quiet) pbo_message(sta, year, day, dat)
  # set paths
  pth <- unavco_path(sta, year, jday, type=dat, url.check=TRUE)
  # select data
  toDL <- pth$wget
  canDL <- pth$wget.status
  nDL <- length(toDL)
  # download and read
  if (is.null(localfile)){
    tempLOC <- rep(tempfile(), nDL)
  } else {
    tempLOC <- as.character(localfile)
  }
  stopifnot(length(tempLOC) == nDL)
  for (dli in seq_len(nDL)){
    dl <- toDL[dli]
    tl <- tempLOC[dli]
    cdl <- canDL[dli]
    if (cdl){
      unavco_downloader(dl, temp=tl, quiet=quiet)
      dat <- unavco_temploader(tl, dat)
      #dat <- loadUnav(tmp, typ=typ)
    } else {
      pbo_message(dl, "unavailable", lead.char="!")
      dat <- NULL
    }
    if (impute){
      if (!is.null(dat)){
        #dat <- impute(dat)
      } else {
        #generate [ ]
        #dat <- genPP(year, day, sec.samp=default.sec.samp)
      }
    }
  }
  return(dat)
}

#' @details \code{\link{unavco_temploader}} loads and process data downloaded from the UNAVCO archive
#' 
#' @rdname data-retrieval
#' @export
unavco_temploader <- function(file, dattype = c("pp","pt"), ...) UseMethod("unavco_temploader")
#' @rdname data-retrieval
#' @export
unavco_temploader.default <- function(file, dattype = c("pp","pt"), ...){
  dat <- match.arg(dattype)
  #
  dn <- switch(dat, pp="hPa", pt="degC")
  #
  dtmp <- read.table(file, header=FALSE, colClasses=c(rep("character",2),"numeric"), sep=" ")
  data <- base::as.data.frame.POSIXct( unavco_temp_toPOS(paste(dtmp$V1, dtmp$V2)) )
  data$V3 <- dtmp$V3
  #
  names(data) <- c("Dt.",dn)
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
  if (inherits(funcRes, "error")) {
    conditionMessage(funcRes)
    status <- "FAILED"
  } else {
    status <- "OK"
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
  # was .unavPath
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
  #
  baserepos <- paste(repos, sta, sep="/")
  daturl <- paste(baserepos, yr4, dy3, sep="/")
  # data files
  fi <- paste0(sta, yr2, dy3, longtype)
  fitxt <- paste(fi, "txt", sep=".")
  fitxtgz <- paste(fitxt, "gz", sep=".")
  # full download links
  todl <- paste(daturl, fitxtgz, sep="/")
  #
  if (url.check){
    URLTEST <- RCurl::url.exists
    if (!URLTEST(baserepos)){
      # first check base repo, to save time
      pbo_message(baserepos, "non-existent", lead.char="!")
      passed <- rep(FALSE, length(todl))
    } else {
      passed <- unlist(lapply(X=todl, FUN=URLTEST))
    }
  } else {
    passed <- rep(NA, length(todl))
  }
  # return
  toret <- list(daturl=daturl, bot=fi, txt=fitxt, txtgz=fitxtgz, wget=todl, wget.status=passed)
  # pbodl class  [ ]
  # class(toret) <- "pbodl"
  return(toret)
}