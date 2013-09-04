#/Users/abarbour/kook.processing/R/dev/timetasks/merge/funcs.R

#' Retrieve and load data from the UNAVCO archive
#' 
#' @details
#' \code{\link{unavco_path}} is used, \code{url.check=TRUE} enabled, 
#' to generate the file path(s)
#'
#' \code{\link{unavco_downloader}} is used
#' to download the file(s)
#'  
#' @param sta character;
#' @param year numeric;
#' @param day numeric;
#' @param dattype character;
#' @param default.sec.samp numeric;
#' @param localfile character;
#' @param quiet logical;
#' @param impute logical;
#' 
#' @family data-retrieval
#' @export
unavco_dataload <- function(sta, year, day, dattype = c("pp","pt"), 
                            default.sec.samp=1, localfile=NULL, quiet=FALSE,
                            impute=TRUE) UseMethod("unavco_dataload")
#' @rdname unavco_dataload
#' @method unavco_dataload default
#' @S3method unavco_dataload default
unavco_dataload.default <- function(sta, year, day, 
                                    dattype = c("pp","pt"), 
                                    default.sec.samp=1, localfile=NULL, quiet=FALSE,
                                    impute=TRUE){
  #### LOAD TEMPERATURE OR PORE PRESSURE DATA from the UNAVCO REPOSITORY
  dat <- match.arg(dattype)
  #if (!quiet) pbo_message(sta, year, day, dat)
  # set paths
  pth <- unavco_path(sta, year, day, type=dat, url.check=TRUE)
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

#' Load and process data downloaded from the UNAVCO archive
#' @param file character; the file to be loaded
#' @param dattype character;
#' @param ... additional parameters
#' 
#' @family data-retrieval
#' @export
unavco_temploader <- function(file, dattype = c("pp","pt"), ...) UseMethod("unavco_temploader")
#' @rdname unavco_temploader
#' @method unavco_temploader default
#' @S3method unavco_temploader default
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

#' Downloader for the UNAVCO data archive
#' @details
#' \code{\link{download.file}} is used with
#' \code{cacheOK==FALSE}, and \code{mode=="w"}.
#' 
#' Exception handling is performed with \code{\link{dlCatcher}}.
#' 
#' @param url.toget character; the URL to download
#' @param temp character; file for temporary writing
#' @param quiet logical; should the downloader be verbose?
#' @param ... additional parameters to \code{\link{download.file}}
#'
#' @export
#' @family data-retrieval
unavco_downloader <- function(url.toget, temp, PLUGIN, quiet=FALSE, remove.temp=FALSE, ...) UseMethod("unavco_downloader")
#' @rdname unavco_downloader
#' @method unavco_downloader default
#' @S3method unavco_downloader default
unavco_downloader.default <- function(url.toget, temp, PLUGIN, quiet=FALSE, remove.temp=FALSE, ...){
  if (missing(temp)) temp <- tempfile()
  if (!quiet) pbo_message(url.toget, "--to--", temp, lead.char=">")
  ec <- dlCatcher(FUN = function() {download.file(url.toget, temp, quiet=quiet, cacheOK=FALSE, mode="w", ...)})
  return(invisible(ec))
}

#' @rdname unavco_downloader
#' @param FUN A function to try
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

#' Generate the appropriate path for UNAVCO data retrieval
#' @description
#' Returns the appropriate url to download a .txt.gz data file
#' from the UNAVCO data archive.
#' 
#' @details
#' If \code{url.check==TRUE} the function \code{url.exists} is used
#' to test the status of each link; this can add a substantial amount
#' of processing time for large numbers of urls.
#' 
#' @param sta4 character; 4-character station name (e.g. B084).
#' @param year numeric; year of data
#' @param jday numeric; Julian date (day of year)
#' @param type character; the type of data to generate download data for
#' @param url.check logical; should the urls be tested for existence?
#' 
#' @family data-retrieval
#' @export
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
  repos <- pbo_constants(FALSE)$unavco
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