#' Station information from metadata
#' @param name character; the string or \code{\link{regexp}} to use
#' @param meta character; the metadata source
#' @param use.regexp logical; should \code{name} be considered to be a \code{\link{regexp}}?
#' @export
#' @examples
#' station_data()
#' station_data("B084")
#' station_data("bcs", use.regexp=TRUE)
#' station_data(c("B082","B084"), use.regexp=TRUE) # will use only the first
#' station_data("B08[24]", use.regexp=TRUE) # gets what you want
#' #
#' # Get the 16-character codes
#' sta16_from_sta4("B082","B084")
station_data <- function(name=NULL, meta="bsm", use.regexp=FALSE){
  metao <- match.arg(meta)
  meta <- switch(metao, bsm="bsmmeta")
  env <- new.env()
  do.call("data", list(meta, envir=env))
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
      dat <- dat[dat==name, ]      
    }
  }
  rownames(dat) <- NULL
  return(dat)
}
#' @rdname station_data
#' @export
sta16_from_sta4 <- function(sta4=NULL, meta="bsm", use.regexp=FALSE){
  if (!is.null(sta4)){
    stadat <- sapply(sta4, function(x, MET=meta, REG=use.regexp) station_data(name=x, meta=MET, use.regexp=REG)[["sta16"]])
  } else {
    stadat <- station_data(name=NULL, meta=meta, use.regexp=use.regexp)[["sta16"]]
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
#' String to posix for unavco data
#' @export
#' @family utilities time-conversion
unavco_temp_toPOS <- function(tstr, tz="UTC"){
  #2008-12-31 21:00:00.81
  base::as.POSIXct(paste(tstr,tz),format="%Y-%m-%d %H:%M:%S",tz=tz)
}
#' year and day to date
#' @export
#' @family utilities time-conversion
unavco_temp_toDate <- function(year, day, tz="UTC"){
  base::as.Date(paste(year,day),format="%Y %j",tz=tz)
}
#' @export
#' @family utilities time-conversion
unavco_temp_setTZ <- function(df, tz="UTC"){
  #### some functions change the POSIX timezone, this returns it to normal
  df$Dt. <- base::as.POSIXlt(df$Dt., tz=tz)
  return(df)
}