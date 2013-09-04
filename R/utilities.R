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
pbo_message <- function(..., lead.char=c("+",">"), n=8){
  msg <- paste(...)
  lead.char <- match.arg(lead.char)
  fullmsg <- paste(paste(rep(lead.char, n), collapse=""), msg, collapse="\n")
  message(fullmsg)
  return(invisible(msg))
}