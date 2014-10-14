#' Use \code{read.table} with a secure http url (https)
#' @description Uses functions in the \pkg{httr} package to load
#' a file at a secure url (regular urls work too)
#' @details The main function to be used is \code{\link{read.https}}.
#' @name read.https
#' @param URL character; the location of the file
#' @param local.file character; if \code{NULL} a temporary file is used
#' @param delete logical; should \code{local.file} be delete upon exit?
#' @param verbose logical; should messages be shown?
#' @param FUN the function to use for reading \code{URL}; default is \code{\link{read.table}}
#' @param ... additional parameters sent to \code{URL} or \code{\link{content}}
#' @export
#' @author A.J. Barbour
#' @examples
#' \dontrun{
#' x <- "https://raw.githubusercontent.com/abarbour/pborepo/master/data/gen/lsm_coords"
#' get_https(x) # returns raw content
#' read.https(x, header=TRUE) # an actual data.frame instead
#' # (if 'x' is, say, a csv file, change FUN: read.https(x, FUN=read.csv)
#' }
read.https <- function(URL, local.file=NULL, delete=TRUE, verbose=TRUE, FUN=read.table, ...){
  URL <- as.character(URL)
  temp <- is.null(local.file)
  local.file <- if (temp){
    tempfile()
  } else {
    as.character(local.file)
  }
  if (delete) on.exit(unlink(local.file))
  #
  if (verbose){
    message(sprintf("url:\t%s",URL))
    if (!temp) message(sprintf("locally:\t%s",local.file))
  }
  writeBin(object=get_https(URL), con=local.file)
  FUN(local.file, ...)
}
#' @rdname read.https
#' @export
get_https <- function(URL, ...){
  response <- httr::GET(URL)
  httr::stop_for_status(response)
  httr::content(response, type = "raw", ...)
}

#' Retrieve data from the \code{pborepo} online repository (github)
#' @aliases gitdata
#' @param file character; the name of the file to retrieve; if this is \code{NULL} only the
#' results of \code{\link{list_github_files}} are returned
#' @param save.local logical; should the file be saved locally
#' @param saf logical; the value of \code{stringsAsFactors}
#' @param verbose logical;
#' @param ... additional parameters to \code{\link{read.https}}
#' @export
#' 
#' @seealso \code{\link{read.https}}, which is used to retrieve the data
#' @examples
#' \dontrun{
#' #
#' list_github_files() # returns the filenames, with other info
#' #
#' print(gitfiles <- pborepo_gitdata()) # returns only the filenames
#' print(lsm <- pborepo_gitdata("lsm_coords")) # returns a data.frame
#' pborepo_gitdata("bogus_filename") # an error, as expected
#' #
#' # Get them all with plyr/dplyr
#' library(plyr)
#' library(dplyr, warn.conflicts=FALSE)
#' alldata <- llply(gitfiles, function(fi) tbl_df(pborepo_gitdata(fi, verbose=FALSE)), .progress = 'text')
#' names(alldata) <- gitfiles
#' summary(alldata)
#' print(str(alldata, vec.len=2, nchar.max=10))
#' lsm2 <- alldata[['lsm_coords']]
#' all.equal(tbl_df(lsm),lsm2)
#' }
pborepo_gitdata <- function(file=NULL, save.local=FALSE, saf = FALSE, verbose=TRUE, ...){
  df <- list_github_files()
  if (is.null(file)){
    if (verbose) message("No filename specified, so here are the options:")
    return(df$gitfile$file)
  }
  # if a name was specified we continue marching...
  Df <- df[["gitfile"]]
  Df$status <- Df$file %in% file
  Dfsub <- subset(Df, status)
  nr <- nrow(Dfsub)
  if (nr==0){
    stop("No file found. Try 'list_github_files()'")
  } else if (nr>1){
    stop("Multiple files match.")
  }
  fi. <- Dfsub$file
  base. <- df[["gitbase"]]
  url. <- paste(base., fi., sep="/")
  typ. <- Dfsub$type
  hdr. <- Dfsub$hdr
  if (verbose) message(sprintf("%s\ttype '%s'\theader '%s'", fi., typ., hdr.))
  READFUN <- switch(typ., tbl=read.table, csv=read.csv)
  read.https(URL=url., 
             local.file = ifelse(save.local,file,NULL), 
             delete = ifelse(save.local,FALSE,TRUE),
             FUN=READFUN, header=hdr., verbose=verbose, 
             comment.char = "#", 
             stringsAsFactors = saf, ...)
  #class(Dat) <- "pborepo_github"
  #return(Dat)
}
#' @rdname pborepo_gitdata
#' @export
list_github_files <- function(){
  list(gitbase="https://raw.githubusercontent.com/abarbour/pborepo/master/data/gen",
       gitfile=data.frame(
         file=c("bsm_azimuths","bsm_coords","bsm_gaps","bsm_igppGeod","bsm_regions",
                "cgps_coords",
                "lsm_coords",
                "pbo_stations_accel","pbo_stations_bsm","pbo_stations_instrumentcount",
                "pbo_stations_lsm","pbo_stations_pp","pbo_stations_raw","pbo_stations_seis"),
         type=c("tbl","csv","tbl","tbl","csv",
                "csv",
                "tbl",
                "tbl","tbl","tbl",
                "tbl","tbl","tbl","tbl"),
         hdr=c(FALSE,TRUE,FALSE,FALSE,TRUE,
               FALSE,
               TRUE,
               TRUE,TRUE,FALSE,
               TRUE,TRUE,TRUE,TRUE),
         stringsAsFactors = FALSE)
       )
}