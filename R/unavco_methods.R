#' Methods for the 'unavco' class
#' @name unavco-methods
#' @seealso \code{\link{consistent}} and \code{\link{unavco_dataload}}
NULL

#' @rdname unavco-methods
#' @export
as.zoo.unavco <- function(x, frequency = NULL, ...){
  udf <- as.data.frame(x, ...)
  xd <- data.frame(udf[, 2])
  names(xd) <- names(x)[2]
  xord <- udf[, 1]
  zoo(xd, order.by = xord, frequency = frequency)
}
#' @rdname unavco-methods
#' @export
zoo.unavco <- as.zoo.unavco
#' @rdname unavco-methods
#' @export
as.data.frame.unavco <- function(x, ...){
  #> class(x[[2]])
  #[1] "numeric" <-- the data
  #> class(x[[1]])
  #[1] "POSIXct" "POSIXt"  <- timestamps
  df. <- data.frame(x[[1]], x[[2]], stringsAsFactors=FALSE)
  names(df.) <- names(x)[1:2]
  df.
}
#' @rdname unavco-methods
#' @export
data.frame.unavco <- as.data.frame.unavco

#' @rdname unavco-methods
#' @export
window.unavco <- function(x, ...){
  window(as.zoo(x), ...)
}


#' Make the downloaded data consistent in time
#' @export
#' @param dat downloaded data to fortify
#' @param ... additional parameters
#' @seealso \code{\link{unavco_dataload}} and \code{\link{unavco-methods}}
#' @examples
#' consistent(1:10)  # does nothing but return
#' 
#' # ensures all possible data points are represented
#' library(zoo)
#' xc <- consistent(zoo(1:9, c(1,3:10))) # note the NA at index 2
#' na.approx(xc) # interpolate, for example
consistent <- function(dat, ...) UseMethod("consistent")

#' @rdname consistent
#' @export
consistent.default <- function(dat, ...){
  dat
}

#' @rdname consistent
#' @export
consistent.unavco <- function(dat, ...){
  dat.z <- as.zoo(dat)
  dat.cz <- consistent(dat.z)
  times <- as.POSIXct(format(time(dat.cz)), tz = 'UTC')
  dat.c <- data.frame(times, dat.cz[,1])
  rownames(dat.c) <- NULL
  names(dat.c) <- names(dat)
  dat.c
}

#' @rdname consistent
#' @export
consistent.zoo <- function(dat, verbose=TRUE, ...){
  #
  # Make sure there is an observation at
  # every point from the beginning of time
  # to the end of time, for the sampling rate
  #
  stopifnot(is.zoo(dat))
  nms <- names(dat)
  frq <- frequency(dat)
  st <- start(dat)
  en <- end(dat)
  # reformat if posix -- hopefully this catches all
  if (all(!is.numeric(st), !is.integer(st))){
    st <- as.POSIXct(format(st), tz = 'UTC')
  }
  if (all(!is.numeric(en), !is.integer(en))){
    en <- as.POSIXct(format(en), tz = 'UTC')
  }
  time.seq <- seq.int(from=st, to=en, by=frq)
  #
  nnew <- length(time.seq)
  new.dat <- zoo(rep.int(NA, nnew), order.by=time.seq)
  dat.m <- merge(dat, new.dat)
  #
  dat.c <- zoo(data.frame(dat.m[,1]), order.by=time.seq)
  names(dat.c) <- nms
  dat.c
}
