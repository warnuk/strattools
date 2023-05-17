#' Meshgrid
#' @description returns 2D grid coordinates from input x and y vectors.
#' @param x Input vector of x-coordinates.
#' @param y Input vector of y-coordinates.
#' @return A list of length 2 with a matrix (X) of x-coordinates and a matrix
#' (Y) of y-coordinates. Each row of X is a copy of input 'x'; each column of Y
#' is a copy of input 'y'.
#' @examples
#' x <- 1:10
#' y <- 1:10
#' meshgrid(y, x)
#' @export
meshgrid <- function(x, y) {
  X <- matrix(rep(x, length(y)), nrow=length(y), byrow=TRUE)
  Y <- matrix(rep(y, length(x)), ncol=length(x), byrow=FALSE)
  return(list(X=X, Y=Y))
}

#' Locate peaks
#' @param y Input vector of data in which to find peaks.
#' @param x (optional) locations of data observations.
#' @param threshold An optional value to be used as a cut-off for peaks.
#' @return A data.frame object with peak locations (x) and peak values (y).
#' @examples
#' x <- seq(0, 2*pi, length=1000)
#' y <- sin(2*x)
#' find_peaks(y, x)
#' @export
find_peaks <- function(y, x=seq(1,length(y)), threshold=NULL) {
  peaks <- x[seq(1, length(x))[which(diff(sign(diff(y)))==-2)]+1]
  peaks_y <- y[!is.na(match(x, peaks))]
  if (!is.null(threshold)) {
    peaks <- peaks[peaks_y >= threshold]
    peaks_y <- peaks_y[peaks_y >= threshold]
  }
  return(data.frame(x=peaks, y=peaks_y))
}

#' Locate troughs
#' @param y Input vector of data in which to find troughs.
#' @param x (optional) locations of data observations.
#' @param threshold An optional value to be used as a cut-off for troughs.
#' @return A data.frame object with trough locations (x) and troughs value (y).
#' @examples
#' x <- seq(0, 2*pi, length=1000)
#' y <- sin(2*x)
#' find_troughs(y, x)
#' @export
find_troughs <- function(y, x=seq(1,length(y)), threshold=NULL) {
  troughs <- x[seq(1, length(x))[which(diff(sign(diff(y)))==2)]+1]
  troughs_y <- y[!is.na(match(x, troughs))]
  if (!is.null(threshold)) {
    troughs <- troughs[troughs_y <= threshold]
    troughs_y <- troughs_y[troughs_y <= threshold]
  }
  return(data.frame(x=troughs, y=troughs_y))
}
