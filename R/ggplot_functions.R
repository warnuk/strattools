library(ggplot2)


#' Lithologic Stratigraphic Column using ggplot2
#'
#' @param z A vector of depth points or thicknesses, the same length as 'lith'.
#' @param lith A vector of lithology/unit codes, the same length as 'z'.
#' @param widthmap A numeric value between (0,1), 'proportional', 'equal',
#' or a named vector specifying which width between (0,1) to use for each
#' 'lith' ID.
#' @param colormap A named vector of colors (as HEX codes, RGB values, or color
#' strings recognized by R), defining which color to use for each 'lith' ID.
#' @param method 'thicknesses' if 'z' represents thicknesses, 'midpoint' if
#' you wish to interpolate transitions as the midpoint between consecutive
#' different lithologies, or any other string value (in which case, unit
#' thicknesses will be computed and the function will be called again
#' recursively with 'thicknesses' as the 'method').
#' @param start The starting depth to use if plotting with unit thicknesses.
#' @param decreasing A Boolean specifying whether to plot with increasing or
#' decreasing depth.
#' @param ... ...
#' @return NULL
#' @examples
#' plot.new()
#' lith_plot(1:5, c(1, 2, 3, 3, 1), widthmap='proportional',
#'           colormap=c('1'='blue', '2'='red', '3'='yellow'),
#'           method='other')
#' plot.new()
#' lith_plot(c(1, 1, 2, 1), c(1, 2, 3, 1), widthmap='proportional',
#'           colormap=c('1'='blue', '2'='red', '3'='yellow'),
#'           method='thicknesses')
#' @export
gglith_plot <- function(z, lith, widthmap, colormap, method='thicknesses',
                        start=0, decreasing=F, ...) {

  x1 <- c()
  x2 <- c()
  y1 <- c()
  y2 <- c()
  col <- c()

  # Control logic handling for widthmap parameter
  if (all(typeof(widthmap) == 'numeric')) {
    # if widthmap is a numeric value, assign it to all lithologies
    width <- widthmap
    widthmap <- c()
    for (i in unique(lith)) {
      widthmap[paste(i)] <- width
    }
  } else if (all(widthmap == 'proportional')) {
    # if widthmap is "proportional", map between 0 and 1 based on lith
    widthmap <- c()
    lb <- min(lith)-1
    ub <- max(lith)+1
    for (i in unique(lith)) {
      widthmap[paste(i)] <- (i-lb)/(ub-lb)
    }
  } else if (all(widthmap == 'equal')) {
    # if widthmap is "equal", map all liths to 1
    widthmap <- c()
    for (i in unique(lith)) {
      widthmap[paste(i)] <- 1
    }
  } else if (is.vector(widthmap)) {
    # if widthmap is a vector map, make sure it contains all lith values
    if (all(paste(unique(lith)[1:length(unique(lith))]) %in% names(widthmap)) != TRUE) {
      stop("vector widthmap must contain all unique values contained in 'lith'. ")
    }
  } else {
    stop("widthmap must be 'proportional', 'equal', a numeric value between 0 and 1, or a named character vector that maps lithologies to widths between 0 and 1.")
  }

  # check which 'method' is specified
  if (method == 'thicknesses') {
    # if 'z' is unit thicknesses, use start + thickness to build rectangles

    if (decreasing) {
      ylim <- c(start+sum(z), start)
    } else {
      ylim <- c(start, start+sum(z))
    }

    for (i in 1:length(z)) {
      # set the thickness for the unit
      thickness = z[i]
      if (thickness > 0) {
        # set x and y values for rectangle corners
        y1 <- c(y1, start)
        y2 <- c(y2, start + thickness)
        x1 <- c(x1, 0)
        x2 <- c(x2, widthmap[paste(lith[i])])

        # set color based on lithology
        col <- c(col, colormap[paste(lith[i])])
      }

      # set the next starting point
      start <- start + thickness
    }

    d <- data.frame(x1=as.numeric(x1), x2=as.numeric(x2),
                    y1=as.numeric(y1), y2=as.numeric(y2), c=col)
    p <- geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, ...),
                   fill=d$c)

  } else {
    # sort lith and depth vectors using 'decreasing' boolean
    lith <- lith[order(z, decreasing=decreasing)]
    z <- sort(z, decreasing=decreasing)

    if (decreasing) {
      start <- z[length(z)]
    } else {
      start <- z[1]
    }

    # find indices where lith changes between consecutive points
    changes <- append(which(diff(append(lith, 999, 0))!=0), length(z))

    # get depths and liths at changepoints
    z1 <- z[changes]
    lith1 <- lith[changes]

    if (method == 'midpoint') {

      for (i in 1:(length(changes)-1)) {
        col <- c(col, colormap[paste(lith1[i])])
        x1 <- c(x1, 0)
        x2 <- c(x2, widthmap[paste(lith1[i])])
        if (i == 1) {
          y1 <- c(y1, z1[i])
          y2 <- c(y2, mean(c(z1[i], z1[i+1])))
        } else {
          y1 <- c(y1, mean(c(z1[i], z1[i-1])))
          y2 <- c(y2, mean(c(z1[i], z1[i+1])))
        }
      }

      d <- data.frame(x1=as.numeric(x1), x2=as.numeric(x2),
                      y1=as.numeric(y1), y2=as.numeric(y2), c=col)
      p <- geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, ...),
                     fill=d$c)

    } else {
      df <- depths2thicknesses(z, lith)
      p <- gglith_plot(z=df$thickness, lith=df$unit, widthmap=widthmap, colormap=colormap, start=start, method='thicknesses', decreasing=decreasing, ...)
    }
  }
  return(p)
}
