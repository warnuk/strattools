#' Lithologic Stratigraphic Column using base R graphics
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
lith_plot <- function(z, lith, widthmap, colormap, method='thicknesses',
                      start=0, decreasing=F, ...) {

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
    plot.window(xlim=c(0,1), ylim=ylim, ...)
    axis(1, labels=FALSE, tick=FALSE)
    axis(2)
    box()
    for (i in 1:length(z)) {
      # set the thickness for the unit
      thickness = z[i]
      # set x and y values for rectangle corners
      y1 <- start
      y2 <- start + thickness
      x1 <- 0
      x2 <- widthmap[paste(lith[i])]

      # set color based on lithology
      color <- colormap[paste(lith[i])]

      if (thickness > 0) {
        # plot rectangle
        rect(x1, y1, x2, y2, col=color, border=NULL)
      }

      # set the next starting point
      start <- y2
    }
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
      plot.window(xlim=c(0,1), ylim=c(max(z)+1, min(z)-1), ...)
      axis(1, labels=FALSE, tick=FALSE)
      axis(2)
      box()

      for (i in 1:(length(changes)-1)) {
        color <- colormap[paste(lith1[i])]
        x1 <- 0
        x2 <- widthmap[paste(lith1[i])]
        if (i == 1) {
          y1 <- z1[i]
          y2 <- mean(c(z1[i], z1[i+1]))
        } else {
          y1 <- mean(c(z1[i], z1[i-1]))
          y2 <- mean(c(z1[i], z1[i+1]))
        }
        rect(x1, y1, x2, y2, col=color, border=NULL)
      }
    } else {
      df <- depths2thicknesses(z, lith)

      lith_plot(z=df$thickness, lith=df$unit, widthmap=widthmap, colormap=colormap, start=start, method='thicknesses', decreasing=decreasing)

    }
  }
}

#' Ternary Plot
#' @description Plots ternary coordinates on a triangle plot
#' @param a Input vector of a-values in decimal form (a + b + c = 1.0)
#' @param b Input vector of b-values in decimal form (a + b + c = 1.0)
#' @param increment Decimal value specifying increment for minor grid lines.
#' @param new_plot Boolean value specifying whether to create a new plot object
#' or plot on an existing plot object.
#' @param ... Remaining args are passed to points() function for plotting data
#' points.
#' @examples
#' a <- c(0.1, 0.5, 0.2)
#' b <- c(0.6, 0.35, 0.2)
#' ternary_plot(a, b, increment=0.1, col="blue", pch=19)
#' @export
ternary_plot <- function(a, b, c=NULL, increment=NULL, new_plot=T, ...) {

  # height of an equilateral triangle with side-lengths of 1
  h <- tan(deg2rad(60)) * 0.5

  # transform data from ternary space to XY coordinates
  x = (b + 0.5*a)
  y = a * h

  # Cartesian coordinates of vertices of equil. triangle w/ side-lengths of 1
  x0 = c(0, 1, 0.5, 0)
  y0 = c(0, 0, h, 0)

  if (new_plot) {
    plot.new()
    plot.window(xlim=c(-0.1, 1.1), ylim=c(-0.2,1))
  }

  if (!is.null(increment)){
    lb <- increment
    ub <- 1 - increment

    for (i in seq(lb, ub, increment)) {
      x1 <- (c(1-i, 0) + 0.5*rep(i, 2))
      y1 <- rep(i, 2) * h
      x2 <- (rep(i, 2) + 0.5*c(1-i, 0))
      y2 <- c(1-i, 0) * h
      x3 <- (c(0, 1-i) + 0.5*c(1-i, 0))
      y3 <- c(1-i, 0) * h

      lines(x1, y1, col="gray", lwd=0.5)
      lines(x2, y2, col="gray", lwd=0.5)
      lines(x3, y3, col="gray", lwd=0.5)
    }
  }

  lines(x0, y0, col="black", lwd="2")
  points(x, y, ...)
}
