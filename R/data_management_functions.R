#' Convert depths to unit thicknesses
#' @param depths A vector of depth points, the same length as 'values'.
#' @param values A vector of lithology/unit values, the same length as 'depths'.
#' @param decreasing A boolean dictating whether to sort depths increasing or decreasing
#' @return A data.frame object with thicknesses in the first column, lithology/unit values in the second column.
#' @examples
#' depths2thicknesses(c(1, 2, 3, 4, 5), c(1, 1, 2, 2, 1))
#' depths2thicknesses(c(1010, 1020, 1030, 1040, 1050), c(1, 1, 2, 2, 1))
#' @export
depths2thicknesses <- function(depths, values, decreasing=F) {
  # sort by depths (increasing unless decreasing==T)
  values <- values[order(depths, decreasing=decreasing)]
  depths <- sort(depths, decreasing=decreasing)

  # find indices where value changes between consecutive points
  changes <- append(which(diff(append(values, 999, 0))!=0), length(depths))

  # get depths and liths at changepoints
  dz <- depths[changes]
  dx <- values[changes]

  thicknesses = abs(diff(dz))
  units = dx[1:(length(dx)-1)]

  return(data.frame(thickness=thicknesses, unit=units))
}

#' Convert unit thicknesses to depths
#' @param thicknesses A vector of unit thicknesses, the same length as 'values'.
#' @param values A vector of lithology/unit values, the same length as 'thicknesses'.
#' @param start A starting depth to which thicknesses will be added to produce depths
#' @return A data.frame object with depths in the first column, lithology/unit values in the second column.
#' @examples
#' thicknesses2depths(c(2, 2, 5, 1), c(1, 2, 3, 1), 0)
#' thicknesses2depths(c(10, 5, 2, 3), c(3, 1, 2, 3), 150)
#' @export
thicknesses2depths <- function(thicknesses, values, start=0) {
  depths <- append(cumsum(thicknesses)+start, start, 0)
  values <- append(values, values[length(values)])
  return(data.frame(depths=depths, values=values))
}

#' deg2rad
#' @description Convert angular measurements from degrees to radians
#' @param degrees A numeric measurement in degrees
#' @return A numeric measurement in radians
#' @examples
#' deg2rad(30)
#' deg2rad(180)
#' deg2rad(360)
#' deg2rad(720)
#' @export
deg2rad <- function(degrees) {
  radians <- degrees * pi / 180
  return(radians)
}

#' rad2deg
#' @description Convert angular measurements from radians to degrees
#' @param radians A numeric measurement in radians
#' @return A numeric measurement in degrees
#' @examples
#' deg2rad(pi/2)
#' deg2rad(pi)
#' deg2rad(2*pi)
#' deg2rad(6*pi)
#' @export
rad2deg <- function(radians) {
  degrees <- radians * 180 / pi
  return(degrees)
}
