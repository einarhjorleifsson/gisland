#' Binning data
#'
#' @param x A numerical vector
#' @param dx A value, normally less than or equal to 1
#'
#' @return A numerical vector
#' @export
#'
grade <- function(x, dx) {

  if(dx > 1) warning("Not tested for grids larger than one")
  brks <- seq(floor(min(x)), ceiling(max(x)),dx)
  ints <- findInterval(x, brks, all.inside = TRUE)
  x <- (brks[ints] + brks[ints + 1]) / 2
  return(x)
}

