# ---------------------------------------------------------
# Stuff stolen/adapted/inspired from the geo-package
#  Functions are amended to be more in flow with the
#  dplyr syntax

#' @title Convert latitude and longitude
#'
#' @description Convert between different representations of latitude and
#' longitude, namely degrees-minutes-decimal minutes (DDMMmm) and decimal
#' degrees (DD.dd).
#'
#' Adapted from geo::geoconvert to be dplyr pipe friendly
#'
#' @export
#'
#' @author Hoskuldur Bjornsson <hoski@@hafro.is>
#'
#' @param x A vector of the form DDMMmm or DD.dd
#' @param inverse Which conversion should be undertaken, default from
#' degrees-minutes-decimal minutes (DDMMmm) to decimal degrees (DD.dd)
#' @param na A boolean, if FALSE (default) minutes larger than 60 are
#' rounded up, if TRUE returns NA's. Only appicable when inverse TRUE.
#' @examples
#' geo_convert(c(-124598, 032269))
#' geo_convert(c(-124598, 037569), na = TRUE)
#' geo_convert(c(-12.766333, 3.378167), inverse = TRUE)
geo_convert <- function (x, inverse = FALSE, na = FALSE)
{
  if(!inverse) {
    i <- sign(x)
    x <- abs(x)

    min <- (x/100) - trunc(x/10000) * 100
    if(na) min <- ifelse(min > 60, NA, min)

    return((i * (x + (200/3) * min))/10000)
  } else {
    i <- sign(x)
    x <- abs(x)
    p1 <- floor(x)
    p2 <- floor((x - p1) * 60)
    p3 <- round((x - p1 - p2/60) * 100 * 60)
    return(i * (p1 * 10000 + p2 * 100 + p3))
  }
}

#' Classifies if points are inside a region
#'
#' @param x A vector of longitudes.
#' @param y A vector of latitudes.
#' @param reg A data.frame contain column names lon, lat and Region.
#'
#' @return A vector of TRUE and FALSE
#'
geo_inside <- function(x, y, reg) {

  x.reg <- reg$lon
  y.reg <- reg$lat

  border <- adapt(y.reg, x.reg, projection = "none")
  tmpinside <- rep(0, length(border$lxv))
  inside <- rep(0, length(x))
  inside <- .C("geomarghc", PACKAGE = "geo", as.double(x),
               as.double(y), as.integer(length(y)), as.double(border$x),
               as.double(border$y), as.integer(border$lxv), as.integer(length(border$lxv)),
               as.integer(inside), as.integer(tmpinside))

  inside <- as.logical(inside[[8]])

  return(inside)
}

#' Allocate region name to points
#'
#' @param x A vector of longitudes.
#' @param y A vector of latitudes.
#' @param reg A data.frame contain column names lon, lat and Region.
#' @param region.name The column name in object reg that contains
#' the value/text to be returned.
#'
#' @return A vector of region names
#'
geo_region <- function(x, y, reg, region.name = "Region") {
  reg.name <- unique(reg[,region.name])
  ret <- rep(NA, length(x)) # stuff to return
  x <- ifelse(is.na(x), -999, x) # dummy
  y <- ifelse(is.na(y), -999, y) # dummy
  for(i in 1:length(reg.name)) {
    inside <- geo_inside(x, y, reg[reg[,region.name] %in% reg.name[i],])
    if(any(inside)) ret[inside] <- reg.name[i]
  }
  return(ret)
}

#' Geographic distance computation
#'
#' dplyr-ized version of geo::arcdist
#'
#' @param x1 A vector of longitudes.
#' @param y1 A vector of latitudes.
#' @param x2 A vector of longitudes.
#' @param y2 A vector of latitudes.
#' @param unit A atomic character vector, if "nmi" (default) returns
#' values in nautical miles, any other value returns kilometers
#'
#' @return A numerical vector
#' @export
#'
geo_distance <- function(x1, y1, x2, y2, unit = "nmi") {

  if (unit == "nmi") {
    miles <- 1.852
  } else {
    miles <- 1
  }

  rad <- 6367
  mult1 <- (rad/miles)
  mult2 <- pi/180

  ret <- mult1 * acos(sin(mult2 * y1) * sin(mult2 * y2) +
                        cos(mult2 * y1) * cos(mult2 * y2) * cos(mult2 * x1 - mult2 * x2))

  return(ret)

}

#' Title
#'
#' @param reg.lat XXX
#' @param reg.lon XXX
#' @param projection XXX

adapt <- function (reg.lat, reg.lon, projection = "Mercator") {
  ind <- c(1:length(reg.lat))
  nholes <- length(reg.lat[is.na(reg.lat)])
  lxv <- c(0:(nholes + 1))
  if (nholes != 0) {
    ind1 <- ind[is.na(reg.lat)]
    ind2 <- c(ind1 - 1, ind1, length(reg.lat))
    lon <- reg.lon[-ind2]
    lat <- reg.lat[-ind2]
    for (i in 2:(nholes + 1)) {
      lxv[i] <- ind1[i - 1] - 2 * (i - 1)
    }
  }
  else {
    ind <- (1:length(reg.lon) - 1)
    lon <- reg.lon[ind]
    lat <- reg.lat[ind]
  }
  lxv[nholes + 2] <- length(lon)
  if (projection == "none")
    return(list(x = lon, y = lat, lxv = lxv))
  else return(list(lat = lat, lon = lon, lxv = lxv))
}
