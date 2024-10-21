#' Pretty coordinates
#'
#' Creates pretty coordinates for printing.
#'
#' @export
#'
#' @param x A vector contain data of the format degree-minutes-second (ddmmss)
#' @param suffix A character, e.g. N, E, W, S
pretty_coordinates <- function(x,suffix="") {
  x <- paste0(degrees(stringr::str_sub(x,1,2)),stringr::str_sub(x,3,4),"'",stringr::str_sub(x,5),suffix,sep="")
  return(x)
}

#' degrees
#'
#' Af function that adds the suffix degree to object. Used e.g.
#' in \code{pretty_coordinates}
#'
#' @param x A value or text
degrees <- function(x) paste0(x,"\u00B0")
