# https://inbo.github.io/tutorials/tutorials/spatial_wfs_services


#' Get LMI strandlínur
#'
#' @return An sf object
#' @export
#'
gl_get_strandlinur <- function() {
  wfs_lmi <- "https://gis.lmi.is/geoserver/wfs"
  url <- httr::parse_url(wfs_lmi)
  url$query <- list(service = "wfs",
                    #version = "2.0.0", # optional
                    request = "GetFeature",
                    typename = "IS_50V:strandlina_flakar",
                    srsName = "EPSG:4326"
  )
  request <- httr::build_url(url)
  sf::read_sf(request) %>%
    sf::st_cast(to = "GEOMETRYCOLLECTION") %>%
    sf::st_collection_extract(type = "POLYGON")
}
