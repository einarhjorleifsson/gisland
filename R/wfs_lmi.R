# https://github.com/eblondel/ows4R
# https://inbo.github.io/tutorials/tutorials/spatial_wfs_services

#' Get overview of LMI features
#'
#' @return A tibble
#' @export
#'
gl_lmi_features <- function() {
  wfs_lmi <- "https://gis.lmi.is/geoserver/wfs"
  lmi_client <- ows4R::WFSClient$new(wfs_lmi,
                                     serviceVersion = "2.0.0")
  lmi_client$getFeatureTypes(pretty = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(abstract = lmi_client$
                    getCapabilities()$
                    getFeatureTypes() %>%
                    #map(function(x){x$getAbstract()})
                    purrr::map_chr(function(x){x$getAbstract()})) %>%
    dplyr::mutate(abstract = stringr::str_replace_all(abstract, "\\\r", ""),
                  abstract = stringr::str_replace_all(abstract, "\\\n", " ")) %>%
    dplyr::arrange(name)
}


#' @export
read_lmi_features <- defunct("read_lmi_features changed name to gl_lmi_features")

# base
lmi_connection <- function() {
  wfs_lmi <- "https://gis.lmi.is/geoserver/wfs"
  url <- httr::parse_url(wfs_lmi)
}

#' read object
#'
#' @param typename The typname
#'
#' @return A tibble
#' @export
read_lmi <- function(typename) {
  url <- lmi_connection()
  url$query <- list(service = "wfs",
                    #version = "2.0.0", # optional
                    request = "GetFeature",
                    typename = typename,
                    srsName = "EPSG:4326"
  )
  url %>%
    httr::build_url() %>%
    sf::read_sf()
}

#' Get strandlínur
#'
#' @param fix A boolean (default TRUE) to "fix" the shapefile
#'
#' @return An sf object
#' @export
#'
read_strandlinur <- function(fix = TRUE) {
  d <-
    "IS_50V:strandlina_flakar" %>%
    read_lmi() %>%
    sf::st_cast(to = "GEOMETRYCOLLECTION") %>%
    dplyr::filter(!sf::st_is_empty(.)) %>%
    sf::st_collection_extract(type = "POLYGON")
  d %>%
    sf::st_make_valid() %>%
    dplyr::mutate(geom = lwgeom::lwgeom_make_valid(geom))
}

#' Get grunnpunktar
#'
#' @return An sf object
#' @export
#'
read_grunnpunktar <- function() {
  "LHG:grunnlina_punktar" %>%
    read_lmi()
}

#' Get grunnpunktar
#'
#' @return An sf object
#' @export
#'
read_grunnlinur <- function() {
  "LHG:grunnlina" %>%
    read_lmi() %>%
    sf::st_cast(to = "MULTILINESTRING")
}

#' Get 12 miles
#'
#' @return An sf object
#' @export
#'
read_12miles <- function() {
  "LHG:landhelgi_12_milur_lina" %>%
    read_lmi() %>%
    sf::st_cast(to = "MULTILINESTRING")
}

#' Get EEZ
#'
#' @return An sf object
#' @export
#'
read_eez <- function() {
  "LHG:efnahagslogsaga_lina" %>%
    read_lmi() %>%
    sf::st_cast(to = "MULTILINESTRING")
}

#' Get depth contours
#'
#' @return An sf object
#' @export
#'
read_depth <- function() {
  "LHG:dypislinur" %>%
    read_lmi() %>%
    sf::st_cast(to = "MULTILINESTRING")
}

