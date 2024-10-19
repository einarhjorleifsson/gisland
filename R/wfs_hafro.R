# https://github.com/eblondel/ows4R
# https://inbo.github.io/tutorials/tutorials/spatial_wfs_services
# https://gis.hafogvatn.is/geoserver/hafro/wms

#' Get overview of hafro features
#'
#' @return A tibble
#' @export
#'
gl_h2o_features <- function() {
  # wfs <- paste0("https://gis.hafogvatn.is/geoserver/", what, "/wfs")
  wfs <- "https://gis.hafogvatn.is/geoserver/wfs"
  client <- ows4R::WFSClient$new(wfs,
                                 serviceVersion = "2.0.0")
  client$getFeatureTypes(pretty = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(abstract = client$
                    getCapabilities()$
                    getFeatureTypes()  |>
                    purrr::map_chr(function(x){x$getAbstract()})) |>
    dplyr::mutate(abstract = stringr::str_replace_all(abstract, "\\\r", ""),
                  abstract = stringr::str_replace_all(abstract, "\\\n", " ")) |>
    dplyr::arrange(name)
}



# base
h2o_connection <- function() {
  wfs <- "https://gis.hafogvatn.is/geoserver/wfs"
  url <- httr::parse_url(wfs)
}

#' read object
#'
#' @param typename The typname
#'
#' @return A tibble
#' @export
read_h2o <- function(typename) {
  url <- h2o_connection()
  url$query <- list(service = "wfs",
                    #version = "2.0.0", # optional
                    request = "GetFeature",
                    typename = typename
  )
  url |>
    httr::build_url() |>
    sf::read_sf()
}


#' Get overview of hafro features
#'
#' @return A tibble
#' @export
#'
gl_h2o_features_new <- function() {

  wfs <- "https://hafsja-geoserver.dev.hafogvatn.cloud/geoserver/web/wfs"
  client <- ows4R::WFSClient$new(wfs,
                                 serviceVersion = "2.0.0")
  client$getFeatureTypes(pretty = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(abstract = client$
                    getCapabilities()$
                    getFeatureTypes()  |>
                    purrr::map_chr(function(x){x$getAbstract()})) |>
    dplyr::mutate(abstract = stringr::str_replace_all(abstract, "\\\r", ""),
                  abstract = stringr::str_replace_all(abstract, "\\\n", " ")) |>
    dplyr::arrange(name)
}



# base
h2o_connection <- function() {
  wfs <- "https://gis.hafogvatn.is/geoserver/wfs"
  url <- httr::parse_url(wfs)
}

#' read object
#'
#' @param typename The typname
#'
#' @return A tibble
#' @export
read_h2o <- function(typename) {
  url <- h2o_connection()
  url$query <- list(service = "wfs",
                    #version = "2.0.0", # optional
                    request = "GetFeature",
                    typename = typename
  )
  url |>
    httr::build_url() |>
    sf::read_sf()
}

