# https://github.com/eblondel/ows4R
# https://inbo.github.io/tutorials/tutorials/spatial_wfs_services

#' Get overview of LMI features
#'
#' @return A tibble
#' @export
#'
lmi_features <- function() {
  wfs_lmi <- "https://gis.lmi.is/geoserver/wfs"
  lmi_client <- ows4R::WFSClient$new(wfs_lmi,
                                     serviceVersion = "2.0.0")
  lmi_client$getFeatureTypes(pretty = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(abstract = lmi_client$
                    getCapabilities()$
                    getFeatureTypes() |>
                    #map(function(x){x$getAbstract()})
                    purrr::map_chr(function(x){x$getAbstract()})) |>
    dplyr::mutate(abstract = stringr::str_replace_all(abstract, "\\\r", ""),
                  abstract = stringr::str_replace_all(abstract, "\\\n", " ")) |>
    dplyr::arrange(name)
}



# base
con_lmi <- function() {
  wfs_lmi <- "https://gis.lmi.is/geoserver/wfs"
  url <- httr::parse_url(wfs_lmi)
}

#' read object
#'
#' @param typename The typname
#'
#' @return A tibble
read_lmi0 <- function(typename) {
  url <- con_lmi()
  url$query <- list(service = "wfs",
                    #version = "2.0.0", # optional
                    request = "GetFeature",
                    typename = typename,
                    srsName = "EPSG:4326"
  )
  url |>
    httr::build_url() |>
    sf::read_sf()
}

# Landmaelingar ----------------------------------------------------------------

#' Get strandlínur
#'
#' @param make_valid A boolean (default TRUE) to "fix" the shapefile
#'
#' @return An sf object
#' @export
#'
gl_lmi_strandlina <- function(make_valid = TRUE) {
  "IS_50V:strandlina_flakar" |>
    read_lmi0() |>
    sf::st_cast("GEOMETRYCOLLECTION") |>
    dplyr::filter(!sf::st_is_empty(geom)) |>
    sf::st_collection_extract(type = "POLYGON") ->
    d
  if(make_valid) {
    d |>
      sf::st_make_valid() |>
      dplyr::mutate(geom = lwgeom::lwgeom_make_valid(geom)) ->
      d
  }
  return(d)
}


# Safer way than read_shorline, at least wrt valid geometries
gl_lmi_shoreline <- function(mainland = TRUE) {
  s <-
    "IS_50V:strandlina_flakar" |>
    gisland::read_lmi()
  tmp <- tempdir()
  sf::write_sf(s, paste0(tmp, "/in.gpkg"))
  cmd <-
    paste0("ogr2ogr ",
           tmp,
           "/out.gpkg ",
           tmp,
           "/in.gpkg",
           " -explodecollections -nlt CONVERT_TO_LINEAR")
  system(cmd)
  s <-
    sf::read_sf(paste0(tmp, "/out.gpkg")) |>
    dplyr::filter(!sf::st_is_empty(geom)) |>
    #sf::st_collection_extract(type = "POLYGON")
    sf::st_make_valid() |>
    dplyr::mutate(geom = lwgeom::lwgeom_make_valid(geom)) |>
    dplyr::mutate(area = sf::st_area(geom),
                  on_land = TRUE) |>
    dplyr::select(area, on_land)
  if(mainland) {
    s <-
      s |>
      dplyr::filter(area == max(area)) |>
      dplyr::select(on_land)
  }
  return(s)
}

# LHG --------------------------------------------------------------------------

#' Get adlaeg belti
#'
#' @return An sf object
#' @export
#'
gl_lhg_adlaegbelti <- function() {
  "LHG:adlaekt_belti_24_milur_lina" |>
    read_lmi0() |>
    sf::st_cast(to = "MULTILINESTRING") |>
    sf::st_cast(to = "LINESTRING", warn = FALSE)
}

#' Get depth contours
#'
#' @return An sf object
#' @export
#'
gl_lhg_dypislinur <- function() {
  "LHG:dypislinur" |>
    read_lmi0() |>
    sf::st_cast(to = "MULTILINESTRING") |>
    sf::st_cast(to = "LINESTRING", warn = FALSE)
}

#' Get EEZ
#'
#' @return An sf object
#' @export
#'
gl_lhg_eez <- function() {
  "LHG:efnahagslogsaga_lina" |>
    read_lmi0() |>
    sf::st_cast(to = "MULTILINESTRING") |>
    sf::st_cast(to = "LINESTRING", warn = FALSE)
}

#' Get grunnlinur
#'
#' @return An sf object
#' @export
#'
gl_lhg_grunnlina <- function() {
  "LHG:grunnlina" |>
    read_lmi0() |>
    sf::st_cast(to = "MULTILINESTRING") |>
    sf::st_cast(to = "LINESTRING", warn = FALSE)
}

#' Get grunnpunktar
#'
#' @return An sf object
#' @export
#'
gl_lhg_grunnpunktar <- function() {
  "LHG:grunnlina_punktar" |>
    read_lmi0()
}


#' Get 12 miles
#'
#' @return An sf object
#' @export
#'
gl_lhg_landhelgi <- function() {
  "LHG:landhelgi_12_milur_lina" |>
    read_lmi0() |>
    sf::st_cast(to = "MULTICURVE") |>
    sf::st_cast(to = "MULTILINESTRING") |>
    sf::st_cast(to = "LINESTRING", warn = FALSE)
}

# Fiskistofa -------------------------------------------------------------------

#' Fiskistofa - get reglugerðir
#'
#' @return An sf object
#' @export
gl_fs_adrar_reglugerdir <- function() {
  "fiskistofa:adrar_reglugerdir" |>
    read_lmi0()
}

#' Fiskistofa - get efnistökuleyfi
#'
#' @return An sf object
#' @export
gl_fs_efnistokuleyfi <- function() {
  "fiskistofa:efnistokuleyfi" |>
    read_lmi0()
}
#' Fiskistofa - get framkvæmdarleyfi
#'
#' @return An sf object
#' @export
gl_fs_framkvaemdaleyfi_igildi <- function() {
  "fiskistofa:framkvaemdaleyfi_igildi" |>
    read_lmi0()
}

#' Fiskistofa - get framkvæmdarleyfi
#'
#' @return An sf object
#' @export
gl_fs_framkvaemdaleyfi_ogild <- function() {
  "fiskistofa:framkvaemdaleyfi_ogild" |>
    read_lmi0()
}

#' Fiskistofa - get dragnótarsvæði
#'
#' @return An sf object
#' @export
gl_fs_virk_dragnotaveidisvaedi <- function() {
  "fiskistofa:virk_dragnotaveidisvaedi" |>
    read_lmi0()
}

#' Fiskistofa - get grásleppulokanir
#'
#' @return An sf object
#' @export
gl_fs_virk_grasleppulokanir <- function() {
  "fiskistofa:virk_grasleppulokanir" |>
    read_lmi0()
}

gl_fs_virk_hrygningarsvaedi <- function() {
  "fiskistofa:virk_hrygningarsvaedi" |>
    read_lmi0()
}

#' Fiskistofa - get humarveiðisvæði
#'
#' @return An sf object
#' @export
gl_fs_virk_humarveidisvaedi  <- function() {
  "fiskistofa:virk_humarveidisvaedi" |>
    read_lmi0()
}

#' Fiskistofa - get reglugerði
#'
#' @return An sf object
#' @export
gl_fs_virkar_reglugerdir <- function() {
  "fiskistofa:virkar_reglugerdir" |>
    read_lmi0()
}

#' Fiskistofa - get skyndilokanir
#'
#' @return An sf object
#' @export
gl_fs_virkar_skyndilokanir <- function() {
  "fiskistofa:virkar_skyndilokanir" |>
    read_lmi0()
}

# Orkustofnun ------------------------------------------------------------------

#' Orkustofnun: leyfi
#'
#' @return An sf object
#' @export
gl_os_leyfi <- function() {
  d <-
    "orkustofnun:gisleyfiview_ls" |>
    read_lmi0() |>
    sf::st_cast("GEOMETRYCOLLECTION") |>
    sf::st_cast("POLYGON")
}
