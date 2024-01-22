# Harbours ---------------------------------------------------------------------
# Collection of harbour positions from various sources
# This is the base, but later handedited in qgis:
#  see ~/stasi/gis/harbours
SAVE <- FALSE

library(mapview)
library(sf)
library(tidyverse)
library(omar)
con <- connect_mar()
if(SAVE) {
  read_sf("~/stasi/gis/harbours/harbours_2022-10-30.gpkg") |>
    write_sf("/home/ftp/pub/data/shapes/harbours.gpkg")
}


HB <- gisland::gl_read_is_harbours(trim = FALSE)
mapview(HB, zcol = "hid")
if(SAVE) {
  write_sf(HB, "~/stasi/gis/harbours/harbours.gpkg")
}



# Icelandic harbours -----------------------------------------------------------
hb.mfri <-
  stk_trail(con) %>%
  filter(!is.na(hid)) %>%
  collect(n = Inf) %>%
  # the io == "O" is actually the first point detected once outside the harbour
  filter(io == "I") %>%
  filter(hid != "ISL") %>%
  # why filter this?
  filter(!hid %in% c("REYF")) %>%
  select(hid, lon, lat) |>
  distinct() %>%
  # Merge some harbour ids
  mutate(hid = case_when(hid %in% c("HEL", "KEF", "NJA") ~ "KEF",
                         hid %in% c("BAK", "BAK2") ~ "BAK",
                         hid %in% c("MJO", "MJF") ~ "MJO",
                         hid %in% c("REY", "SNF", "GUF") ~ "REY",
                         hid %in% c("KRS", "AKU") ~ "AKU",
                         TRUE ~ hid)) %>%
  group_by(hid) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 3) %>%
  select(-n) %>%
  mutate(cntr = case_when(lat > 63 ~ "IS",
                          str_sub(hid, 1, 3) == "FO-" ~ "FO",
                          TRUE ~ NA_character_)) %>%
  filter(cntr %in% c("IS", "FO"))
median <-
  hb.mfri %>%
  group_by(hid) %>%
  summarise(lon = median(lon),
            lat = median(lat),
            .groups = "drop")
hb.mfri <-
  hb.mfri %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>%
  select(hid, cntr) %>%
  nest(points = geometry) %>%
  mutate(mpoints = map(points, st_combine),
         geometry = map(mpoints, st_convex_hull)) %>%
  select(hid, cntr, geometry) %>%
  unnest(geometry) %>%
  mutate(harbour = hid) %>%
  left_join(median) %>%
  select(hid, harbour, cntr, lon, lat, geometry) %>%
  mutate(source = "stk")
st_geometry(hb.mfri) <- hb.mfri$geometry
mapview(hb.mfri, zcol = "hid")
if(SAVE) {
  write_sf(hb.mfri, "~/stasi/gis/harbours/tmp.gpkg")
}



hb.mfri <-
  hb.mfri %>%
  st_buffer(500)
if(SAVE) {
  write_sf(hb.mfri, "/net/www/export/home/ftp/pub/data/shapes/harbours.gpkg")
  system("chmod a+rX /net/www/export/home/ftp/pub/data/shapes/harbours.gpkg")
  harbours <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/harbours.gpkg")
}


# VMSTOOLS harbours ------------------------------------------------------------
library(vmstools)
data(harbours)
harbours.vmstools <-
  harbours %>%
  as_tibble() %>%
  mutate(hid = as.character(1:n()),
         harbour = iconv(harbour, from = "latin1", to = "UTF-8")) %>%
  select(hid, harbour, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE) %>%
  # just to create a small polygon of 20 meters in order to have all
  #  harbours as polygons irrespective of source (point, polygon, ...)
  st_buffer(dist = 20) %>%
  mutate(cntr = NA_character_) %>%
  select(hid, harbour, cntr, lon, lat) %>%
  mutate(source = "vmstools")



# Jeppe
tmpfile <- tempfile()
download.file("https://github.com/ices-eg/WKSSFGEO/blob/main/data/harbours.rds",
              destfile = tmpfile)
jp <- readRDS(tmpfile)

#Download the harbours file to you desk and load it to the r environment:
#https://github.com/ices-eg/WKSSFGEO/blob/main/data/harbours.rds
hbs <- readRDS("//aqua-cp-jepol18/C$/Users/jepol/Downloads/harbours.rds")



