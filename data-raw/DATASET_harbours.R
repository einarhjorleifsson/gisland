# Harbours

# https://en.wikipedia.org/wiki/UN/LOCODE
# https://service.unece.org/trade/locode/loc232csv.zip
# Changelog --------------------------------------------------------------------
#  2024-01-22 Moved and ammended stasi/fishydata/scripts/00_auxillary-data.R

library(sf)
library(tidyverse)
library(omar)
con <- connect_mar()

# International codes ----------------------------------------------------------
fil <- "https://service.unece.org/trade/locode/loc232csv.zip"


# Standardized harbours --------------------------------------------------------
# Inputs:
#  stk.trail
#  data-raw/stk_harbours.xlsx
# Output: gpkg/harbours.gpkg
#
# Processing:
#  1. Extract all points from stk were variable io is defined as "I"
#  2. Add standardized harbour-id and name
#     some minor corrections also done
#  3. Create shape
#   3.1. A simple convex hull - because that does not work do:
#   3.3. Calculate the median lon-lat point for each point assigned to harbour
#   3.3. Create convex hull conditional on point within 10km from median
#         Sufficent in this specific case, could probably be less
#         Add a 100 buffer on the convex hull
#   4. Output: data-aux/harbours.gpkg


# Get standardized harbour character acronym
# system("cp -p ~/stasi/fishydata/data-aux/harbours.xlsx inst/extdata/.")
lookup_harbours <-
  readxl::read_excel("inst/extdata/harbours.xlsx") |>
  select(hid_std, harbour, hid_stk = hid) |>
  filter(!is.na(hid_std))

## 1. Extract all points from stk were variable io is defined as "I" ------------
#  In the oracle stk.trail tail table the orginal variable names are:
#    io: "in_out_of_harbor"
#    hid: "harborid"
hb <-
  stk_trail(con) %>%
  filter(!is.na(hid)) %>%
  collect(n = Inf) %>%
  filter(io == "I") %>%
  select(hid, lon, lat) |>
  distinct()
##  2. Add standardized harbour-id and name ------------------------------------
hb <-
  hb |>
  # Thorlaksofn vs Thorshofn
  mutate(hid = case_when(hid == "THH" & lat < 65 ~ "THH",
                         hid == "THH" & lat > 65 ~ "THO",
                         hid == "THO" & lat < 65 ~ "THH",
                         hid == "THO" & lat > 65 ~ "THO",
                         .default = hid)) |>
  inner_join(lookup_harbours |> select(hid, hid_std, harbour),
             multiple = "all") |>
  # Split Borgarfjordur eystri - (only for creating the harbour shapes)
  mutate(hid_std = ifelse(hid_std == "BGJ" & lon < -13.78, "BGJ0", hid_std))

##  3. Create shape -------------------------------------------------------------
hb.pt <-
  hb |>
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)
### 3.1. A simple convex hull ---------------------------------------------------
hb.po <-
  hb.pt |>
  select(hid_std) |>
  group_by(hid_std) |>
  summarise(do_union = FALSE) |>
  st_convex_hull()
# mapview::mapview(hb.po)
# because of things above:
# generate a convex hull around the io == "I" points
#  because there are some cases were the hid is totally wrong, if done directly
#  the polygon will include the wrong coordinats. hence this Kriskuvikurleid

### 3.3. Calculate the median lon-lat point for each point assigned to harbour ---
hb.hid_std.median <-
  hb |>
  group_by(hid_std) |>
  summarise(lon = median(lon),
            lat = median(lat),
            .groups = "drop") |>
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) |>
  st_transform(crs = 3857) |>
  st_buffer(dist = 10000) |>
  st_transform(crs = 4326) |>
  mutate(within10K = TRUE)
# hb.hid_std.median |> mapview::mapview()

### 3.3. Create convex hull conditional on point within 10km from median --------
# Loop through each harbour (hid_std)
HID_STD <- hb.hid_std.median$hid_std |> unique()
res <- list()
for(p in 1:length(HID_STD)) {
  print(p)
  res[[p]] <-
    hb.pt |>
    filter(hid_std %in% HID_STD[p]) |>
    select(-hid_std) |>
    st_join(hb.hid_std.median |>
              filter(hid_std == HID_STD[p])) |>
    filter(within10K) |>
    select(-within10K)
}
hb.pt <-
  res |>
  bind_rows()
hb.po <-
  hb.pt |>
  group_by(hid_std, harbour) |>
  summarise(do_union = FALSE,
            .groups = "drop") |>
  st_convex_hull() |>
  st_transform(crs = 3857) |>
  st_buffer(dist = 100) |>
  st_transform(crs = 4326)
# tmp <- hb.po |> select(hid_std)
# mapview::mapview(tmp)

## 4. Output: harbours/gpkg/harbours.gpkg ---------------------------------------
hb.po |>
  mutate(hid_std = ifelse(hid_std == "BGJ0", "BGJ", hid_std)) |>
  mutate(country = ifelse(str_starts(hid_std, "FO-"), "FO", "IS")) |>
  select(hid_std, harbour, country) |>
  write_sf("/net/hafgola/var/ftp/pub/data/shapes/harbours.gpkg")



# OLDER CODE (not run) ---------------------------------------------------------
# Collection of harbour positions from various sources
# This is the base, but later handedited in qgis:
#  see ~/stasi/gis/harbours
if(FALSE) {
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
    sf::harbours <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/harbours.gpkg")
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
}
