# ------------------------------------------------------------------------------
# EEZ - no shoreline
# 2022-04-21: Zip file from Halli via email, source not specified
library(sf)
library(tidyverse)
tmpdir <- tempdir()
unzip("data-raw/Map_EEZ_and_high_sea.zip", exdir = tmpdir)
unzip(paste0(tmpdir, "/EEZ_land_union_v3_202003.zip"), exdir = tmpdir)
files <- fs::dir_ls(paste0(tmpdir, "/EEZ_land_union_v3_202003"))
eez <- read_sf(files[5])
if(FALSE) {
  write_sf(eez, "/net/hafgola.hafro.is/var/ftp/pub/data/shapes/eez_no-shoreline.gpkg")
  system("chmod a+rx /net/hafgola.hafro.is/var/ftp/pub/data/shapes/eez_no-shoreline.gpkg")
}

# ------------------------------------------------------------------------------
# FAO - orginal shape, taken from fao-site
fao <- read_sf("~/prj2/misc/inst/gpkg/FAO_AREAS_CWP_NOCOASTLINE.gpkg")
if(FALSE) {
  write_sf(fao, "/net/hafgola.hafro.is/var/ftp/pub/data/shapes/FAO_AREAS_CWP_NOCOASTLINE.gpkg")
  system("chmod a+rx /net/hafgola.hafro.is/var/ftp/pub/data/shapes/FAO_AREAS_CWP_NOCOASTLINE.gpkg")
}
