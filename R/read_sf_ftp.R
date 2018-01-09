#' Read shapefile from ftp
#'
#' @param dsn The name of the shapefile (without extention)
#' @param shape_url The ftp address
#'
#' @return a simple feature dataframe
#' @export
#'
read_sf_ftp <- function(dsn, shape_url = "ftp://ftp.hafro.is/pub/reiknid/einar/shapes") {

  tmpdir <- download_shapefile(shape_url, dsn)
  sf::read_sf(paste0(tmpdir, "/", dsn, ".shp"))

}

#' Read shapefile from ftp
#'
#' @param dsn The name of the shapefile (without extention)
#' @param shape_url The ftp address
#'
#' @return a simple feature dataframe
#' @export
#'
read_as_df_ftp <- function(dsn, shape_url = "ftp://ftp.hafro.is/pub/reiknid/einar/shapes") {

  tmpdir <- gisland2:::download_shapefile(shape_url, dsn)
  rgdal::readOGR(paste0(tmpdir, "/", dsn, ".shp")) %>%
    ggplot2::fortify() %>%
    as_tibble()

}

download_shapefile <- function(shape_url, layer, outfile=layer) {
  # source: https://landeco2point0.wordpress.com/2013/09/30/an-r-function-to-download-shapefiles/

  #written by: jw hollister
  #Oct 10, 2012

  #set-up/clean-up variables
  if(length(grep("/$",shape_url))==0)
  {
    shape_url<-paste(shape_url,"/",sep="")
  }
  #creates vector of all possible shapefile extensions
  shapefile_ext<-c(".shp",".shx",".dbf",".prj",".sbn",".sbx",
                   ".shp.xml",".fbn",".fbx",".ain",".aih",".ixs",
                   ".mxs",".atx",".cpg")

  #Check which shapefile files exist
  if(require(RCurl))
  {
    xurl<-getURL(shape_url)
    xlogic<-NULL
    for(i in paste(layer,shapefile_ext,sep=""))
    {
      xlogic<-c(xlogic,grepl(i,xurl))
    }

    #Set-up list of shapefiles to download
    shapefiles<-paste(shape_url,layer,shapefile_ext,sep="")[xlogic]
    #Set-up output file names
    outfiles<-paste(outfile,shapefile_ext,sep="")[xlogic]   }

  # temporary directory
  tmpdir <- tempdir()

  #Download all shapefiles
  if(sum(xlogic)>0) {
    for(i in 1:length(shapefiles))
    {
      download.file(shapefiles[i], paste0(tmpdir, "/", outfiles[i]),
                    method="auto",mode="wb")
    }
  } else {
    stop("An Error has occured with the input URL
            or name of shapefile")
  }

  return(tmpdir)
}
