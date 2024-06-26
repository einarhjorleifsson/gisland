% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_sf_ftp.R
\name{gl_read_harbours}
\alias{gl_read_harbours}
\title{Read Icelandic harbours}
\usage{
gl_read_harbours(trim = TRUE)
}
\arguments{
\item{trim}{Boolean(default TRUE) filters out some smaller harbours, cause a nuisance downstream}
}
\value{
a tibble with harbour polygons
}
\description{
Read Icelandic harbours
}
