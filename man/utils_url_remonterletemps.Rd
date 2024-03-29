% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_utils.R
\name{utils_url_remonterletemps}
\alias{utils_url_remonterletemps}
\title{get IGN remonterletemps url.}
\usage{
utils_url_remonterletemps(lng = 6.869433, lat = 45.92369, zoom = 12)
}
\arguments{
\item{lng}{longitude.}

\item{lat}{latitude.}

\item{zoom}{zoom level.}
}
\value{
a string with url link.
}
\description{
This function take longitude and latitude to build and url to go IGN remonterletemps website on the same place.
}
\examples{
utils_url_remonterletemps(lng=6.869433, lat=45.923690, zoom = 12)
}
