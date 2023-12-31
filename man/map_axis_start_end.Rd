% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_map.R
\name{map_axis_start_end}
\alias{map_axis_start_end}
\title{Add start and end markers to a leaflet map}
\usage{
map_axis_start_end(map, axis_start_end, region_axis)
}
\arguments{
\item{map}{A Leaflet map object created using the 'leaflet' package.}

\item{axis_start_end}{A data frame containing start and end coordinates with
columns 'X' for longitude and 'Y' for latitude.}

\item{region_axis}{A data frame containing region-specific axes to be displayed.}
}
\value{
A Leaflet map object with start and end markers added.
}
\description{
This function adds start and end markers to a Leaflet map based on the provided
start and end coordinates.
}
\examples{
library(leaflet)
library(dplyr)

# Create a simple Leaflet map
my_map <- leaflet() \%>\%
  setView(lng = 4.968697, lat = 45.103354, zoom = 8) \%>\%
  addProviderTiles(providers$CartoDB.Positron)

# Create a data frame with start and end coordinates
coordinates_df <- data_get_axis_start_end(network_axis \%>\%
                                            filter(fid == 5))

# Add start and end markers to the map
my_map <- map_axis_start_end(my_map, axis_start_end = coordinates_df,
                             region_axis = network_axis)
my_map

}
