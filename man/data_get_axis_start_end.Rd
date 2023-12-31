% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data.R
\name{data_get_axis_start_end}
\alias{data_get_axis_start_end}
\title{Get the start and end coordinates of a spatial object's axis}
\usage{
data_get_axis_start_end(dgo_axis)
}
\arguments{
\item{dgo_axis}{A spatial sf object with a LINESTRING geometry representing an axis.}
}
\value{
A data frame with two rows, where the first row contains the start
        coordinates (x and y) and the second row contains the end coordinates (x and y).
}
\description{
This function takes a spatial object with a LINESTRING geometry and returns
a data frame containing the start and end coordinates of the axis.
}
\examples{
library(sf)
line_coords <- matrix(c(0, 0, 1, 1), ncol = 2)
# Create an sf object with the LINESTRING
line_sf <- st_sf(geometry = st_sfc(st_linestring(line_coords)))
df <- data_get_axis_start_end(line_sf)

}
