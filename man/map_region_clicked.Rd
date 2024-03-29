% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_map.R
\name{map_region_clicked}
\alias{map_region_clicked}
\title{Update Leaflet Map for a Clicked Region}
\usage{
map_region_clicked(
  map,
  region_click,
  selected_region_feature,
  regions_data,
  roe_region,
  hydro_sites_region
)
}
\arguments{
\item{map}{An existing Leaflet map to be updated.}

\item{region_click}{A vector containing information about the clicked region.}

\item{selected_region_feature}{A sf data frame containing information about the selected region feature.}

\item{regions_data}{A sf data.frame with the hydrographic regions of the bassin selected.}

\item{roe_region}{sf data.frame ROE in selected region.}

\item{hydro_sites_region}{sf data.frame hydrometric sites in selected region.}
}
\value{
An updated Leaflet map with relevant layers and information displayed.
}
\description{
This function updates an existing Leaflet map when a region is clicked, displaying the region and overlayers.
}
\examples{
library(leaflet)
library(dplyr)
library(sf)
# Create init bassin map
map_bassin <- map_init_bassins(bassins_data = bassin_hydrographique,
                               id_logo_ign_remonterletemps = "logo_ign_remonterletemps")

# simulate bassin selected
selected_bassin <- bassin_hydrographique

# get centroid coordinate (in shiny see leaflet mapid_shape_click)
centre <- sf::st_centroid(selected_bassin)
centre_coord <- as.data.frame(st_coordinates(centre)) \%>\%
  rename("lng" = X,
         "lat" = Y)

# map region
map_region <- map_add_regions_in_bassin(map = map_bassin,
                                        bassins_data = bassin_hydrographique,
                                        bassin_click = centre_coord,
                                        regions_data = region_hydrographique)
# simulate selected region
selected_region <- region_hydrographique

# get centroid coordinate (in shiny see leaflet mapid_shape_click)
centre_region <- sf::st_centroid(selected_bassin)
centre_region_coord <- as.data.frame(st_coordinates(centre_region)) \%>\%
  rename("lng" = X,
         "lat" = Y)
centre_region_coord$id <- 11

con <- db_con()
# get ROE in region
roe_region <- data_get_roe_in_region(centre_region_coord$id, con = con)
# get hydro sites in region
hydro_sites_region <- data_get_hydro_sites(centre_region_coord$id, con = con)
DBI::dbDisconnect(con)

# map the element in the region clicked
map <- map_region_clicked(map = map_region,
                          region_click = centre_region_coord,
                          selected_region_feature = selected_region,
                          regions_data = region_hydrographique,
                          roe_region = roe_region,
                          hydro_sites_region = hydro_sites_region)
map

}
