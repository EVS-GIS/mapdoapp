# Get the start and end coordinates of a spatial object's axis

This function takes a spatial object with a LINESTRING geometry and
returns a data frame containing the start and end coordinates of the
axis.

## Usage

``` r
data_get_axis_start_end(dgo_axis)
```

## Arguments

- dgo_axis:

  A spatial sf object with a LINESTRING geometry representing an axis.

## Value

A data frame with two rows, where the first row contains the start
coordinates (x and y) and the second row contains the end coordinates (x
and y).

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
line_coords <- matrix(c(0, 0, 1, 1), ncol = 2)
# Create an sf object with the LINESTRING
line_sf <- st_sf(geometry = st_sfc(st_linestring(line_coords)))
df <- data_get_axis_start_end(line_sf)
```
