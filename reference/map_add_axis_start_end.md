# Add start and end markers to a leaflet map

This function adds start and end markers to a Leaflet map based on the
provided start and end coordinates.

## Usage

``` r
map_add_axis_start_end(map, axis_start_end, group)
```

## Arguments

- map:

  A Leaflet map object created using the 'leaflet' package.

- axis_start_end:

  A data frame containing start and end coordinates with columns 'X' for
  longitude and 'Y' for latitude.

## Value

A Leaflet map object with start and end markers added.

## Examples

``` r
library(leaflet)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Create a simple Leaflet map
my_map <- leaflet() %>%
  setView(lng = 4.968697, lat = 45.103354, zoom = 8) %>%
  addProviderTiles(providers$CartoDB.Positron)

# Create a data frame with start and end coordinates
coordinates_df <- data_get_axis_start_end(network_axis %>%
                                            filter(fid == 5))
#> Error in (function (cond) .Internal(C_tryCatchHelper(addr, 1L, cond)))(structure(list(message = "argument \"con\" is missing, with no default",     call = data_get_axis_start_end(network_axis %>% filter(fid ==         5))), class = c("getvarError", "missingArgError", "error", "condition"))): error in evaluating the argument 'conn' in selecting a method for function 'sqlInterpolate': argument "con" is missing, with no default

# Add start and end markers to the map
my_map <- map_add_axis_start_end(my_map, axis_start_end = coordinates_df)
#> Warning: restarting interrupted promise evaluation
#> Warning: restarting interrupted promise evaluation
#> Warning: restarting interrupted promise evaluation
#> Warning: restarting interrupted promise evaluation
#> Error: argument "group" is missing, with no default
my_map

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"setView":[[45.103354,4.968697],8,[]],"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]}]},"evals":[],"jsHooks":[]}
```
