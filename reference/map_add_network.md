# Map the hydrographique network

Adds the hydrographique network with a custom styling and regional
constraint to the map

## Usage

``` r
map_add_network(
  map,
  wms_params_network,
  group,
  cql_filter = "",
  style = "mapdo:classes_proposed_strahler"
)
```

## Arguments

- map:

  An existing Leaflet map to which WMS tiles will be added.

- cql_filter:

  A CQL filter to apply to the WMS request.

- wms_params:

  list containing the specific network-WMS parameters.

## Value

An updated Leaflet map with WMS tiles of the styled network. Standard
styling according to strahler-order and standard regional constraint:
whole France.

## Examples

``` r
map %>%
map_add_network(wms_params = params_wms()$network, cql_filter = "gid_region <> 11", style = "mapdo:classes_proposed_urban")
#> Error: object 'map' not found
```
