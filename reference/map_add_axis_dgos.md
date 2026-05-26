# Add Axis Data to an Existing Leaflet Map

This function adds axis data as polylines to an existing Leaflet map.
The axis are transparent, but when hovering over they appear red.

## Usage

``` r
map_add_axis_dgos(map, axis_data, Proposed_class, group)
```

## Arguments

- map:

  An existing Leaflet map to which axis data will be added.

- data_axis:

  A sf data frame containing axis data.

## Value

An updated Leaflet map with axis data added.
