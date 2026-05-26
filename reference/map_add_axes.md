# Add Axis Data to an Existing Leaflet Map

This function adds axis data as simplified polylines to an existing
Leaflet map. The axis are transparent, but when hovering over they
appear red.

## Usage

``` r
map_add_axes(map, data_axis, group, selected_axis_id = NULL)
```

## Arguments

- map:

  An existing Leaflet map to which axis data will be added.

- data_axis:

  A sf data frame containing axis data.

- selected_axis:

  id of selected axis, so that it will not be included in mapping

## Value

An updated Leaflet map with axis data added.
