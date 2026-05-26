# Add a metric layer with custom symbology to the map.

This function adds a metric layer with custom symbology based on
user-defined classes to a leaflet map. It allows you to specify custom
parameters for the Web Map Service (WMS) request and customize the SLD
(Styled Layer Descriptor) body for styling the layer.

## Usage

``` r
map_add_network_metric(
  map,
  wms_params = globals$wms_params$metric,
  breaks = c(0, 531.39),
  colors = c("#67A9CF", "#BDD1BA"),
  metric = "talweg_elevation_min",
  sld_legend = "",
  group = "Réseau hydrographique"
)
```

## Arguments

- map:

  A leaflet map object to which the metric layer will be added.

- wms_params:

  A list containing WMS parameters for the metric layer. If not
  provided, default parameters are retrieved using the
  [`params_wms`](https://evs-gis.github.io/mapdoapp/reference/params_wms.md)
  function.

- breaks:

  A numeric vector containing quantile breaks.

- colors:

  A character vector of colors generated based on quantile breaks.

- metric:

  The name of the selected metric.

- sld_legend:

  A character string representing the SLD (Styled Layer Descriptor) body
  for creation of the legend.

- group:

  The name of the group to which the metric layer will be added.

## Value

A leaflet map object with the metric layer and its legend added.
