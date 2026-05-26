# Generate and display a legend for a map layer using a provided SLD (Styled Layer Descriptor) body.

This function constructs a legend for a map layer by sending a
GetLegendGraphic request to a WMS (Web Map Service) server.

## Usage

``` r
map_legend_metric(sld_body, wms_params)
```

## Arguments

- sld_body:

  A character string containing the SLD (Styled Layer Descriptor) body
  that defines the map layer's styling.

- wms_params:

  list containing the specific network-WMS parameters.

## Value

An HTML img tag representing the legend for the specified map layer.
