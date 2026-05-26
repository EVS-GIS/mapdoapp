# Define Web Map Service (WMS) parameters for different map layers and basemaps.

This function defines a set of WMS parameters for various map layers and
basemaps. The parameters include information such as the layer name, URL
of the WMS server, version, format, style, and more. These parameters
are organized into a list, making it easy to configure and access them
for map display and legend generation.

## Usage

``` r
params_wms()
```

## Value

A list containing WMS parameters for different map layers and basemaps.

## Examples

``` r
# Retrieve WMS parameters for a specific map layer
wms_params <- params_wms()
metric_wms_params <- wms_params$metric

# Access specific WMS parameters
metric_name <- metric_wms_params$name
metric_url <- metric_wms_params$url
```
