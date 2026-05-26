# Generate and display a legend for a WMS (Web Map Service) layer overlay using specified WMS parameters.

This function constructs a legend for a WMS layer overlay by sending a
GetLegendGraphic request to a WMS server.

## Usage

``` r
map_legend_wms_overlayer(wms_params)
```

## Arguments

- wms_params:

  A list containing the following parameters for the WMS layer overlay:

  - `language` (character): The language to use for the legend.

  - `version` (character): The version of the WMS service.

  - `service` (character): The WMS service type (e.g., "WMS").

  - `sld_version` (character): The SLD (Styled Layer Descriptor)
    version.

  - `layer` (character): The name of the WMS layer for which the legend
    is generated.

  - `format` (character): The desired format of the legend image (e.g.,
    "image/png").

  - `style` (character): The style to use for rendering the legend.

  - `url` (character): The URL of the WMS server.

## Value

An HTML div element containing an img tag representing the legend for
the specified WMS layer overlay.

## Examples

``` r
# Define WMS parameters for a layer overlay like
wms_params <- params_wms()$inondation

# Generate and display the legend for the WMS layer overlay
legend <- map_legend_wms_overlayer(wms_params)
print(legend)
#> [1] "https://georisques.gouv.fr/services?LANGUAGE=fre&VERSION=1.3.0&SERVICE=WMS&REQUEST=GetLegendGraphic&SLD_VERSION=1.1.0&LAYER=ALEA_SYNT_01_02MOY_FXX&FORMAT=image%2Fpng&STYLE=inspire_common%3ADEFAULT&LEGEND_OPTIONS=dpi%3A80"
```
