# Generate and display a legend for a map layer using a pre-defined style

This function constructs a legend for a map layer by sending a
GetLegendGraphic request to a WMS (Web Map Service) server.

## Usage

``` r
map_legend_class(style, wms_params)
```

## Arguments

- style:

  A character string containing the name of the predefined style that
  defines the map layer's styling.

## Value

An HTML img tag representing the legend for the specified map layer.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
# Define an SLD body for a map layer
sld_body <- sld_get_style(breaks = sld_get_quantile_metric(
                                     selected_region_id = 11,
                                     selected_metric = "active_channel_width",
                                     con = con),
                          colors = sld_get_quantile_colors(
                                     quantile_breaks = sld_get_quantile_metric(
                                                         selected_region_id = 11,
                                                         selected_metric = "active_channel_width",
                                                         con = con)),
                          metric = "active_channel_width")
#> Error in sld_get_style(breaks = sld_get_quantile_metric(selected_region_id = 11,     selected_metric = "active_channel_width", con = con), colors = sld_get_quantile_colors(quantile_breaks = sld_get_quantile_metric(selected_region_id = 11,     selected_metric = "active_channel_width", con = con)), metric = "active_channel_width"): could not find function "sld_get_style"
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found

# Generate and display the legend for the map layer
legend <- map_legend_metric(sld_body)
#> Error in map_legend_metric(sld_body): argument "wms_params" is missing, with no default
```
