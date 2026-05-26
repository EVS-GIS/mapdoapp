# Get Parameters for Map Layer Groups

This function returns a list of parameters representing different map
layer groups.

## Usage

``` r
params_map_group(wms_params)
```

## Value

A list of parameters including names for groups such as "BASSIN,"
"REGION," "SELECT_REGION," "METRIC," "AXIS," "LEGEND," and "ROE".

## Examples

``` r
# all group available
map_group_params <- params_map_group()
#> Error in params_map_group(): argument "wms_params" is missing, with no default
# get specific group name
map_metric_group <- params_map_group()$metric
#> Error in params_map_group(): argument "wms_params" is missing, with no default
map_selected_region_group <- params_map_group()$select_region
#> Error in params_map_group(): argument "wms_params" is missing, with no default
```
