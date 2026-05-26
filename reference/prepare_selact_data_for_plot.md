# Prepare selected data for interactive class distribution plot

This function processes the network data and prepares it for plotting
the class distribution across different scales (France, Basin, Region,
and Axis). It applies filters, computes class shares, and generates
color mappings.

## Usage

``` r
prepare_selact_data_for_plot(
  data,
  classification_type = "classes",
  manual_classes_table = NULL,
  basin_id = NULL,
  region_id = NULL,
  strahler = 0,
  region_names = NULL,
  axis_data = NULL
)
```

## Arguments

- data:

  A dataframe containing classified network data with regional and axis
  information.

- basin_id:

  Integer specifying the basin ID. Default is NULL.

- region_id:

  Integer specifying the region ID. Default is NULL.

- strahler:

  Integer for Strahler order. Default is 0.

- region_names:

  Dataframe containing region names and corresponding IDs.

- axis_data:

  Dataframe containing axis data (optional). Default is NULL.

## Value

A prepared dataframe with class distribution percentages and color
mappings for plotting.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- prepare_selact_data_for_plot(data = classified_data, basin_id = 3, region_id = 1)
} # }
```
