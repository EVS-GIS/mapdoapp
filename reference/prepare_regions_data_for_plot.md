# Prepare region-level data for interactive class distribution plot

This function processes the network data at the region level, preparing
it for plotting the class distribution for specific regions. It filters
data, calculates shares, and renames region scales based on the provided
region names.

## Usage

``` r
prepare_regions_data_for_plot(
  data,
  classification_type = "classes",
  manual_classes_table = NULL,
  region_id = NULL,
  region_strahler = 0,
  region_names = NULL
)
```

## Arguments

- data:

  A dataframe containing classified network data.

- region_id:

  Integer specifying the region ID. Default is NULL.

- region_strahler:

  Integer for Strahler order of the region. Default is 0.

- region_names:

  Dataframe containing region names and corresponding IDs.

## Value

A prepared dataframe with class distribution percentages and color
mappings for plotting.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- prepare_regions_data_for_plot(data = classified_data, region_id = 1, region_names = region_names_df)
} # }
```
