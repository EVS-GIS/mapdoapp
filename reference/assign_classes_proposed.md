# Assign classification with colors to network dgos

Assign classification with colors to network dgos

## Usage

``` r
assign_classes_proposed(data, proposed_class, colors_df)
```

## Arguments

- data:

  dataframe or sf object containing all dgos of an axis

- proposed_class:

  string indicating the type of classification which should be applied
  to the data

## Value

classified dataframe/sf object with additional variables: class_name and
color

## Examples

``` r

network_metrics_data <- data_get_axis_dgos(selected_axis_id = 2000796122, con = con, aggregated=TRUE) %>%
    assign_classes_proposed(proposed_class = "class_style",
                             colors_df = params_classes_colors())
#> Error in assign_classes_proposed(., proposed_class = "class_style", colors_df = params_classes_colors()): could not find function "assign_classes_proposed"
```
