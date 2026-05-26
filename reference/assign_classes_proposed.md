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
classified_network <- network_dgo %>%
    assign_classes(proposed_class = "class_strahler",
    colors_df = globals$classes_proposed_colors)
#> Error in assign_classes(., proposed_class = "class_strahler", colors_df = globals$classes_proposed_colors): could not find function "assign_classes"
```
