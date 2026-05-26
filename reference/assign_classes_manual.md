# Assign classes to network dgos

Assign classes to network dgos

## Usage

``` r
assign_classes_manual(data, classes)
```

## Arguments

- data:

  dataframe or sf object which contains dgos of axis or region

- classes:

  df containing columns variables, greater_thans, class_names, colors
  which define the classification of the network

## Value

classified dataframe/sf object with additional variables: class_name and
color

## Examples

``` r
classified_network <- network_dgo %>%
    assign_classes(variables = as.character(r_val$grouping_table_data$variable),
    greater_thans = r_val$grouping_table_data$greaterthan,
    class_names = r_val$grouping_table_data$class)
#> Error in assign_classes(., variables = as.character(r_val$grouping_table_data$variable),     greater_thans = r_val$grouping_table_data$greaterthan, class_names = r_val$grouping_table_data$class): could not find function "assign_classes"
```
