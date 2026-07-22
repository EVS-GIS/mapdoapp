# Create initial dataframe for 1-variable classification, to be displayed on the UI table

Create initial dataframe for 1-variable classification, to be displayed
on the UI table

## Usage

``` r
create_df_input(variable_name, q_0025, q_0975, quantile = 95, no_classes = 4)
```

## Arguments

- variable_name:

  name of variable for which the classification should be undertaken

- q_0025:

  2.5%-quantile value of selected metric

- q_0975:

  97.5%-quantile value of selected metric

- quantile:

  size of quantile which provides value-range of classification

- no_classes:

  number of classes to be generated

## Value

dataframe with 4 columns: class (name of each class, here automatically
set from A-Z), variable (variable chosen for classification),
greaterthan (values defining the threshold of each class), and color
(defining the coloring for the map)

## Examples

``` r
df <- create_df_input(
      variable_name = "crops_pc",
      q_0025 = 5,
      q_0975 = 346,
      quantile = 75,
      no_classes = 4
      )
#> Error in create_df_input(variable_name = "crops_pc", q_0025 = 5, q_0975 = 346,     quantile = 75, no_classes = 4): could not find function "create_df_input"
```
