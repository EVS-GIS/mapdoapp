# Create reactable table for metrics-statistics

Create reactable table for metrics-statistics

## Usage

``` r
create_analysis_table(df, vars, scale_name = "")
```

## Arguments

- df:

  metric-statistics dataframe with \*\_distr-columns

- vars:

  metric names to be included in table

- strahler_sel:

  selected Strahler order, default is 0 which represents an aggregate of
  the whole entity

## Value

reactable table with variables and sparklines

## Examples

``` r
create_table(df, vars = c("crops_pc", "dense_urban_pc", "dense_urban"))
#> Error in create_table(df, vars = c("crops_pc", "dense_urban_pc", "dense_urban")): could not find function "create_table"
```
