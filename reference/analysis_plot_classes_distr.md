# Create interactive stacked bar plots of class distribution for various scales

This function generates an interactive stacked bar plot showing the
class distribution for various scales (France, Basin, Region, and Axis).
The plot is colored based on class names, and the distribution is shown
as percentages.

## Usage

``` r
analysis_plot_classes_distr(df)
```

## Arguments

- df:

  A dataframe prepared by the \`prepare_selact_data_for_plot\` or
  \`prepare_regions_data_for_plot\` functions, containing the processed
  data ready for plotting.

## Value

Interactive stacked bar plot with class distribution percentages.

## Examples

``` r
if (FALSE) { # \dontrun{
plot <- analysis_plot_classes_distr(df)
} # }
```
