# Create a Biplot with Linear Regression and Statistical Annotations

This function creates a biplot using the \`plotly\` library that
displays a scatterplot of two metrics from a given dataframe, fits a
linear regression line, and shows key statistical measures (correlation,
R², p-value) in a floating text box.

## Usage

``` r
create_analysis_biplot(
  df,
  metric_x,
  metric_y,
  classes = FALSE,
  lm = FALSE,
  axis_name
)
```

## Arguments

- df:

  A data frame containing the metrics to be plotted.

- metric_x:

  A string specifying the name of the x-axis metric (column in the
  dataframe).

- metric_y:

  A string specifying the name of the y-axis metric (column in the
  dataframe).

- classes:

  A boolean indicating whether to group the data by a categorical
  variable.

- lm:

  A boolean indicating whether to add a linear regression line.

## Value

A \`plotly\` plot object showing the biplot, regression line, and
statistical annotations.

## Examples

``` r
create_analysis_biplot(df = mtcars, metric_x = "mpg", metric_y = "wt", classes = FALSE, lm = TRUE)
#> Error in create_analysis_biplot(df = mtcars, metric_x = "mpg", metric_y = "wt",     classes = FALSE, lm = TRUE): object 'globals' not found
```
