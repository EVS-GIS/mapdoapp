# Create a vertical dashed line annotation for longitudinal profile plots

This function generates a vertical dashed line annotation for
longitudinal profile plots using the 'plotly' package.

## Usage

``` r
lg_vertical_line(x = 0, color = "purple")
```

## Arguments

- x:

  The x-coordinate where the vertical line should be positioned.

- color:

  The color of the vertical dashed line (default is "green").

## Value

A list object representing a vertical dashed line annotation.

## Examples

``` r
# see lg_profile_main() function to use it in a plotly graph
# Create a vertical dashed line annotation at x = 10 with a red color
vertical_line_annotation <- lg_vertical_line(x = 10, color = "red")
```
