# Get Styled Layer Descriptor (SLD) for legend of manually classified network_metrics layer

This function generates a Styled Layer Descriptor (SLD) XML for the
legend of the network metric layer which is manually classified based on
quantile breaks colors.

## Usage

``` r
sld_get_style_legend(breaks, colors, classnames, metric)
```

## Arguments

- breaks:

  A numeric vector containing quantile breaks.

- colors:

  A character vector of colors generated based on quantile breaks.

- metric:

  The name of the selected metric.

## Value

A character string containing the SLD XML for styling data
visualization.
