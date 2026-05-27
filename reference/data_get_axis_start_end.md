# Get the start and end coordinates of a spatial object's axis

This function takes a spatial object with a LINESTRING geometry and
returns a data frame containing the start and end coordinates of the
axis.

## Usage

``` r
data_get_axis_start_end(selected_axis_id, con)
```

## Arguments

- selected_axis_id:

  The ID of the selected axis for which to retrieve the start and end
  coordinates.

- con:

  Connection to Postgresql database.

## Value

A data frame with two rows, where the first row contains the start
coordinates (x and y) and the second row contains the end coordinates (x
and y).

## Examples

``` r

df <- data_get_axis_start_end(line_sf)
#> Error in (function (cond) .Internal(C_tryCatchHelper(addr, 1L, cond)))(structure(list(message = "argument \"con\" is missing, with no default",     call = data_get_axis_start_end(line_sf)), class = c("getvarError", "missingArgError", "error", "condition"))): error in evaluating the argument 'conn' in selecting a method for function 'sqlInterpolate': argument "con" is missing, with no default
```
