# Get Network Axes Data

This function retrieves data about the network axes available on the
server

## Usage

``` r
data_get_axes(con)
```

## Arguments

- con:

  Connection to Postgresql database.

## Value

A sf data frame containing the network axes.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
axis_data <- data_get_axes(con = con)
#> Error: object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
