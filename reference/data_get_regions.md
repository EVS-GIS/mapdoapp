# Get hydrological regions

This function retrieves all hydrological regions available on the server
and converts them to sf

## Usage

``` r
data_get_regions(con, opacity = list(clickable = 0.01, not_clickable = 0.1))
```

## Arguments

- con:

  Connection to Postgresql database.

## Value

A sf data frame containing hydrological regions.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
data <- data_get_regions(con = con)
#> Error: object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
