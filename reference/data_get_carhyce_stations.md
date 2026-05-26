# Get Stations Carhyce

This function retrieves the datapoints of the CarHyCe hydrological
stations.

## Usage

``` r
data_get_carhyce_stations(con)
```

## Arguments

- con:

  Connection to Postgresql database.

## Value

A sf data frame containing the CarHyCe datapoints.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
carhyce_stations <- data_get_carhyce_stations(con = con)
#> Error: object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
