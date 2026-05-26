# This function retrieves the aggregated metrics for the whole network.

This function retrieves the aggregated metrics for the whole network.

## Usage

``` r
data_get_metrics(con)
```

## Arguments

- con:

  Connection to Postgresql database.

## Value

sf data frame containing the datapoints of the homogenous segments'
metrics

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
hydro_sites <- data_get_metrics(con = con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbGetQuery': object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
