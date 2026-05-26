# Get Referentiel des Obstacles aux Ecoulement Data

This function retrieves the datapoints of the Referentiel des Obstacles
aux Ecoulement (ROE).

## Usage

``` r
data_get_roe_sites(con)
```

## Arguments

- con:

  Connection to Postgresql database.

## Value

A sf data frame containing the ROE datapoints.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
roe_data <- data_get_roe_sites(con = con)
#> Error: object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
