# Get hydrometric sites.

This function retrieves the locations of the hydrometric sites from
Hubeau.

## Usage

``` r
data_get_hydro_sites(con)
```

## Arguments

- con:

  Connection to Postgresql database.

## Value

sf data frame containing the datapoints of the hydrometric sites
available on the server

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
hydro_sites <- data_get_hydro_sites(con = con)
#> Error: object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
