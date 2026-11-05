# Get Network Metrics Data for a Specific Basin

This function retrieves data about network metrics for a specific basin
based on its ID.

## Usage

``` r
data_get_axis_dgos_from_basin(selected_basin_id, con)
```

## Arguments

- selected_basin_id:

  The ID of the selected basin.

- con:

  Connection to Postgresql database.

## Value

A sf data frame containing information about network metrics for the
specified basin.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
network_metrics_data <- data_get_axis_dgos_from_basin(selected_basin_id = 33, con = con)
#> Error: object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
