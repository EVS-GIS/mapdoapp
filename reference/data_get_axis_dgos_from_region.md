# Get Network Metrics Data for a Specific Region

This function retrieves data about network metrics for a specific region
based on its ID.

## Usage

``` r
data_get_axis_dgos_from_region(selected_region_id, con)
```

## Arguments

- con:

  Connection to Postgresql database.

- selected_axis_id:

  The ID of the selected region.

## Value

A sf data frame containing information about network metrics for the
specified region.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
network_metrics_data <- data_get_axis_dgos_from_region(selected_region_id = 33, con = con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'sqlInterpolate': object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
