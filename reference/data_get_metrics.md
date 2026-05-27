# This function retrieves the aggregated metrics for the whole network.

This function retrieves the aggregated metrics for the whole network.

## Usage

``` r
data_get_metrics(
  con,
  filter_by_basin_id = NULL,
  filter_by_region_id = NULL,
  axis_id = NULL
)
```

## Arguments

- con:

  Connection to Postgresql database.

- filter_by_basin_id:

  if not NULL, filter the data by the specified basin id (cdbh)

- filter_by_region_id:

  if not NULL, filter the data by the specified region id (gid)

- axis_id:

  if not NULL, add a column "selected" to the data, with TRUE for the
  datapoints of the specified axis and FALSE for the others

## Value

sf data frame containing the datapoints of the homogenous segments'
metrics

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
data_get_metrics(con, "01","12","2000810055")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbGetQuery': object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
