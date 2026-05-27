# Get Network Metrics Data for a Specific Network Axis

This function retrieves data about network metrics for a specific
network axis based on its ID.

## Usage

``` r
data_get_axis_dgos(selected_axis_id, aggregated = FALSE, con)
```

## Arguments

- selected_axis_id:

  The ID of the selected network axis.

- aggregated:

  A boolean indicating whether to retrieve aggregated data (TRUE) or
  detailed data (FALSE). Default is FALSE.

- con:

  Connection to Postgresql database.

## Value

A sf data frame containing information about network metrics for the
specified network axis.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
network_metrics_data <- data_get_axis_dgos(selected_axis_id = 2000796122, con = con, aggregated=TRUE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'sqlInterpolate': object 'con' not found
network_metrics_data <- data_get_axis_dgos(selected_axis_id = 2000796122, con = con, aggregated=FALSE)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'sqlInterpolate': object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
