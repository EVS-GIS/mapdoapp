# Get Hydrographic Basins

This function retrieves hydrographic basins.

## Usage

``` r
data_get_basins(con, opacity = list(clickable = 0.01, not_clickable = 0.1))
```

## Arguments

- con:

  Connection to Postgresql database.

- opacity:

  list that contain numeric values clickable and not_clickable to inform
  the user the non available features.

## Value

sf data frame containing information about hydrographic basins.

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
opacity = list(clickable = 0.01,
               not_clickable = 0.10)

data <- data_get_basins(con = con, opacity = opacity)
#> Error: object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
