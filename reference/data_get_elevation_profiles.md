# Get elevation profiles data from selected dgo fid.

Get elevation profiles data from selected dgo fid.

## Usage

``` r
data_get_elevation_profiles(selected_dgo_fid, con)
```

## Arguments

- selected_dgo_fid:

  integer selected dgo fid.

- con:

  PqConnection to Postgresql database.

## Value

data.frame

## Examples

``` r
con <- db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
data_get_elevation_profiles(selected_dgo_fid = 95, con = con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'sqlInterpolate': object 'con' not found
DBI::dbDisconnect(con)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'conn' in selecting a method for function 'dbDisconnect': object 'con' not found
```
