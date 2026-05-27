# Get statistics on classes distributions for different levels (france, basin, region) and a specified class

Get statistics on classes distributions for different levels (france,
basin, region) and a specified class

## Usage

``` r
data_get_distr_class(con, class_name_selected)
```

## Arguments

- con:

  Connection to Postgresql database.

- class_name:

  Name of the proposed class for which stats should be generated

## Value

a dataframe which contains the statistics for the specified class for
different entities: France, Basins, Regions available on the serve

## Examples

``` r
con=db_con()
#> Error: connection to server on socket "/var/run/postgresql/.s.PGSQL.5432" failed: No such file or directory
#>  Is the server running locally and accepting connections on that socket?
data_get_distr_class(con,"class_style")
#> Error in data_get_distr_class(con, "class_style"): could not find function "data_get_distr_class"
```
