# Get statistics on classes distributions for different levels (france, basin, region) and a specified class

Get statistics on classes distributions for different levels (france,
basin, region) and a specified class

## Usage

``` r
data_get_distr_class(con, class_name)
```

## Arguments

- con:

  Connection to Postgresql database.

- class_name:

  Name of the proposed class for which stats should be generated

## Value

Dataframe which contains the statistics for the specified class for
different entities: France, Basins, Regions available on the server
