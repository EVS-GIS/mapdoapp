# Get statistics on classes distributions for different levels (france, basin, region) and manually specified classes

Get statistics on classes distributions for different levels (france,
basin, region) and manually specified classes

## Usage

``` r
data_get_distr_class_man(con, manual_classes_table)
```

## Arguments

- con:

  Connection to Postgresql database.

- manual_classes_table:

  Dataframe of manually defined classes, with 4 columns: variable,
  class, greaterthan, color

## Value

Dataframe which contains the statistics for the specified class for
different entities: France, Basins, Regions available on the server
