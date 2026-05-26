# get IGN remonterletemps url.

This function take longitude and latitude to build and url to go IGN
remonterletemps website on the same place.

## Usage

``` r
utils_url_remonterletemps(lng = 6.869433, lat = 45.92369, zoom = 12)
```

## Arguments

- lng:

  longitude.

- lat:

  latitude.

- zoom:

  zoom level.

## Value

a string with url link.

## Examples

``` r
utils_url_remonterletemps(lng=6.869433, lat=45.923690, zoom = 12)
#> https://remonterletemps.ign.fr/comparer/basic?x=6.869433&y=45.92369&z=12&layer1=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&layer2=ORTHOIMAGERY.ORTHOPHOTOS&mode=vSlider
```
