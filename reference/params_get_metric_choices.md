# get nested list-object with all variables for Metric-selection in selectInput()-Elements

get nested list-object with all variables for Metric-selection in
selectInput()-Elements

## Usage

``` r
params_get_metric_choices()
```

## Value

list-object with first level the names of metric types and second levels
the corresponding metrics for each type

## Examples

``` r
params_get_metric_choices()
#> $`Elévation (m)`
#>          Elévation (m) 
#> "talweg_elevation_min" 
#> 
#> $`Largeurs (m)`
#>           Chenal actif (m)                   W étoilé 
#>     "active_channel_width"                   "w_star" 
#>       Corridor naturel (m)      Corridor connecté (m) 
#>   "natural_corridor_width" "connected_corridor_width" 
#>         Fond de vallée (m) 
#>      "valley_bottom_width" 
#> 
#> $Confinement
#> Indice de confinement  Marge de confinement 
#>     "idx_confinement"         "conf_margin" 
#> 
#> $Sinuosité
#>      Indice de sinuosité Angle de déflexion local 
#>              "sinuosite"            "angle_local" 
#> 
#> $`Multi-chenal`
#> Présence d'îles végétalisées            Nombre de chenaux 
#>          "iles_vegetalisees"        "multi_channel_index" 
#> 
#> $`Pentes (%)`
#>         Pente talweg (%) Pente fond de vallée (%) 
#>           "talweg_slope"       "floodplain_slope" 
#> 
#> $`Occupation du sol (%)`
#>              Surface en eau (%)           Banc sédimentaire (%) 
#>              "water_channel_pc"                "gravel_bars_pc" 
#>       Espace naturel ouvert (%)                       Forêt (%) 
#>               "natural_open_pc"                     "forest_pc" 
#>          Prairie permanente (%)                     Culture (%) 
#>                  "grassland_pc"                      "crops_pc" 
#>                  Périurbain (%)                Urbain dense (%) 
#>              "diffuse_urban_pc"                "dense_urban_pc" 
#> Infrastructure de transport (%) 
#>            "infrastructures_pc" 
#> 
#> $`Occupation du sol (ha)`
#>              Surface en eau (ha)           Banc sédimentaire (ha) 
#>                  "water_channel"                    "gravel_bars" 
#>       Espace naturel ouvert (ha)                       Forêt (ha) 
#>                   "natural_open"                         "forest" 
#>          Prairie permanente (ha)                     Culture (ha) 
#>                      "grassland"                          "crops" 
#>                  Périurbain (ha)                Urbain dense (ha) 
#>                  "diffuse_urban"                    "dense_urban" 
#> Infrastructure de transport (ha) 
#>                "infrastructures" 
#> 
#> $`Continuité latérale (%)`
#>            Bande active (%)        Corridor naturel (%) 
#>         "active_channel_pc"      "riparian_corridor_pc" 
#>   Corridor semi-naturel (%) Espace de réversibilité (%) 
#>           "semi_natural_pc"             "reversible_pc" 
#>       Espace déconnecté (%)    Espace artificialisé (%) 
#>           "disconnected_pc"      "built_environment_pc" 
#> 
#> $`Continuité latérale (ha)`
#>            Bande active (ha)        Corridor naturel (ha) 
#>             "active_channel"          "riparian_corridor" 
#>   Corridor semi-naturel (ha) Espace de réversibilité (ha) 
#>               "semi_natural"                 "reversible" 
#>       Espace déconnecté (ha)    Espace artificialisé (ha) 
#>               "disconnected"          "built_environment" 
#> 
```
