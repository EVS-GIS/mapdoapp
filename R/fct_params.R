#' Define Web Map Service (WMS) parameters for different map layers and basemaps.
#'
#' This function defines a set of WMS parameters for various map layers and basemaps. The parameters include information such as the layer name, URL of the WMS server, version, format, style, and more. These parameters are organized into a list, making it easy to configure and access them for map display and legend generation.
#'
#' @return A list containing WMS parameters for different map layers and basemaps.
#'
#' @examples
#' # Retrieve WMS parameters for a specific map layer
#' wms_params <- params_wms()
#' metric_wms_params <- wms_params$metric
#'
#' # Access specific WMS parameters
#' metric_name <- metric_wms_params$name
#' metric_url <- metric_wms_params$url
#'
#' @export
params_wms <- function(){
  wms <- list(metric = list(name = "Métrique",
                            url = Sys.getenv("GEOSERVER"),
                            language = "",
                            service = "WMS",
                            version = "1.0.0",
                            sld_version = "",
                            layer = "mapdo:network_metrics",
                            format = "image/png",
                            sld = "",
                            style = "", # no style, sld_body define style and legend
                            attribution = "CNRS - EVS",
                            basemap = FALSE,
                            overlayer = FALSE),
              class = list(name = "Style Fluvial",
                           url = Sys.getenv("GEOSERVER"),
                           language = "",
                           service = "WMS",
                           version = "1.1.0",
                           sld_version = "",
                           layer = "mapdo:network_metrics",
                           format = "image/png",
                           sld = "",
                           style = "", # no style, will be defined depending on selection
                           attribution = "CNRS - EVS",
                           basemap = FALSE,
                           overlayer = FALSE),
              network = list(name = "Réseau hydrographique",
                           url = Sys.getenv("GEOSERVER"),
                           language = "",
                           service = "WMS",
                           version = "1.1.0",
                           sld_version = "",
                           layer = "mapdo:network_metrics",
                           format = "image/png",
                           sld = "",
                           style = "", # no style, will be defined depending on selection
                           attribution = "CNRS - EVS",
                           basemap = FALSE,
                           overlayer = FALSE),
              background = list(name = "Background",
                                url = Sys.getenv("GEOSERVER"),
                                language = "",
                                service = "WMS",
                                version = "1.1.0",
                                sld_version = "",
                                layer = "mapdo:network_metrics",
                                format = "image/png",
                                sld = "",
                                style = "mapdo:classes_proposed_strahler",
                                attribution = "CNRS - EVS",
                                basemap = FALSE,
                                overlayer = FALSE),
              carteign = list(name = "Plan IGN",
                              url = 'https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE={style}&TILEMATRIXSET=PM&FORMAT={format}&LAYER=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}',
                              language = "",
                              service = "WMTS",
                              version = "",
                              sld_version = "",
                              layer = "GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2",
                              format = "image/png",
                              sld = "",
                              style = "normal",
                              attribution = "IGN-F/Géoportail",
                              basemap = TRUE,
                              overlayer = FALSE),
              ortho = list(name = "Satellite IGN",
                           url = 'https://data.geopf.fr/wmts?REQUEST=GetTile&SERVICE=WMTS&VERSION=1.0.0&STYLE={style}&TILEMATRIXSET=PM&FORMAT={format}&LAYER=ORTHOIMAGERY.ORTHOPHOTOS&TILEMATRIX={z}&TILEROW={y}&TILECOL={x}',
                           language = "",
                           service = "WMTS",
                           version = "",
                           sld_version = "",
                           layer = "HR.ORTHOIMAGERY.ORTHOPHOTOS",
                           format = "image/jpeg",
                           sld = "",
                           style = "normal",
                           attribution = "IGN-F/Géoportail",
                           basemap = TRUE,
                           overlayer = FALSE),
              # elevation = list(name = "Elévation IGN",
              #     url = 'https://data.geopf.fr/wms-r?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap',
              #   language = "",
              #   service = "WMS",
              #   version = "1.3.0",
              #   layer = "ELEVATION.ELEVATIONGRIDCOVERAGE.HIGHRES",
              #   format = "image/jpeg",
              #   sld = "",
              #   style = "hypso",  # Or another valid style
              #   attribution = "IGN-F/Géoportail",
              #   basemap = TRUE,
              #   overlayer = FALSE),
              landuse = list(name = "Occupation du sol",
                             url = Sys.getenv("GEOSERVER"),
                             language = "",
                             service = "WMS",
                             version = "1.0.0",
                             sld_version = "",
                             layer = "mapdo:mapdo_landuse_1m",
                             format = "image/png",
                             sld = "",
                             style = "mapdo:MAPDO landuse",
                             attribution = "CNRS - EVS",
                             basemap = TRUE,
                             overlayer = FALSE),
              geologie = list(name = "Géologie",
                              url = "http://geoservices.brgm.fr/geologie",
                              language = "",
                              service = "WMS",
                              version = "",
                              sld_version = "",
                              layer = "GEOLOGIE",
                              format = "image/png",
                              sld = "",
                              style = "",
                              attribution = "BRGM",
                              basemap = TRUE,
                              overlayer = FALSE),
              detrend_dem = list(name = "MNT détendancé",
                                 url = Sys.getenv("GEOSERVER"),
                                 language = "",
                                 service = "WMS",
                                 version = "1.0.0",
                                 sld_version = "",
                                 layer = " 	mapdo:mapdo_nearest_height_hillshade_1m ",
                                 format = "image/png",
                                 sld = "",
                                 style = "",
                                 attribution = "CNRS - EVS",
                                 basemap = FALSE,
                                 overlayer = TRUE),
              valley_bottom = list(name = "Fond de vallée",
                                   url = Sys.getenv("GEOSERVER"),
                                   language = "",
                                   service = "WMS",
                                   version = "1.0.0",
                                   sld_version = "",
                                   layer = "mapdo:mapdo_valley_bottom_1m",
                                   format = "image/png",
                                   sld = "",
                                   style = "mapdo:MAPDO valley bottom",
                                   attribution = "CNRS - EVS",
                                   basemap = FALSE,
                                   overlayer = TRUE),
              continuity = list(name = "Continuité latérale",
                                url = Sys.getenv("GEOSERVER"),
                                language = "",
                                service = "WMS",
                                version = "1.0.0",
                                sld_version = "",
                                layer = "mapdo:mapdo_continuity_1m",
                                format = "image/png",
                                sld = "",
                                style = "mapdo:MAPDO continuity",
                                attribution = "CNRS - EVS",
                                basemap = FALSE,
                                overlayer = TRUE),
              inondation = list(name = "Zone inondable centennale",
                                url = "https://georisques.gouv.fr/services",
                                language = "fre",
                                service = "WMS",
                                version = "1.3.0",
                                sld_version = "1.1.0",
                                layer = "ALEA_SYNT_01_02MOY_FXX",
                                format = "image/png",
                                sld = "",
                                style = "inspire_common:DEFAULT",
                                attribution = "Georisques",
                                basemap = FALSE,
                                overlayer = TRUE),
              ouvrage_protection = list(name = "Ouvrage protection inondation",
                                        url = "https://georisques.gouv.fr/services",
                                        language = "fre",
                                        service = "WMS",
                                        version = "1.3.0",
                                        sld_version = "1.1.0",
                                        layer = "OUV_PROTECTION_FXX",
                                        format = "image/png",
                                        sld = "",
                                        style = "inspire_common:DEFAULT",
                                        attribution = "Georisques",
                                        basemap = FALSE,
                                        overlayer = TRUE)
  )
  return(wms)
}


#' Get Parameters for Map Layer Groups
#'
#' This function returns a list of parameters representing different map layer groups.
#'
#' @return A list of parameters including names for groups such as "BASSIN," "REGION," "SELECT_REGION," "METRIC," "AXIS," "LEGEND," and "ROE".
#'
#' @examples
#' # all group available
#' map_group_params <- params_map_group()
#' # get specific group name
#' map_metric_group <- params_map_group()$metric
#' map_selected_region_group <- params_map_group()$select_region
#'
#' @export
params_map_group <- function(wms_params){
  params <- list(
    bassin = "Bassins",
    region = "Régions",
    select_region = "SELECT_REGION",
    network = "Réseau hydrographique",
    metric = "METRIC",
    class = "CLASS",
    background = "BACKGROUND",
    axis = "AXIS",
    dgo_axis = "DGOAXIS",
    dgo = "DGO",
    axis_start_end = "AXIS_START_END",
    axis_opacity = "AXIS_OPACITY",
    legend = "LEGEND",
    roe = "Obstacles à l'ecoulement",
    hydro_sites = "Sites hydrométriques",
    light = "LIGHT",
    inondation = wms_params$inondation$name,
    ouvrage_protection = wms_params$ouvrage_protection$name,
    landuse = wms_params$landuse$name,
    continuity = wms_params$continuity$name,
    valley_bottom = wms_params$valley_bottom$name,
    detrend_dem = wms_params$detrend_dem$name,
    carteign = wms_params$carteign$name,
    ortho = wms_params$ortho$name,
    elevation = wms_params$elevation$name,
    geologie = wms_params$geologie$name
  )

  return(params)
}

#' Get names and description for proposed classifications
#'
#' @importFrom tibble tibble
#'
#' @return tibble with names of classes and their descriptions
#' @export
#'
params_classes <- function() {

  df <- tibble(
    class_title = c(
      "Ordre de Strahler",
      "Topographie",
      "Utilisation dominante des sols",
      "Pression urbaine",
      "Pression agricole",
      "Utilisation naturelle des sols",
      "Présence de bancs sédimentaires",
      "Confinement de la bande active",
      "Connectivité des habitats riverains"
    ),
    description = c(
      # strahler
      "Répresent la complexité du réseaux hydrographique. L'ordre de Strahler est de 1 pour tout cours d'eau entre sa source et sa première confluence et mont avec chaque confluence.",
      # topographie
      "Classification simple basée sur la pente et la hauteur du cours de la rivière :
      - plaines de basse altitude (> 0 m & < 0.5 % pente)
      - plaines de moyenne altitude (> 300 m & < 0.5 % pente)
      - plaines de montagne (> 1000 m & < 0.5 % pente)
      - pentes de basse altitude (> 0 m & > 0.5 % pente)
      - pentes de moyenne altitude (> 300 m & > 0.5 % pente)
      - pentes de montagne (> 1000 m & > 0.5 % pente)
      ",
      # dominant land use
      "Indique la classe d'utilisation des sols la plus dominante dans la zone du fond de vallée de chaque segment de cours d'eau :
      - Forêt
      - Prairies (et sols nus)
      - Cultures
      - Espace construit (zones urbaines et infrastructures)
      ",
      # urban areas
      "Indique le degré de couverture urbaine du fond de vallée du segment :
      - fortement urbanisé (> 70 % zones construites)
      - urbanisé  (> 40 % zones construites)
      - modérément urbanisé (> 10 % zones construites)
      - Presque pas/pas urbanisé (< 10 % zones construites)",
      # agriculture
      "indique la part de l'utilisation des terres agricoles dans la zone du fond de vallée de chaque segment de cours d'eau
      - Forte impact agricole (> 70 % cultures)
      - Impact agricole élevé (> 40 % cultures)
      - Impact agricole modéré (> 10 % cultures)
      - Presque pas/pas d'impact agricole (< 10 % cultures)",
      # natural
      "indique la part de l'occupation naturelle des sols dans la zone du fond de vallée de chaque tronçon fluvial :
      - Très forte utilisation naturelle (> 70 % espaces naturels)
      - Forte utilisation naturelle (> 40 % espaces naturels)
      - Utilisation naturelle modérée (> 10 % espaces naturels)
      - Presque pas/pas naturelle (< 10 % espaces naturels)",
      # gravel bars
      "la présence de bancs sédimentaires. Basé sur le ratio entre la surface des sédiments et la surface du chenal actif, qui se compose des surfaces de sédiments et d'eau :
      - Absent (pas des bancs sédimentaires)
      - occasionnel (bancs sédimentaires < 50 % du chenal actif)
      - fréquent (bancs sédimentaires >= 50 % du chenal actif)",
      # confinement
      "Indique le dégrée du confinement du chenal actif. Basé sur le ratio entre la largeur du chenal actif et la largeur du fond de la vallée.
      - espace abondant (chenal actif > 70 % du fond de la vallée)
      - modérement espace (chenal actif > 40 % du fond de la vallée)
      - confiné (chenal actif > 10 % du fond de la vallée)
      - très confiné (chenal actif < 10 % du fond de la vallée)",
      # habitat connectivity
      "Indique la présence d'un corridor riverain naturel. Basé sur ratio de la surface du corridor connecté (comprenant le chenal actif, le corridor naturel et les corridors semi-naturels) et le fond de la vallée :
      - très bien connecté (>= 70 %)
      - bien connecté (>= 40 %)
      - moyen connecté (>= 10 % )
      - faible / absente (< 10 %)"
    ),
    class_name = c(
      "class_strahler",
      "class_topographie",
      "class_lu_dominante",
      "class_urban",
      "class_agriculture",
      "class_nature",
      "class_gravel",
      "class_confinement",
      "class_habitat"
    ),
    sld_style = c(
      "classes_proposed_strahler",
      "classes_proposed_topographie",
      "classes_proposed_lu_dominante",
      "classes_proposed_urban",
      "classes_proposed_agriculture",
      "classes_proposed_nature",
      "classes_proposed_gravel",
      "classes_proposed_confinement",
      "classes_proposed_habitat"
    )
  )

  return(df)
}


#' get nested list-object with all variables for Metric-selection in selectInput()-Elements
#'
#' @importFrom dplyr filter pull
#'
#' @return list-object with first level the names of metric types and second levels the corresponding metrics for each type
#'
#' @examples
#' params_get_metric_choices()
params_get_metric_choices <- function(){
  # get parameters and create empty list object
  metric_info <- params_metrics()
  input <- list()

  # loop through all types and store metric names
  for (type in unique(metric_info$metric_type_title)) {
    input[type] <- list(
      metric_info |>
        dplyr::filter(metric_type_title == type) |>
        dplyr::pull(metric_name) |>
        setNames(
          metric_info |>
            dplyr::filter(metric_type_title == type) |>
            dplyr::pull(metric_title)
        )
    )
  }
  return(input)
}

#' Get Metric parameters
#'
#' This function returns a nested list of metrics, their names and titles used for plots as well as their description.
#' Can for example be used for the creation of a selectInput of metrics.
#'
#' @importFrom tibble tibble
#'
#' @return A table of all metrics and corresponding info
#'
#' @examples
#' metric_choices <- params_metrics()
#'
#' @export
params_metrics <- function(){

  metric_info <- tibble(
    metric_name = c("talweg_elevation_min", "active_channel_width", "natural_corridor_width", "connected_corridor_width",
                    "valley_bottom_width", "talweg_slope", "floodplain_slope", "water_channel_pc", "gravel_bars_pc",
                    "natural_open_pc", "forest_pc", "grassland_pc", "crops_pc", "diffuse_urban_pc", "dense_urban_pc",
                    "infrastructures_pc", "water_channel", "gravel_bars", "natural_open", "forest", "grassland", "crops",
                    "diffuse_urban", "dense_urban", "infrastructures", "active_channel_pc", "riparian_corridor_pc",
                    "semi_natural_pc", "reversible_pc", "disconnected_pc", "built_environment_pc", "active_channel",
                    "riparian_corridor", "semi_natural", "reversible", "disconnected", "built_environment",
                    "idx_confinement"),
    metric_type_title = c("Elévation (m)", "Largeurs (m)", "Largeurs (m)", "Largeurs (m)", "Largeurs (m)", "Pentes (%)",
                          "Pentes (%)", "Occupation du sol (%)", "Occupation du sol (%)", "Occupation du sol (%)",
                          "Occupation du sol (%)", "Occupation du sol (%)", "Occupation du sol (%)", "Occupation du sol (%)",
                          "Occupation du sol (%)", "Occupation du sol (%)", "Occupation du sol (ha)", "Occupation du sol (ha)",
                          "Occupation du sol (ha)", "Occupation du sol (ha)", "Occupation du sol (ha)", "Occupation du sol (ha)",
                          "Occupation du sol (ha)", "Occupation du sol (ha)", "Occupation du sol (ha)", "Continuité latérale (%)",
                          "Continuité latérale (%)", "Continuité latérale (%)", "Continuité latérale (%)", "Continuité latérale (%)",
                          "Continuité latérale (%)", "Continuité latérale (ha)", "Continuité latérale (ha)", "Continuité latérale (ha)",
                          "Continuité latérale (ha)", "Continuité latérale (ha)", "Continuité latérale (ha)", "Indice"),
    metric_title = c("Elévation (m)", "Chenal actif (m)", "Corridor naturel (m)", "Corridor connecté (m)",
                     "Fond de vallée (m)", "Pente talweg (%)", "Pente fond de vallée (%)", "Surface en eau (%)",
                     "Banc sédimentaire (%)", "Espace naturel ouvert (%)", "Forêt (%)", "Prairie permanente (%)",
                     "Culture (%)", "Périurbain (%)", "Urbain dense (%)", "Infrastructure de transport (%)", "Surface en eau (ha)",
                     "Banc sédimentaire (ha)", "Espace naturel ouvert (ha)", "Forêt (ha)", "Prairie permanente (ha)",
                     "Culture (ha)", "Périurbain (ha)", "Urbain dense (ha)", "Infrastructure de transport (ha)",
                     "Bande active (%)", "Corridor naturel (%)", "Corridor semi-naturel (%)", "Espace de réversibilité (%)",
                     "Espace déconnecté (%)", "Espace artificialisé (%)", "Bande active (ha)", "Corridor naturel (ha)",
                     "Corridor semi-naturel (ha)", "Espace de réversibilité (ha)", "Espace déconnecté (ha)",
                     "Espace artificialisé (ha)", "Indice de confinement"),
    metric_description = c("Elévation minimale du talweg.", "Surface en eau et bancs sédimentaires.",
                           "Surface en eau, bancs sédimentaires et végétation rivulaire connectée.",
                           "Surface en eau, bancs sédimentaires, végétation rivulaire connectée et surfaces agricoles connectées.",
                           "Fond de vallée déterminé par seuil de pente et d'élévation.", "Pente moyenne du talweg.",
                           "Pente moyenne du fond de vallée.", "Surface en eau définie par la BD TOPO® de l'IGN. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Surface des eaux intermittentes de la BD TOPO® de l'IGN. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone de végétation ouverte telles que les forêts ouvertes, les haies ou bandes ligneuses. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone de végétation fermée. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Parcelle de prairie permanente définie dans le RPG®. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone de culture rassemblant les grandes cultures, l'arboriculture et les vignes. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone d'habitation diffus proche de la zone d'habitation de la BD TOPO®. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone continue de l'espace bâti dense ou artificialisée. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Infrastructure routière et ferroviaire. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Surface en eau définie par la BD TOPO® de l'IGN. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Surface des eaux intermittentes de la BD TOPO® de l'IGN. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone de végétation ouverte telles que les forêts ouvertes, les haies ou bandes ligneuses. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone de végétation fermée. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Parcelle de prairie permanente définie dans le RPG®. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone de culture rassemblant les grandes cultures, l'arboriculture et les vignes. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone d'habitation diffus proche de la zone d'habitation de la BD TOPO®. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone continue de l'espace bâti dense ou artificialisée. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Infrastructure routière et ferroviaire. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Les surfaces en eau et les bancs sédimentaires connectés. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Le chenal actif avec la végétation ouverte et fermée connectée. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Le corridor naturel avec les prairies permanentes connectées. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Le corridor semi-naturel avec les cultures connectées. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Espace non urbanisé déconnecté du corridor fluvial par des infrastructures ou du bâti. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone bâti, dense ou peu dense, et les infrastructures de transport. La surface est exprimée en pourcentage du fond de vallée découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Les surfaces en eau et les bancs sédimentaires connectés. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Le chenal actif avec la végétation ouverte et fermée connectée. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Le corridor naturel avec les prairies permanentes connectées. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Le corridor semi-naturel avec les cultures connectées. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Espace non urbanisé déconnecté du corridor fluvial par des infrastructures ou du bâti. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Zone bâti, dense ou peu dense, et les infrastructures de transport. La surface est exprimée en hectares découpée à partir des tronçons de 200m du réseau hydrographique.",
                           "Ratio de la largeur de la bande active sur la largeur du fond de vallée. Il permet d'estimer si le cours d'eau est contraint par la topographie. Plus l'indice est faible plus le cours d'eau a d'espace potentiel pour s'élargir.")
  )
  return(metric_info)
}

#' Get classes names and colors for proposed classifications
#'
#' @return list with classes names and colors, identifiable by each classification-name
#'
#' @examples
#' params_classes_colors()$class_habitat
params_classes_colors <- function() {

  df <- list()
  # STRAHLER
  df$class_strahler <- c("#64b5f6", "#1e88e5", "#1976d2", "#1565c0", "#0d47a1", "#0a2472") %>%
    setNames(c(1,2,3,4,5,6))

  # TOPOGRAPHY
  df$class_topographie <- c( "#bb3e03", "#e9d8a6", "#a3b18a",
                             "#780000","#ee9b00", "#3a5a40") %>%
    setNames(
      c("Plaines de montagne",
        "Plaines de moyenne altitude",
        "Plaines de basse altitude",
        "Pentes de montagne",
        "Pentes de moyenne altitude",
        "Pentes de basse altitude")
    )

  # LU DOMINANT
  df$class_lu_dominante <- c("#2d6a4f", "#99d98c", "#ffdd00", "#ba181b") %>%
    setNames(c("Forêt", "Prairies et sols nus", "Cultures", "Espace construit"))

  # URBAN
  df$class_urban <- c("#6a040f", "#dc2f02", "#ffdd00", "#74c69d") %>%
    setNames(
      c("fortement urbanisé", "urbanisé", "modérément urbanisé", "Presque pas/pas urbanisé")
    )

  # AGRICULTURE
  df$class_agriculture <- c("#6a040f", "#dc2f02", "#ffdd00", "#74c69d") %>%
    setNames(
      c("Forte impact agricole", "Impact agricole élevé",
        "Impact agricole modéré", "Presque pas/pas d'impact agricole")
    )

  # NATURE
  df$class_nature <- c("#081c15", "#2d6a4f", "#74c69d", "#d8f3dc") %>%
    setNames(
      c("Très forte utilisation naturelle", "Forte utilisation naturelle",
        "Utilisation naturelle modérée", "Presque pas/pas naturelle")
    )

  # GRAVEL BARS
  df$class_gravel <- c("#603808", "#e7bc91", "#0077b6") %>%
    setNames(
      c("abundant", "moyennement présente", "absent")
    )

  # CONFINEMENT
  df$class_confinement <- c("#2d6a4f", "#99d98c", "#ffdd00", "#ba181b") %>%
    setNames(
      c("espace abondant", "modérement espace", "confiné", "très confiné")
    )

  # HABITAT CONNECTIVITY
  df$class_habitat <- c("#2d6a4f", "#99d98c", "#ffdd00", "#ba181b") %>%
    setNames(
      c("très bien connecté", "bien connecté", "moyen connecté", "faible / absente")
    )

  return(df)
}
