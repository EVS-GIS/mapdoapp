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
    carhyce_stations="Stations CarHyCE",
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
      "Connectivité des habitats riverains",
      "Style fluvial"
    ),
    description = c(
      # strahler
      "Représente la complexité du réseaux hydrographique. L'ordre de Strahler est de 1 pour tout cours d'eau entre sa source et sa première confluence et mont avec chaque confluence.",
      # topographie
      "Classification simple basée sur la pente et la hauteur du cours de la rivière :
      - Plaines de basse altitude (> 0 m & < 0.5 % pente)
      - Plaines de moyenne altitude (> 300 m & < 0.5 % pente)
      - Plaines de montagne (> 1000 m & < 0.5 % pente)
      - Pentes de basse altitude (> 0 m & > 0.5 % pente)
      - Pentes de moyenne altitude (> 300 m & > 0.5 % pente)
      - Pentes de montagne (> 1000 m & > 0.5 % pente)
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
      - Fortement urbanisé (> 70 % zones construites)
      - Urbanisé  (> 40 % zones construites)
      - Modérément urbanisé (> 10 % zones construites)
      - Presque pas/Pas urbanisé (< 10 % zones construites)",
      # agriculture
      "indique la part de l'utilisation des terres agricoles dans la zone du fond de vallée de chaque segment de cours d'eau
      - Très Forte (> 70 % cultures)
      - Très élevé (> 40 % cultures)
      - Modéré (> 10 % cultures)
      - Basse/Absente (< 10 % cultures)",
      # natural
      "indique la part de l'occupation naturelle des sols dans la zone du fond de vallée de chaque tronçon fluvial :
      - Très forte utilisation naturelle (> 70 % espaces naturels)
      - Forte utilisation naturelle (> 40 % espaces naturels)
      - Utilisation naturelle modérée (> 10 % espaces naturels)
      - Presque pas/Pas naturelle (< 10 % espaces naturels)",
      # gravel bars
      "la présence de bancs sédimentaires. Basé sur le ratio entre la surface des sédiments et la surface du chenal actif, qui se compose des surfaces de sédiments et d'eau :
      - Absent (pas des bancs sédimentaires)
      - Occasionnel (bancs sédimentaires < 50 % du chenal actif)
      - Fréquent (bancs sédimentaires >= 50 % du chenal actif)",
      # confinement
      "Indique le dégrée du confinement du chenal actif. Basé sur le ratio entre la largeur du chenal actif et la largeur du fond de la vallée.
      - Peu confiné (chenal actif > 70 % du fond de la vallée)
      - Modérement confiné (chenal actif > 40 % du fond de la vallée)
      - Confiné (chenal actif > 10 % du fond de la vallée)
      - Très confiné (chenal actif < 10 % du fond de la vallée)",
      # habitat connectivity
      "Indique la présence d'un corridor riverain naturel. Basé sur ratio de la surface du corridor connecté (comprenant le chenal actif, le corridor naturel et les corridors semi-naturels) et le fond de la vallée :
      - très bien connecté (>= 70 %)
      - bien connecté (>= 40 %)
      - moyen connecté (>= 10 % )
      - faible / absente (< 10 %)",
      # style
      "Classification des styles, basée sur la méthod décrite dans De Almeida et al, 2026.
      - anabranche
      - anastomosé
      - bancs alternés
      - divagant
      - méandre passif
      - rectiligne
      - réservoir
      - sinueux
      - sinueux à bancs,
      - tresse
      "
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
      "class_habitat",
      "class_style"
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
      "classes_proposed_habitat",
      "classes_proposed_style"
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
  input <- list()
  data(metric_info)
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
  df$class_topographie <- c( "#bb3e03", "#85ba55", "#2ca555",
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
      c("Fortement urbanisé", "Urbanisé", "Modérément urbanisé", "Presque pas/Pas urbanisé")
    )

  # AGRICULTURE / impact agricole
  df$class_agriculture <- c("#6a040f", "#dc2f02", "#ffdd00", "#74c69d") %>%
    setNames(
      c("Très Forte", "Forte",
        "Modéré", "Basse/Absente")
    )

  # NATURE / utilisation naturelle
  df$class_nature <- c("#081c15", "#2d6a4f", "#74c69d", "#d8f3dc") %>%
    setNames(
      c("Très forte", "Forte",
        "Modérée", "Presque pas/Pas naturelle")
    )

  # GRAVEL BARS
  df$class_gravel <- c("#603808", "#e7bc91", "#0077b6") %>%
    setNames(
      c("Fréquent", "Occasionnel", "Absent")
    )

  # CONFINEMENT
  df$class_confinement <- c("#2d6a4f", "#99d98c", "#ffdd00", "#ba181b") %>%
    setNames(
      c("Peu confiné", "Modérément confiné", "Confiné", "Très confiné")
    )

  # HABITAT CONNECTIVITY
  df$class_habitat <- c("#2d6a4f", "#99d98c", "#ffdd00", "#ba181b") %>%
    setNames(
      c("Élevée", "Bonne", "Moyenne", "Faible/Absente")
    )

  # FLUVIAL STYLES
  df$class_style <- c("#33a02c", "#b2df8a","#b3cde3", "#b012d4", "#363232", "#542788",
                      "#807dba", "#4988C4","#9e9e9e", "#1C4D8D", "#629FAD", "#e69f00") %>%
    setNames(
      c("Anabranche", "Anastomosé", "Bancs alternés", "Divagant","Intermittent","Méandre actif",
        "Méandre passif","Rectiligne","Réservoir","Sinueux","Sinueux à bancs","Tresse")
    )
  return(df)
}

