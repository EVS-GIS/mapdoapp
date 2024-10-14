#' Create Map with basemaps and basic functionalities
#'
#' @description Initializes the map
#'
#' @param params_wms parameters defining properties of wms maps to be added
#' @param id_logo_ign_remonterletemps id of the IGN remonter le temps image.
#'
#' @importFrom leaflet leaflet leafletOptions setView addPolygons addScaleBar addLayersControl addControl hideGroup addCircleMarkers
#' @importFrom leaflet layersControlOptions addProviderTiles scaleBarOptions providers leafletOptions pathOptions highlightOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions addFullscreenControl gpsOptions addControlGPS
#' @importFrom htmltools htmlEscape
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
map_initialize <- function(params_wms, params_map_group,
                           id_logo_ign_remonterletemps,
                           basins_data, regions_data, axes_data,
                           roe_sites, hydro_sites) {

  leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.75)) %>%
    # zoom on France
    setView(lng = 2.468697, lat = 46.603354, zoom = 5.5) %>%
    # scale bar
    addScaleBar(position = "bottomleft",
                scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    # background map
    addProviderTiles(providers$CartoDB.Voyager) %>%
    # search button
    addSearchOSM(options = leaflet.extras::searchOptions(hideMarkerOnCollapse = TRUE)) %>%
    # GPS location button
    addControlGPS(options = leaflet.extras::gpsOptions(
      position = "topleft",
      activate = FALSE,
      autoCenter = FALSE,
      maxZoom = NULL,
      setView = FALSE
    )) %>%
    # fullscreen option
    addFullscreenControl(pseudoFullscreen = TRUE) %>%
    # further basemaps
    map_add_basemaps(params_wms) %>%
    addPolygons(data = basins_data,
                layerId = ~cdbh,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "black",
                opacity = 0.20,
                highlightOptions = highlightOptions(
                  fillColor = "#000000",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbbh),
                options = pathOptions(clickable = ~click),
                group = params_map_group[["bassin"]]
    ) %>%
    # Basin layer hidden by default
    hideGroup(params_map_group[["bassin"]]) %>%
    addPolygons(data = regions_data,
                layerId = ~gid,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "black",
                opacity = 0.20,
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbregionhy),
                options = pathOptions(clickable = ~click),
                group = params_map_group[["region"]]
    ) %>%
    # Basin layer hidden by default
    hideGroup(params_map_group[["region"]]) %>%
    # add ROE overlayers from PostgreSQL
    addCircleMarkers(data = roe_sites,
                     radius = 4.5,
                     weight = 0.5,
                     opacity = 0.9,
                     color = "#D0D0D0",
                     fillColor = "#323232",
                     fillOpacity = 0.9,
                     popup = ~nomprincip,
                     group = params_map_group[["roe"]]
    ) %>%
    # ROE layer hidden by default
    hideGroup(params_map_group[["roe"]]) %>%
    addCircleMarkers(data = hydro_sites,
                     radius = 4.5,
                     weight = 0.5,
                     opacity = 0.9,
                     color = "#E5F6FF",
                     fillColor = "#33B1FF",
                     fillOpacity = 0.9,
                     popup = ~paste0("<a href=\"", url_site, "\",  target = \'_blank\'>", libelle_site, "</a>"),
                     group = params_map_group[["hydro_sites"]]
    ) %>%
    # Hydrometric sites layer hidden by default
    hideGroup(params_map_group[["hydro_sites"]]) %>%
    # add WMS overlayers
    map_add_wms_overlayers(params_wms) %>%
    # add transparent axis
    map_add_axes(data_axis = axes_data, group = params_map_group[["axis"]]) %>%
    # add controller
    addLayersControl(
      baseGroups = c("CartoDB Voyager", unlist(sapply(params_wms, function(x) if (x$basemap) x$name else NULL), use.names = FALSE)),
      options = layersControlOptions(collapsed = TRUE),
      overlayGroups = c(params_map_group[["bassin"]],
                        params_map_group[["region"]],
                        params_map_group[["roe"]],
                        params_map_group[["hydro_sites"]],
                        unlist(sapply(params_wms, function(x) if (x$overlayer) x$name else NULL), use.names = FALSE))
    ) %>%
    addControl(
      className = "img_div_ign_remonterletemps",
      position = "topleft",
      html = tags$a(href = "javascript:void(0);",
                    tags$img(
                      id = id_logo_ign_remonterletemps,
                      src = "www/logo_ign_remonterletemps.jpg",
                      width = 50, height = 50,
                      title="Vers le site IGN remonterletemps"))
    )
}


#' Add Basemap Layers to an Existing Leaflet Map
#'
#' This function adds basemap layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which basemap layers will be added.
#' @param params_wms parameters defining properties of wms maps to be added
#'
#' @importFrom leaflet addWMSTiles WMSTileOptions addTiles tileOptions
#'
#' @return An updated Leaflet map with basemap layers added.
#'
#' @examples
#' \dontrun{
#'   # Used in map_init_bassins() function, its use is in the function
#' }
#'
#' @export
map_add_basemaps <- function(map, params_wms) {
  for (i in params_wms) {
    if (i$basemap == TRUE){
      if ((i$name == "Occupation du sol") || (i$name == "Géologie")){
        map <- map %>%
          addWMSTiles(
            baseUrl = i$url,
            layers = i$layer,
            attribution = i$attribution,
            options = WMSTileOptions(
              format = i$format,
              transparent = TRUE,
              opacity = 0.6,
              styles = i$style,
            ),
            group = i$name
          )
      } else {
        map <- map %>%
          addTiles(
            urlTemplate = i$url,
            options = tileOptions(
              attribution = i$attribution,
              transparent = TRUE,
              opacity = 0.7,
              format = i$format,
              style = i$style
            ),
            group = i$name
          )
      }

    }
  }
  return(map)
}

#' Add Overlayer Layers to an Existing Leaflet Map
#'
#' This function adds overlayer layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which overlayer layers will be added.
#'
#' @return An updated Leaflet map with overlayer layers added.
#'
#' @examples
#' \dontrun{
#'   # Used in map_region_clicked() function, its use is in the function
#' }
#'
#' @importFrom leaflet addWMSTiles hideGroup WMSTileOptions
#'
#' @export
map_add_wms_overlayers <- function(map, params_wms) {
  for (i in params_wms) {
    if (i$overlayer == TRUE){
      map <- map %>%
        addWMSTiles(
          baseUrl = i$url,
          layerId = i$name,
          layers = i$layer,
          attribution = i$attribution,
          options = WMSTileOptions(
            format = i$format,
            transparent = TRUE
          ),
          group = i$name
        ) %>%
        hideGroup(i$name)
    }
  }
  return(map)
}

#' Map the hydrographique network
#'
#' @description
#' Adds the hydrographique network with a custom styling and regional constraint to the map
#'
#'
#' @param map An existing Leaflet map to which WMS tiles will be added.
#' @param wms_params list containing the specific network-WMS parameters.
#' @param cql_filter A CQL filter to apply to the WMS request.
#'
#' @return An updated Leaflet map with WMS tiles of the styled network. Standard styling
#' according to strahler-order and standard regional constraint: whole France.
#'
#' @examples
#' map %>%
#' map_add_network(wms_params = params_wms()$network, cql_filter = "gid_region <> 11", style = "mapdo:classes_proposed_urban")
#'
#' @export
map_add_network <- function(map, wms_params_network,
                            group,
                            cql_filter = "",
                            style = "mapdo:classes_proposed_strahler") {

  map %>%
    clearGroup(group) %>%
    addWMSTiles(
      baseUrl = wms_params_network$url,
      layers = wms_params_network$layer,
      attribution = wms_params_network$attribution,
      options = WMSTileOptions(
        format = wms_params_network$format,
        request = "GetMap",
        transparent = TRUE,
        styles = style,
        cql_filter = cql_filter,
        opacity = 0.7,
        zIndex = 90
      ),
      group = group
    ) %>%
  # Add WMS Legend
  addWMSLegend(uri = map_legend_class(style = style, wms_params_network),
               title = globals$classes_proposed[globals$classes_proposed$sld_style == sub("mapdo:", "", style),]$class_title,
               position = "bottomleft",
               layerId = "legend_metric")

}

#' Add a metric layer with custom symbology to the map.
#'
#' This function adds a metric layer with custom symbology based on user-defined classes to a leaflet map.
#' It allows you to specify custom parameters for the Web Map Service (WMS) request and customize the SLD (Styled Layer Descriptor) body for styling the layer.
#'
#' @param map A leaflet map object to which the metric layer will be added.
#' @param wms_params A list containing WMS parameters for the metric layer. If not provided, default parameters are retrieved using the \code{\link{params_wms}} function.
#' @param breaks A numeric vector containing quantile breaks.
#' @param colors A character vector of colors generated based on quantile breaks.
#' @param metric The name of the selected metric.
#' @param sld_legend A character string representing the SLD (Styled Layer Descriptor) body for creation of the legend.
#' @param group The name of the group to which the metric layer will be added.
#'
#' @return A leaflet map object with the metric layer and its legend added.
#'
#' @importFrom leaflet addWMSTiles clearGroup WMSTileOptions
#' @importFrom leaflet.extras addWMSLegend
#' @importFrom glue glue
#'
#' @export
map_add_network_metric <- function(map,
                                   wms_params = globals$wms_params$metric,
                                   breaks = c(0.00, 531.39),
                                   colors = c("#67A9CF", "#BDD1BA"),
                                   metric = "talweg_elevation_min",
                                   sld_legend = "",
                                   group = "Réseau hydrographique") {



  # Construct the `env` parameter with class names, breaks, colors, and opacities
  env_params <- glue::glue('metric:{metric};')
  for (i in 1:length(breaks)) {
    if(i<length(breaks)) {
    vars_i <- glue::glue('classname_{i}:{breaks[i]} - {breaks[i+1]};color_{i}:{colors[i]};break_{i}:{breaks[i]};opacity_{i}:1;')
    }
    # last class with different name
    else {
      vars_i <- glue::glue('classname_{i}: >= {breaks[i]};color_{i}:{colors[i]};break_{i}:{breaks[i]};opacity_{i}:1')
    }
    env_params <- paste0(env_params,vars_i)
  }

  map %>%
    clearGroup(group) %>%

    # Add WMS tiles, referencing the style template and using the dynamic `env` parameter
    addWMSTiles(
      baseUrl = wms_params$url,
      layers = wms_params$layer,
      attribution = wms_params$attribution,
      options = WMSTileOptions(
        format = wms_params$format,
        request = "GetMap",
        transparent = TRUE,
        styles = "mapdo:classes_manual",    # Reference your GeoServer style template here
        env = env_params,       # Pass env parameters here
        zIndex = 90
      ),
      group = group
    ) %>%

    # Add WMS Legend
    addWMSLegend(uri = map_legend_metric(sld_body = sld_legend, wms_params),
                 title = "Classes",
                 position = "bottomleft",
                 layerId = "legend_metric")
}


#' Generate and display a legend for a map layer using a pre-defined style
#'
#' This function constructs a legend for a map layer by sending a GetLegendGraphic request to a WMS (Web Map Service) server.
#'
#' @param style A character string containing the name of the predefined style that defines the map layer's styling.
#'
#' @return An HTML img tag representing the legend for the specified map layer.
#'
#' @importFrom httr modify_url
#' @importFrom htmltools tags
#'
#' @examples
#' con <- db_con()
#' # Define an SLD body for a map layer
#' sld_body <- sld_get_style(breaks = sld_get_quantile_metric(
#'                                      selected_region_id = 11,
#'                                      selected_metric = "active_channel_width",
#'                                      con = con),
#'                           colors = sld_get_quantile_colors(
#'                                      quantile_breaks = sld_get_quantile_metric(
#'                                                          selected_region_id = 11,
#'                                                          selected_metric = "active_channel_width",
#'                                                          con = con)),
#'                           metric = "active_channel_width")
#' DBI::dbDisconnect(con)
#'
#' # Generate and display the legend for the map layer
#' legend <- map_legend_metric(sld_body)
#'
#' @export
map_legend_class <- function(style, wms_params){

  # Construct the query parameters for legend
  query_params <- list(
    REQUEST = "GetLegendGraphic",
    VERSION = wms_params$version,
    FORMAT = wms_params$format,
    STYLE = style,
    LAYER = wms_params$layer,
    LEGEND_OPTIONS = "dpi:80"
  )

  # Build the legend URL
  legend_url <- modify_url(wms_params$url, query = query_params)

  return(legend_url)
}


#' Generate and display a legend for a map layer using a provided SLD (Styled Layer Descriptor) body.
#'
#' This function constructs a legend for a map layer by sending a GetLegendGraphic request to a WMS (Web Map Service) server.
#'
#' @param sld_body A character string containing the SLD (Styled Layer Descriptor) body that defines the map layer's styling.
#' @param wms_params list containing the specific network-WMS parameters.
#'
#' @return An HTML img tag representing the legend for the specified map layer.
#'
#' @importFrom httr modify_url
#'
#' @export
map_legend_metric <- function(sld_body, wms_params){

  # Construct the query parameters for legend
  query_params <- list(
    REQUEST = "GetLegendGraphic",
    VERSION = wms_params$version,
    FORMAT = wms_params$format,
    SLD_BODY = sld_body,
    LAYER = wms_params$layer,
    LEGEND_OPTIONS = "dpi:80"
  )

  # Build the legend URL
  legend_url <- modify_url(wms_params$url, query = query_params)

  return(legend_url)
}

#' Generate and display a legend for a WMS (Web Map Service) layer overlay using specified WMS parameters.
#'
#' This function constructs a legend for a WMS layer overlay by sending a GetLegendGraphic request to a WMS server.
#'
#' @param wms_params A list containing the following parameters for the WMS layer overlay:
#'   \itemize{
#'     \item \code{language} (character): The language to use for the legend.
#'     \item \code{version} (character): The version of the WMS service.
#'     \item \code{service} (character): The WMS service type (e.g., "WMS").
#'     \item \code{sld_version} (character): The SLD (Styled Layer Descriptor) version.
#'     \item \code{layer} (character): The name of the WMS layer for which the legend is generated.
#'     \item \code{format} (character): The desired format of the legend image (e.g., "image/png").
#'     \item \code{style} (character): The style to use for rendering the legend.
#'     \item \code{url} (character): The URL of the WMS server.
#'   }
#'
#' @return An HTML div element containing an img tag representing the legend for the specified WMS layer overlay.
#'
#' @importFrom httr modify_url
#' @importFrom htmltools div
#' @importFrom htmltools img
#'
#' @examples
#' # Define WMS parameters for a layer overlay like
#' wms_params <- params_wms()$inondation
#'
#' # Generate and display the legend for the WMS layer overlay
#' legend <- map_legend_wms_overlayer(wms_params)
#' print(legend)
#'
#' @export
map_legend_wms_overlayer <- function(wms_params){

  # Construct the query parameters for legend
  query_params <- list(
    LANGUAGE = wms_params$language,
    VERSION = wms_params$version,
    SERVICE = wms_params$service,
    REQUEST = "GetLegendGraphic",
    SLD_VERSION = wms_params$sld_version,
    LAYER = wms_params$layer,
    FORMAT = wms_params$format,
    STYLE = wms_params$style,
    LEGEND_OPTIONS = "dpi:80"
  )

  # Build the legend URL
  legend_url <- modify_url(wms_params$url, query = query_params)

  return(legend_url)
}

#' Add Axis Data to an Existing Leaflet Map
#'
#' This function adds axis data as simplified polylines to an existing Leaflet map. The axis are transparent,
#' but when hovering over they appear red.
#'
#' @param map An existing Leaflet map to which axis data will be added.
#' @param data_axis A sf data frame containing axis data.
#' @param selected_axis id of selected axis, so that it will not be included in mapping
#'
#' @importFrom leaflet addPolylines clearGroup pathOptions highlightOptions
#' @importFrom sf st_is_empty
#' @import dplyr
#'
#' @return An updated Leaflet map with axis data added.
#' @export
map_add_axes <- function(map, data_axis, group, selected_axis_id = NULL) {

  # check if axis is selected - if yes, filter out axis
  if (!is.null(selected_axis_id)) {

    # filter out selected axis
    data_axis <- data_axis %>%
      filter(axis != selected_axis_id) %>%
      filter(!st_is_empty(geom))

  }

  # add to map
  map %>%
    clearGroup(group) %>% # clear existing axis layer
    addPolylines(data = data_axis,
                 layerId = ~axis,
                 weight = 5,
                 color = "#ffffff00",
                 opacity = 1,
                 label = ~toponyme,
                 highlightOptions = highlightOptions(
                   color = "red",
                   bringToFront = TRUE
                 ),
                 group = group
    )
}

#' Add Axis Data to an Existing Leaflet Map
#'
#' This function adds axis data as polylines to an existing Leaflet map. The axis are transparent,
#' but when hovering over they appear red.
#'
#' @param map An existing Leaflet map to which axis data will be added.
#' @param data_axis A sf data frame containing axis data.
#'
#' @importFrom leaflet addPolylines
#'
#' @return An updated Leaflet map with axis data added.
#' @export
map_add_axis_dgos <- function(map, axis_data, group) {

  # create HTML conditional tooltip labels
  tooltip_label <- lapply(paste0('<span style="color:#212529;"> <b>', axis_data$toponyme, '</b> </span> <br/>',
                                 '<span style="color:#495057;"> <b>', round(axis_data$measure/1000, 2), ' km depuis l\'exutoire', '</b> </span>'),
                          htmltools::HTML)

  map %>%
    clearGroup(group) %>%
    addPolylines(
      data = axis_data,
      layerId = ~fid,
      weight = 5,
      color = "#ffffff00",
      label = tooltip_label,
      opacity = 1,
      highlightOptions = highlightOptions(
        opacity = 1,
        color = "red"
      ),
      options = pathOptions(zIndex = 100),
      group = group
    )

}

#' Add start and end markers to a leaflet map
#'
#' This function adds start and end markers to a Leaflet map based on the provided
#' start and end coordinates.
#'
#' @param map A Leaflet map object created using the 'leaflet' package.
#' @param axis_start_end A data frame containing start and end coordinates with
#'        columns 'X' for longitude and 'Y' for latitude.
#'
#' @return A Leaflet map object with start and end markers added.
#'
#' @importFrom leaflet addMarkers clearGroup makeIcon pathOptions
#' @importFrom dplyr filter
#'
#' @examples
#' library(leaflet)
#' library(dplyr)
#'
#' # Create a simple Leaflet map
#' my_map <- leaflet() %>%
#'   setView(lng = 4.968697, lat = 45.103354, zoom = 8) %>%
#'   addProviderTiles(providers$CartoDB.Positron)
#'
#' # Create a data frame with start and end coordinates
#' coordinates_df <- data_get_axis_start_end(network_axis %>%
#'                                             filter(fid == 5))
#'
#' # Add start and end markers to the map
#' my_map <- map_add_axis_start_end(my_map, axis_start_end = coordinates_df)
#' my_map
#'
#' @export
map_add_axis_start_end <- function(map, axis_start_end, group) {

  # Define the start and end icon
  start_end_icon <- makeIcon(
    iconUrl = system.file("pin-sharp.png", package = "mapdoapp"),
    iconWidth = 24,
    iconHeight = 24,
    iconAnchorX = 16,
    iconAnchorY = 24
  )

  # add to map
  map %>%
    clearGroup(group) %>%
    addMarkers(
      lng = axis_start_end$X,
      lat = axis_start_end$Y,
      options = pathOptions(interactive = FALSE),
      icon = start_end_icon,
      group = group
    )
}

#' Highlight DGO clicked for cross section.
#'
#' @param map A Leaflet map object.
#' @param selected_dgo sf dgo clicked.
#'
#' @importFrom leaflet clearGroup addPolylines pathOptions
#'
#' @return Leaflet map
#' @export
map_dgo_cross_section <- function(map, selected_dgo, group){
  map %>%
    clearGroup(group) %>%
    addPolylines(
      data = selected_dgo,
      layerId = ~fid,
      weight = 8,
      color = "purple",
      opacity = 1,
      group = group,
      options = pathOptions(zIndex = 90)
    )
  return(map)
}
