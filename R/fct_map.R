#' Initialize a Leaflet Map with hydrological Bassins
#'
#' This function initializes a Leaflet map with bassins data and various map layers.
#'
#' @param bassins_data A hydrological bassins sf data frame.
#' @param id_logo_ign_remonterletemps id of the IGN remonter le temps image.
#'
#' @return A Leaflet map object with basemaps, scale and layer control.
#'
#' @examples
#' map <- map_init_bassins(bassins_data = bassin_hydrographique,
#'                         id_logo_ign_remonterletemps = "logo_ign_remonterletemps")
#' map
#'
#' @importFrom leaflet leaflet setView addPolygons addScaleBar addLayersControl addControl
#' @importFrom leaflet layersControlOptions addProviderTiles scaleBarOptions providers leafletOptions
#' @importFrom leaflet.extras addSearchOSM searchOptions addFullscreenControl
#' @importFrom htmltools htmlEscape
#' @importFrom shiny tags
#'
#' @export
map_init_bassins <- function(bassins_data, id_logo_ign_remonterletemps) {

  # Build the BRGM legend URL
  # legend_url <- modify_url("http://mapsref.brgm.fr/legendes/geoservices/Geologie1000_legende.jpg")

  leaflet() %>%
    setView(lng = 2.468697, lat = 46.603354, zoom = 5) %>%
    addPolygons(data = bassins_data,
                layerId = ~cdbh,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "blue",
                opacity = 0.20,
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbbh),
                options = pathOptions(clickable = ~click),
                group = params_map_group()[["bassin"]]
    ) %>%
    addScaleBar(position = "bottomleft",
                scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addSearchOSM(options = leaflet.extras::searchOptions(hideMarkerOnCollapse = TRUE)) %>%
    addFullscreenControl(pseudoFullscreen = TRUE) %>%
    map_add_basemaps() %>%
    addLayersControl(
      baseGroups = c("CartoDB Positron", unlist(sapply(params_wms(), function(x) if (x$basemap) x$name else NULL), use.names = FALSE)),
      options = layersControlOptions(collapsed = TRUE)
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

#' Add hydrological Regions in a Bassin to an existing Leaflet Map
#'
#' This function adds regions within a bassin to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which regions will be added.
#' @param bassin_click A vector containing information about the clicked bassin.
#' @param regions_data A sf data.frame containing information about regions within the bassin.
#' @param bassins_data A sf data.frame with bassins data.
#'
#' @return An updated Leaflet map with regions added.
#'
#' @importFrom leaflet setView
#' @importFrom leaflet clearGroup
#' @importFrom leaflet addPolygons
#' @importFrom htmltools htmlEscape
#'
#' @examples
#' library(leaflet)
#' library(dplyr)
#' library(sf)
#' # Create init bassin map
#' my_map <- map_init_bassins(bassins_data = bassin_hydrographique,
#'                            id_logo_ign_remonterletemps = "logo_ign_remonterletemps")
#'
#' # simulate bassin selected
#' selected_bassin <- bassin_hydrographique
#'
#' # get centroid coordinate (in shiny see leaflet mapid_shape_click)
#' centre <- sf::st_centroid(selected_bassin)
#' centre_coord <- as.data.frame(st_coordinates(centre)) %>%
#'   rename("lng" = X,
#'          "lat" = Y)
#' # map region
#' map <- map_add_regions_in_bassin(map = my_map,
#'                                  bassins_data = bassin_hydrographique,
#'                                  bassin_click = centre_coord,
#'                                  regions_data = region_hydrographique)
#' map
#'
#' @export
map_add_regions_in_bassin <- function(map, bassins_data,
                                      bassin_click = bassin_click,
                                      regions_data = region_hydro) {
  map %>%
    setView(lng = bassin_click$lng , lat = bassin_click$lat, zoom = 6.5) %>%
    clearGroup(params_map_group()[["bassin"]]) %>%
    addPolygons(data = bassins_data,
                layerId = ~cdbh,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "blue",
                opacity = 0.20,
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbbh),
                options = pathOptions(clickable = ~click),
                group = params_map_group()[["bassin"]]
    ) %>%
    addPolygons(data = regions_data,
                layerId = ~gid,
                smoothFactor = 2,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbregionhy),
                options = pathOptions(clickable = ~click),
                group = params_map_group()[["region"]]
    )
}


#' Update Leaflet Map for a Clicked Region
#'
#' This function updates an existing Leaflet map when a region is clicked, displaying the region and overlayers.
#'
#' @param map An existing Leaflet map to be updated.
#' @param region_click A vector containing information about the clicked region.
#' @param selected_region_feature A sf data frame containing information about the selected region feature.
#' @param regions_data A sf data.frame with the hydrographic regions of the bassin selected.
#' @param roe_region sf data.frame ROE in selected region.
#' @param hydro_sites_region sf data.frame hydrometric sites in selected region.
#'
#' @return An updated Leaflet map with relevant layers and information displayed.
#'
#' @importFrom leaflet setView layersControlOptions addPolygons addCircleMarkers addLayersControl hideGroup
#' @importFrom htmltools tags
#'
#' @examples
#' library(leaflet)
#' library(dplyr)
#' library(sf)
#' # Create init bassin map
#' map_bassin <- map_init_bassins(bassins_data = bassin_hydrographique,
#'                                id_logo_ign_remonterletemps = "logo_ign_remonterletemps")
#'
#' # simulate bassin selected
#' selected_bassin <- bassin_hydrographique
#'
#' # get centroid coordinate (in shiny see leaflet mapid_shape_click)
#' centre <- sf::st_centroid(selected_bassin)
#' centre_coord <- as.data.frame(st_coordinates(centre)) %>%
#'   rename("lng" = X,
#'          "lat" = Y)
#'
#' # map region
#' map_region <- map_add_regions_in_bassin(map = map_bassin,
#'                                         bassins_data = bassin_hydrographique,
#'                                         bassin_click = centre_coord,
#'                                         regions_data = region_hydrographique)
#' # simulate selected region
#' selected_region <- region_hydrographique
#'
#' # get centroid coordinate (in shiny see leaflet mapid_shape_click)
#' centre_region <- sf::st_centroid(selected_bassin)
#' centre_region_coord <- as.data.frame(st_coordinates(centre_region)) %>%
#'   rename("lng" = X,
#'          "lat" = Y)
#' centre_region_coord$id <- 11
#'
#'con <- db_con()
#' # get ROE in region
#' roe_region <- data_get_roe_in_region(centre_region_coord$id, con = con)
#' # get hydro sites in region
#' hydro_sites_region <- data_get_hydro_sites(centre_region_coord$id, con = con)
#' DBI::dbDisconnect(con)
#'
#' # map the element in the region clicked
#' map <- map_region_clicked(map = map_region,
#'                           region_click = centre_region_coord,
#'                           selected_region_feature = selected_region,
#'                           regions_data = region_hydrographique,
#'                           roe_region = roe_region,
#'                           hydro_sites_region = hydro_sites_region)
#' map
#'
#' @export
map_region_clicked <- function(map,
                               region_click,
                               selected_region_feature,
                               regions_data,
                               roe_region,
                               hydro_sites_region) {
  map %>%
    setView(lng = region_click$lng , lat = region_click$lat, zoom = 7.5) %>%
    clearGroup(c(params_map_group()[["region"]],
                 params_map_group()[["roe"]],
                 params_map_group()[["hydro_sites"]],
                 params_map_group()[["dgo_axis"]],
                 params_map_group()[["dgo"]],
                 params_map_group()[["axis_start_end"]],
                 unlist(sapply(params_wms(), function(x) if (x$overlayer) x$name else NULL), use.names = FALSE))) %>%
    # restyle the regions
    addPolygons(data = regions_data,
                layerId = ~gid,
                fillColor = "black",
                fillOpacity = ~opacity,
                weight = 2,
                color = "black",
                highlightOptions = highlightOptions(
                  fillColor = "#a8d1ff",
                  fillOpacity = 0.5),
                label = ~htmlEscape(lbregionhy),
                options = pathOptions(clickable = ~click),
                group = params_map_group()[["region"]]
    ) %>%
    # add ROE overlayers from PostgreSQL
    addCircleMarkers(data = roe_region,
                     radius = 4.5,
                     weight = 0.5,
                     opacity = 0.9,
                     color = "#D0D0D0",
                     fillColor = "#323232",
                     fillOpacity = 0.9,
                     popup = ~nomprincip,
                     group = params_map_group()[["roe"]]
    ) %>%
    # hydrometric sites layer hidden by default
    hideGroup(params_map_group()[["roe"]]) %>%
    addCircleMarkers(data = hydro_sites_region,
                     radius = 4.5,
                     weight = 0.5,
                     opacity = 0.9,
                     color = "#E5F6FF",
                     fillColor = "#33B1FF",
                     fillOpacity = 0.9,
                     popup = ~paste0("<a href=\"", url_site, "\",  target = \'_blank\'>", libelle_site, "</a>"),
                     group = params_map_group()[["hydro_sites"]]
    ) %>%
    # Hydrometric sites layer hidden by default
    hideGroup(params_map_group()[["hydro_sites"]]) %>%
    # add WMS overlayers
    map_add_wms_overlayers() %>%
    addLayersControl(
      baseGroups = c("CartoDB Positron", unlist(sapply(params_wms(), function(x) if (x$basemap) x$name else NULL), use.names = FALSE)),
      options = layersControlOptions(collapsed = TRUE),
      overlayGroups = c(params_map_group()[["roe"]],
                        params_map_group()[["hydro_sites"]],
                        unlist(sapply(params_wms(), function(x) if (x$overlayer) x$name else NULL), use.names = FALSE))
    )
}

#' Map WMS metric
#'
#' This function adds WMS tiles with metric data to an existing Leaflet map, allowing for customization of style and filtering.
#'
#' @param map An existing Leaflet map to which WMS tiles will be added.
#' @param wms_params A list of WMS parameters.
#' @param cql_filter A CQL filter to apply to the WMS request.
#' @param sld_body A custom SLD (Styled Layer Descriptor) body for symbology customization.
#'
#' @return An updated Leaflet map with WMS tiles containing metric data added.
#'
#' @importFrom leaflet addWMSTiles WMSTileOptions
#'
#' @examples
#' \dontrun{
#'   # Used in map_metric() function, see full example in map_metric() documentation
#' }
#'
#' @export
map_wms_metric <-function(map, wms_params = params_wms()$metric,
                          cql_filter = "", sld_body = "") {
  map %>%
    addWMSTiles(
      baseUrl = wms_params$url,
      layers = wms_params$layer,
      attribution = wms_params$attribution,
      options = WMSTileOptions(
        format = wms_params$format,
        request = "GetMap",
        transparent = TRUE,
        styles = wms_params$style,
        cql_filter = cql_filter,
        sld_body = sld_body,
        zIndex = 90
      ),
      group = params_map_group()[["metric"]]
    )
}

#' Map WMS class
#'
#' This function adds WMS tiles with fluvial style to an existing Leaflet map, allowing for customization of style and filtering.
#'
#' @param map An existing Leaflet map to which WMS tiles will be added.
#' @param wms_params A list of WMS parameters.
#' @param cql_filter A CQL filter to apply to the WMS request.
#' @param sld_body A custom SLD (Styled Layer Descriptor) body for symbology customization.
#'
#' @return An updated Leaflet map with WMS tiles containing metric data added.
#'
#' @importFrom leaflet addWMSTiles WMSTileOptions
#'
#' @examples
#' \dontrun{
#'   # Used in map_metric() function, see full example in map_metric() documentation
#' }
#'
#' @export
map_wms_class <- function(map, wms_params = params_wms()$class,
                          cql_filter = "", sld_body = "") {
  map %>%
    addWMSTiles(
      baseUrl = wms_params$url,
      layers = wms_params$layer,
      attribution = wms_params$attribution,
      options = WMSTileOptions(
        format = wms_params$format,
        request = "GetMap",
        transparent = TRUE,
        styles = wms_params$style,
        cql_filter = cql_filter,
        sld_body = sld_body,
        zIndex = 90
      ),
      group = params_map_group()[["class"]]
    )
}

#' Add Axis Data to an Existing Leaflet Map
#'
#' This function adds axis data as polylines to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which axis data will be added.
#' @param data_axis A sf data frame containing axis data.
#'
#' @return An updated Leaflet map with axis data added.
#'
#' @examples
#' \dontrun{
#'   # Used in map_metric() function, see full example in map_metric() documentation
#' }
#'
#' @importFrom leaflet addPolylines
#'
#' @export
map_axis <- function(map, data_axis) {
  map %>%
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
                 group = params_map_group()[["axis"]]
    )
}


#' Add a metric layer with custom symbology to a map.
#'
#' This function adds a metric layer with custom symbology to a leaflet map. It allows you to specify custom parameters for the Web Map Service (WMS) request, apply a CQL (Common Query Language) filter, and provide a custom SLD (Styled Layer Descriptor) body for styling the layer. Additionally, you can specify the data axis to display on the map.
#'
#' @param map A leaflet map object to which the metric layer will be added.
#' @param wms_params A list containing WMS parameters for the metric layer. If not provided, default parameters are retrieved using the \code{\link{params_wms}} function.
#' @param cql_filter A character string representing a CQL filter to apply to the metric layer.
#' @param sld_body A character string representing the SLD (Styled Layer Descriptor) body for custom styling of the metric layer.
#' @param data_axis A data axis to display on the map.
#'
#' @return A leaflet map object with the metric layer added.
#'
#' @examples
#' library(leaflet)
#' library(dplyr)
#' library(sf)
#' # Create init bassin map
#' map_bassin <- map_init_bassins(bassins_data = bassin_hydrographique,
#'                                id_logo_ign_remonterletemps = "logo_ign_remonterletemps")
#'
#' # simulate bassin selected
#' selected_bassin <- bassin_hydrographique
#'
#' # get centroid coordinate (in shiny see leaflet mapid_shape_click)
#' centre <- sf::st_centroid(selected_bassin)
#' centre_coord <- as.data.frame(st_coordinates(centre)) %>%
#'   rename("lng" = X,
#'          "lat" = Y)
#'
#' # map region
#' map_region <- map_add_regions_in_bassin(map = map_bassin,
#'                                         bassins_data = bassin_hydrographique,
#'                                         bassin_click = centre_coord,
#'                                         regions_data = region_hydrographique)
#'
#' # simulate selected region
#' selected_region <- region_hydrographique
#'
#' # get centroid coordinate (in shiny see leaflet mapid_shape_click)
#' centre_region <- sf::st_centroid(selected_bassin)
#' centre_region_coord <- as.data.frame(st_coordinates(centre_region)) %>%
#'   rename("lng" = X,
#'          "lat" = Y)
#' centre_region_coord$id <- 11
#'
#' con = db_con()
#' # get ROE in region
#' roe_region <- data_get_roe_in_region(centre_region_coord$id, con = con)
#' # get hydro sites in region
#' hydro_sites_region <- data_get_hydro_sites(centre_region_coord$id, con = con)
#'
#'
#' # map the element in the region clicked
#' map <- map_region_clicked(map = map_region,
#'                           region_click = centre_region_coord,
#'                           selected_region_feature = selected_region,
#'                           regions_data = region_hydrographique,
#'                           roe_region = roe_region,
#'                           hydro_sites_region = hydro_sites_region)
#' map
#'
#' # build geoserver WMS filter
#' cql_filter=paste0("gid_region=", selected_region[["gid"]])
#'
#' # build geoserver SLD symbology
#' sld_body <- sld_get_style(breaks = sld_get_quantile_metric(
#'                                     selected_region_id = selected_region[["gid"]],
#'                                     selected_metric = "active_channel_width",
#'                                     con = con),
#'                           colors = sld_get_quantile_colors(
#'                                     quantile_breaks = sld_get_quantile_metric(
#'                                        selected_region_id = selected_region[["gid"]],
#'                                        selected_metric = "active_channel_width",
#'                                        con = con)),
#'                           metric = "active_channel_width")
#'DBI::dbDisconnect(con)
#'
#' # Network axis by region
#' network_region_axis <- network_axis %>%
#'   filter(gid_region == selected_region[["gid"]])
#'
#' # Add metric with quantile symbology
#' # wms_params = params_wms()$metric_basic with sld_body = NULL for default blue style$
#' map_metric <- map_metric(map = map,
#'                          wms_params = params_wms()$metric,
#'                          cql_filter = cql_filter,
#'                          sld_body = sld_body,
#'                          data_axis = network_region_axis)
#' map_metric
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet setView
#' @importFrom leaflet clearGroup
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_metric <- function(map, wms_params = params_wms()$metric,
                       cql_filter = "", sld_body = "", data_axis) {
  map %>%
    clearGroup(params_map_group()[["axis"]]) %>%
    clearGroup(params_map_group()[["metric"]]) %>%
    clearGroup(params_map_group()[["class"]]) %>%
    # add metric with custom symbology
    map_wms_metric(wms_params = wms_params,
                   cql_filter = cql_filter, sld_body = sld_body) %>%
    # add transparent axis
    map_axis(data_axis = data_axis)
}

#' Add a metric layer with custom symbology to a map.
#'
#' This function adds a metric layer with custom symbology to a leaflet map. It allows you to specify custom parameters for the Web Map Service (WMS) request, apply a CQL (Common Query Language) filter, and provide a custom SLD (Styled Layer Descriptor) body for styling the layer. Additionally, you can specify the data axis to display on the map.
#'
#' @param map A leaflet map object to which the metric layer will be added.
#' @param wms_params A list containing WMS parameters for the metric layer. If not provided, default parameters are retrieved using the \code{\link{params_wms}} function.
#' @param cql_filter A character string representing a CQL filter to apply to the metric layer.
#' @param sld_body A character string representing the SLD (Styled Layer Descriptor) body for custom styling of the metric layer.
#' @param data_axis A data axis to display on the map.
#'
#' @return A leaflet map object with the metric layer added.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet setView
#' @importFrom leaflet clearGroup
#' @importFrom leaflet addWMSTiles
#'
#' @export
map_class <- function(map, wms_params = params_wms()$class,
                       cql_filter = "", sld_body = "", data_axis) {
  map %>%
    clearGroup(params_map_group()[["axis"]]) %>%
    clearGroup(params_map_group()[["metric"]]) %>%
    clearGroup(params_map_group()[["class"]]) %>%
    # add metric with custom symbology
    map_wms_class(wms_params = wms_params,
                   cql_filter = cql_filter, sld_body = sld_body) %>%
    # add transparent axis
    map_axis(data_axis = data_axis)
}

#' Add DGO axis to a Leaflet map
#'
#' This function adds DGO axis to a Leaflet map with the option to highlight selected axes.
#'
#' @param map A Leaflet map object.
#' @param selected_axis A data frame containing selected axe to be displayed.
#' @param region_axis A data frame containing region-specific axes to be displayed.
#' @param main_metric text with the main selected metric name.
#' @param second_metric text with the second axis selected metric name.
#'
#' @return A modified Leaflet map object with DGO axes added.
#'
#' @importFrom leaflet clearGroup addPolylines highlightOptions pathOptions
#' @importFrom htmltools HTML
#'
#' @examples
#' # Create a basic Leaflet map
#' library(leaflet)
#' library(dplyr)
#'
#' my_map <- leaflet() %>%
#'   setView(lng = 4.968697, lat = 45.103354, zoom = 8) %>%
#'   addProviderTiles(providers$CartoDB.Positron)
#'
#' # Define selected and region-specific axes data frames
#' selected_axes <- network_axis %>% filter(axis == 5)
#' region_axes <- network_axis
#'
#' # Add DGO axes to the map
#' my_map <- map_dgo_axis(my_map, selected_axes, region_axes,
#'                         main_metric = "active_channel_width", second_metric = "talweg_slope")
#' my_map
#'
#' @export
map_dgo_axis <- function(map, selected_axis, region_axis, main_metric, second_metric) {

  # create HTML conditional tooltip labels
  tooltip_label <- lapply(paste0('<span style="color:#212529;"> <b>', selected_axis$toponyme, '</b> </span> <br/>',
                                   '<span style="color:#495057;"> <b>', round(selected_axis$measure, 2), ' km depuis l\'exutoire', '</b> </span>'),
                            htmltools::HTML)

  map %>%
    clearGroup(params_map_group()$dgo_axis) %>%
    clearGroup(params_map_group()$axis) %>%
    clearGroup(params_map_group()$dgo) %>%
    map_axis(data_axis = region_axis) %>%
    addPolylines(
      data = selected_axis,
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
      group = params_map_group()$dgo_axis
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
map_dgo_cross_section <- function(map, selected_dgo){
  map %>%
    clearGroup(params_map_group()$dgo) %>%
    addPolylines(
      data = selected_dgo,
      layerId = ~fid,
      weight = 8,
      color = "purple",
      opacity = 1,
      group = params_map_group()$dgo,
      options = pathOptions(zIndex = 90)
    )
  return(map)
}


#' Add start and end markers to a leaflet map
#'
#' This function adds start and end markers to a Leaflet map based on the provided
#' start and end coordinates.
#'
#' @param map A Leaflet map object created using the 'leaflet' package.
#' @param axis_start_end A data frame containing start and end coordinates with
#'        columns 'X' for longitude and 'Y' for latitude.
#' @param region_axis A data frame containing region-specific axes to be displayed.
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
#' my_map <- map_axis_start_end(my_map, axis_start_end = coordinates_df,
#'                              region_axis = network_axis)
#' my_map
#'
#' @export
map_axis_start_end <- function(map, axis_start_end, region_axis) {

  # Define the start and end icon
  start_end_icon <- makeIcon(
    iconUrl = system.file("pin-sharp.png", package = "mapdoapp"),
    iconWidth = 24,
    iconHeight = 24,
    iconAnchorX = 16,
    iconAnchorY = 24
  )

  # Clear the previous group of markers and add new markers to the map
  map %>%
    clearGroup(params_map_group()$axis_start_end) %>%
    clearGroup(params_map_group()$axis_opacity) %>%
    addMarkers(
      lng = axis_start_end$X,
      lat = axis_start_end$Y,
      options = pathOptions(interactive = FALSE),
      icon = start_end_icon,
      group = params_map_group()$axis_start_end
    ) %>%
    addPolylines(data = region_axis,
                 layerId = ~axis,
                 weight = 5,
                 color = "white",
                 opacity = 0.4,
                 options = pathOptions(interactive = FALSE,
                                       zIndex = 100),
                 group = params_map_group()$axis_opacity
    )
}


#' Add Basemap Layers to an Existing Leaflet Map
#'
#' This function adds basemap layers to an existing Leaflet map.
#'
#' @param map An existing Leaflet map to which basemap layers will be added.
#'
#' @return An updated Leaflet map with basemap layers added.
#'
#' @examples
#' \dontrun{
#'   # Used in map_init_bassins() function, its use is in the function
#' }
#'
#' @importFrom leaflet addWMSTiles addTiles tileOptions
#'
#' @export
map_add_basemaps <- function(map) {
  for (i in params_wms()) {
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
              opacity = 0.7,
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
#' @importFrom leaflet addWMSTiles hideGroup
#'
#' @export
map_add_wms_overlayers <- function(map) {
  for (i in params_wms()) {
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


#' Generate and display a legend for a map layer using a provided SLD (Styled Layer Descriptor) body.
#'
#' This function constructs a legend for a map layer by sending a GetLegendGraphic request to a WMS (Web Map Service) server.
#'
#' @param sld_body A character string containing the SLD (Styled Layer Descriptor) body that defines the map layer's styling.
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
map_legend_metric <- function(sld_body){

  # Construct the query parameters for legend
  query_params <- list(
    REQUEST = "GetLegendGraphic",
    VERSION = params_wms()$metric$version,
    FORMAT = params_wms()$metric$format,
    SLD_BODY = sld_body,
    LAYER = params_wms()$metric$layer
  )

  # Build the legend URL
  legend_url <- modify_url(params_wms()$metric$url, query = query_params)

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
    STYLE = wms_params$style
  )

  # Build the legend URL
  legend_url <- modify_url(wms_params$url, query = query_params)

  return(legend_url)
}


#' Generate a legend entry for a vector overlay layer.
#'
#' This function generates an HTML representation of a legend entry for a vector overlay layer. The legend entry consists of a colored circle with a label indicating the layer's name.
#'
#' @param layer_label A character string representing the label or name of the vector overlay layer.
#' @param color text the legend marker color.
#'
#' @return An HTML div element representing the legend entry for the vector overlay layer.
#'
#' @examples
#' # Create a legend entry for a vector overlay layer
#' legend_entry <- map_legend_vector_overlayer(layer_label = "ROE", color = "blue")
#' print(legend_entry)
#'
#' @importFrom htmltools div span
#' @importFrom glue glue
#'
#' @export
map_legend_vector_overlayer <- function(layer_label, color){

  div(
    style = "display: flex; align-items: center;",
    div(
      style = glue::glue("background-color: {color};
                         border-radius: 50%; width: 10px;
                         height: 10px; margin-top: 3px;"),
      ""
    ),
    span(
      style = "margin-left: 5px; font-size: 0.8rem;",
      layer_label
    ) # span
  ) # div
}


