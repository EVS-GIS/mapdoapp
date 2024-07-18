
# UI ----------------------------------------------------------------------



#' mapdo_app UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom bslib popover
#' @importFrom bsicons bs_icon
#' @importFrom leaflet leafletOutput
#' @importFrom htmltools HTML div img
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mapdo_app_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(
          HTML("
          .form-group{margin-bottom: 10px}
          .leaflet-container {
            overflow: hidden !important;
            height: 500px; /* Ensure height is set to match the leafletOutput */
            width: 100%;
          }
          ")
        )
      ), # head
      fluidRow(
        column(
          width = 7,
          withSpinner(leafletOutput(ns("map"), height = 500)),
          textOutput(ns("selection_textUI"))
        ),
        column(
          width = 5,
          tabsetPanel(
            id = "tabset",
            tabPanel("Classes proposées",
                     mod_classification_proposed_ui("classification_proposed_1")
            ),
            tabPanel("Classification manuelle",
                     mod_classification_manual_ui("classification_manual_1")
            ),
            tabPanel("Aperçu métriques",
                     mod_metric_overview_ui("metric_overview_1")
            ), type = "pills"
          ) #tabsetpanel
        ) #column
      ), #row
      fluidRow(
        style = "margin-top: 10px;
        margin-bottom: 10px;",
        tabsetPanel(
          tabPanel(
            "Évolution longitudinale",
            mod_profil_long_ui("profil_long_1")
          ),
          tabPanel("Profil transversal",
                   mod_profil_transverse_ui("profil_transverse_1")
          ),
          tabPanel("Distribution des classes",
                   mod_classes_distribution_ui("classes_distribution_1")
          ), type = "pills"
        )
      )
    ) #page

  )
}



# SERVER ------------------------------------------------------------------


#' mapdo_app Server Functions
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet removeControl addLegend addControl
#' @importFrom leaflet.extras addWMSLegend
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull
#' @importFrom plotly event_register event_data plotlyProxy plotlyProxyInvoke renderPlotly plotlyOutput
#' @importFrom bslib popover update_popover
#' @importFrom bsicons bs_icon
#' @importFrom sf st_write
#' @importFrom shinyjs onclick runjs
#' @noRd
mod_mapdo_app_server <- function(id, con, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ### INITIALIZATION ####

    #### Map ####

    output$map <- renderLeaflet({
      r_val$bassins = data_get_bassins(opacity = r_val$opacity, con = con)
      map_init_bassins(bassins_data = r_val$bassins,
                       id_logo_ign_remonterletemps = ns("logo_ign_remonterletemps"))
    })
    onclick(id = "logo_ign_remonterletemps", expr =
              runjs(sprintf("window.open('%s', '_blank')",
                            utils_url_remonterletemps(lng = input$map_center$lng,
                                                      lat = input$map_center$lat,
                                                      zoom = input$map_zoom)))
    )

    #### Description Text ####

    output$selection_textUI <- renderText({
      r_val$selection_text
    })

    observe({
      r_val$map_proxy <- leafletProxy("map")
    })

    ### EVENT MAP CLICK ####

    observeEvent(input$map_shape_click, {

      # track input
      track_inputs(input = input)

      #### bassin clicked ####
      if (input$map_shape_click$group == params_map_group()[["bassin"]]){

        # get bassin name
        r_val$bassin_name = r_val$bassins %>%
          filter(cdbh == input$map_shape_click$id) %>%
          pull(lbbh)

        # disable the click interactivity for the bassin selected
        r_val$bassins = r_val$bassins %>%
          mutate(click = if_else(display == TRUE, TRUE, click)) %>%
          mutate(click = if_else(display == TRUE & cdbh == input$map_shape_click$id, FALSE, click))
        # get the regions data in selected bassin
        r_val$regions_in_bassin = data_get_regions_in_bassin(selected_bassin_id = input$map_shape_click$id,
                                                             opacity = r_val$opacity,
                                                             con = con)
        # update map : zoom in clicked bassin, clear bassin data, display region in bassin
        leafletProxy("map") %>%
          map_add_regions_in_bassin(bassin_click = input$map_shape_click,
                                    regions_data = r_val$regions_in_bassin,
                                    bassins_data = r_val$bassins)
        # print name of basin below map
        r_val$selection_text = paste0("Bassin: ", r_val$bassin_name)
      }

      ### region clicked ####
      if (input$map_shape_click$group == params_map_group()$region){

        # register first selection of region
        r_val$region_clicked = TRUE

        # set axis and dgo values back to NULL
        r_val$network_region_classified = NULL
        r_val$axis_name = NULL
        r_val$axis_click = NULL
        r_val$axis_clicked = FALSE
        r_val$dgo_axis = NULL
        r_val$axis_start_end = NULL
        r_val$data_dgo_clicked = NULL
        r_val$data_section = NULL
        r_val$network_region_classified = NULL
        r_val$dgo_axis_classified = NULL
        r_val$merged_networks_classified = NULL
        r_val$roe_region = NULL

        # store the region click values
        r_val$region_click = input$map_shape_click

        # disable the click interactivity for the bassin selected
        r_val$regions_in_bassin = r_val$regions_in_bassin %>%
          mutate(click = if_else(display == TRUE, TRUE, click)) %>%
          mutate(click = if_else(display == TRUE & gid == r_val$region_click$id, FALSE, click))

        # save the selected region feature for mapping
        r_val$selected_region_feature = data_get_region(region_click_id = r_val$region_click$id,
                                                        con = con)

        # get network of region
        r_val$network_region = data_get_network_region(selected_region_id = r_val$region_click$id,
                                                       con = con)

        # set region name to download
        r_val$region_name = utils_normalize_string(r_val$selected_region_feature$lbregionhy)

        # get the axis in the region
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con)
        # get ROE in region
        r_val$roe_region = data_get_roe_in_region(r_val$region_click$id,
                                                  con = con)

        # get hydro sites in region
        r_val$hydro_sites_region = data_get_hydro_sites(r_val$region_click$id,
                                                        con = con)

        # build SLD symbology
        r_val$sld_body = sld_get_style(
          breaks = sld_get_quantile_metric(
            selected_region_id = r_val$region_click$id,
            selected_metric = r_val$selected_metric,
            con = con
          ),
          colors = sld_get_quantile_colors(
            quantile_breaks = sld_get_quantile_metric(
              selected_region_id = r_val$region_click$id,
              selected_metric = r_val$selected_metric,
              con = con
            )
          ),
          metric = r_val$selected_metric
        )

        # map region clicked with region clicked and overlayers and initial network with strahler-order
        r_val$map_proxy %>%
          map_region_clicked(region_click = input$map_shape_click,
                             selected_region_feature = r_val$selected_region_feature,
                             regions_data = r_val$regions_in_bassin,
                             roe_region = r_val$roe_region,
                             hydro_sites_region = r_val$hydro_sites_region)

        # add strahler-order network visualization to map when classes visualisation is selected (and not manual)
        if ((r_val$visualization == "classes") && is.null(r_val$classes_proposed_selected)) {
          r_val$map_proxy %>%
            map_class(wms_params = params_wms()$class,
                      cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
                      sld_body = params_classes()[1,]$class_sld,
                      data_axis = r_val$network_region_axis) %>%
            addWMSLegend(uri = map_legend_metric(sld_body = params_classes()[1,]$class_sld),
                         position = "bottomright",
                         layerId = "legend_metric")
        }
        # map_metric(wms_params = params_wms()$metric,
        #            cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
        #            sld_body = r_val$sld_body,
        #            data_axis = r_val$network_region_axis) %>%
        # addWMSLegend(uri = map_legend_metric(sld_body = r_val$sld_body),
        #              position = "bottomright",
        #              layerId = "legend_metric")
        # addControl(
        #   html = HTML(paste0('<div id="legend-container" style="background: white; padding: 10px; border: 1px solid #ccc;">
        #                  <h6 id="legend-title" style="cursor: pointer; margin: 0; font-size: 12px;">Legend Title</h6>
        #                  <div id="legend-content">
        #                    <img src="', map_legend_metric(sld_body = r_val$sld_body), '" alt="Legend">
        #                  </div>
        #                </div>
        #                <script>
        #                  document.getElementById("legend-title").onclick = function() {
        #                    var content = document.getElementById("legend-content");
        #                    if (content.style.display === "none") {
        #                      content.style.display = "block";
        #                    } else {
        #                      content.style.display = "none";
        #                    }
        #                  };
        #                </script>')),
        #   position = "bottomright"
        # )

        # addControl(
        #   html = paste0('<div style="background: white; padding: 10px;">
        #             <h4>Legend Title</h4>
        #             <img src="', map_legend_metric(sld_body = r_val$sld_body), '" alt="Legend">
        #           </div>'),
        #   position = "bottomright"
        # )
        # addControl(
        #   html = "<strong>Custom Legend Title</strong>",
        #   position = "bottomright",
        #   className = "legend-title"
        # )




        # print name of basin and region below map
        r_val$selection_text = paste0("Bassin: ", r_val$bassin_name,
                                      ", région: ", r_val$selected_region_feature$lbregionhy)
      }

      ### axis clicked ####

      if (input$map_shape_click$group == params_map_group()$axis) {

        # set values back to NULL
        r_val$data_dgo_clicked = NULL
        r_val$data_section = NULL
        r_val$dgo_axis_classified = NULL
        r_val$merged_networks_classified = NULL
        r_val$leaflet_hover_measure = NULL
        r_val$roe_axis = NULL
        r_val$dgo_axis = NULL

        # save the clicked axis values
        r_val$axis_click = input$map_shape_click

        # reget the axis in the region without the selected axis
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con) %>%
          filter(axis != r_val$axis_click$id)

        # get the DGO axis data
        r_val$dgo_axis = data_get_network_axis(selected_axis_id = r_val$axis_click$id,
                                               con = con) %>%
          mutate(measure = measure/1000)

        # get axis name
        r_val$axis_name = r_val$dgo_axis$toponyme[1]

        # extract axis start end point
        r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$dgo_axis)

        # get ROE in axis clicked
        r_val$roe_axis = r_val$roe_region %>%
          filter(axis == r_val$axis_click$id)

        # map dgo axis when axis clicked and metric selected
        r_val$map_proxy %>%
          map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
                       main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric) %>%
          map_axis_start_end(axis_start_end = r_val$axis_start_end, region_axis = r_val$network_region_axis)

        # print name of basin and region below map
        r_val$selection_text = paste0("Bassin: ", r_val$bassin_name,
                                      ", région: ", r_val$selected_region_feature$lbregionhy,
                                      ", axe: ", r_val$axis_name)

        # create or update profile dataset with new axis
        r_val$selected_axis_df = r_val$dgo_axis %>%
          as.data.frame()

        # inform the first axis has been clicked
        r_val$axis_clicked = TRUE
      }
      #
      # ### dgo clicked ####
      #
      if (input$map_shape_click$group == params_map_group()$dgo_axis) {
        # get data with dgo id
        r_val$data_section = data_get_elevation_profiles(selected_dgo_fid = input$map_shape_click$id,
                                                         con = con)

        # get dgo clicked feature
        r_val$data_dgo_clicked = r_val$dgo_axis %>%
          filter(fid == input$map_shape_click$id)

        # Highlight clicked DGO
        r_val$map_proxy %>%
          map_dgo_cross_section(selected_dgo = r_val$data_dgo_clicked)
      }
    })

    #### DGO Mouseover ####

    # check for hover over dgo event
    observeEvent(input$map_shape_mouseover, {
      if (input$map_shape_mouseover$group == params_map_group()$dgo_axis && !is.null(input$map_shape_mouseover)){

        # extract dgo axis fid from map
        r_val$leaflet_hover_measure = r_val$dgo_axis %>%
          filter(fid == input$map_shape_mouseover$id) %>%
          pull(measure)
      } else {
        r_val$leaflet_hover_measure = NULL
      }
    })

    #### EVENT legend update ####

    observeEvent(input$map_groups, {

      # landuse map
      if (any(input$map_groups %in% params_map_group()$landuse)) {
        leafletProxy("map") %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = params_wms()$landuse),
                       position = "bottomleft",
                       layerId = "landuse")
      } else {
        leafletProxy("map") %>%
          removeControl(layerId = "landuse")
      }

      # lateral continuity map
      if (any(input$map_groups %in% params_map_group()$continuity)) {
        leafletProxy("map") %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = params_wms()$continuity),
                       position = "bottomleft",
                       layerId = "continuity")
      } else {
        leafletProxy("map") %>%
          removeControl(layerId = "continuity")
      }

      # valley bottom map
      if (any(input$map_groups %in% params_map_group()$valley_bottom)) {
        leafletProxy("map") %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = params_wms()$valley_bottom),
                       position = "bottomleft",
                       layerId = "valley_bottom")
      } else {
        leafletProxy("map") %>%
          removeControl(layerId = "valley_bottom")
      }

      # zone inondable map
      if (any(input$map_groups %in% params_map_group()$inondation)) {
        leafletProxy("map") %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = params_wms()$inondation),
                       position = "bottomleft",
                       layerId = "inondation")
      } else {
        leafletProxy("map") %>%
          removeControl(layerId = "inondation")
      }

      # ouvrage de protection map
      if (any(input$map_groups %in% params_map_group()[["ouvrage_protection"]])) {
        leafletProxy("map") %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = params_wms()$ouvrage_protection),
                       position = "bottomleft",
                       layerId = "ouvrage_protection")
      } else {
        leafletProxy("map") %>%
          removeControl(layerId = "ouvrage_protection")
      }

      # Custom legend HTML
      custom_legend <-

        # ROE
        if (any(input$map_groups %in% params_map_group()[["roe"]])) {
          leafletProxy("map") %>%
            addControl(
              HTML("
                <div style='display: flex; align-items: center;'>
                  <div style='background-color: #323232; border-radius: 50%; width: 12px; height: 12px; margin-right: 8px;'></div>
                  <div>Obstacles à l'Ecoulement (ROE)</div>
                </div>"),
              position = "bottomleft", layerId = "roe"
            )
          # addLegend(
          #   position = "bottomright",
          #   colors = "#323232",
          #   labels = "Obstacles à l'Ecoulement (ROE)",
          #   layerId = "roe"
          # )
        } else {
          leafletProxy("map") %>%
            removeControl(layerId = "roe")
        }

      # Site hydrométrique
      if (any(input$map_groups %in% params_map_group()[["hydro_sites"]])) {
        leafletProxy("map") %>%
          addControl(
            HTML("
                <div style='display: flex; align-items: center;'>
                  <div style='background-color: #33B1FF; border-radius: 50%; width: 12px; height: 12px; margin-right: 8px;'></div>
                  <div>Sites hydrométriques</div>
                </div>"),
            position = "bottomleft", layerId = "hydro_sites"
          )
        # addLegend(
        #   position = "bottomright",
        #   colors = "#33B1FF",
        #   labels = "Obstacles à l'Ecoulement (ROE)",
        #   layerId = "hydro_sites"
        # )
      } else {
        leafletProxy("map") %>%
          removeControl(layerId = "hydro_sites")
      }

      # Site hydrométrique
      # if (any(input$map_groups %in% params_map_group()[["hydro_sites"]])) {
      #   map_legend_vector_overlayer(layer_label = "Site hydrométrique",
      #                               color = "#33B1FF")
      # }
    })


  })
}
