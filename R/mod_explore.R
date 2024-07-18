# for dev, create reactivity graph
# library(reactlog)
# reactlog_enable()

#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_explore
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom bslib popover
#' @importFrom bsicons bs_icon
#'
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(
            HTML(".form-group{margin-bottom: 10px}") # less space below selectInput metric_type
        )
      ),
      fluidRow(
        column(
          width = 6,
          withSpinner(leafletOutput(ns("exploremap"), height = 700))
        ), # column
        column(
          width = 3,
          uiOutput(ns("metricUI")),
          uiOutput(ns("areaUI")),
          uiOutput(ns("radioButtonsUI")) # uiOutput radios buttons metrics
        ), # column

        column(
          width = 2,
          uiOutput(ns("strahlerfilterUI")),
          uiOutput(ns("metricsfilterUI")),
          uiOutput(ns("legendUI")),
          # uncomment line below to add the download button
          # uiOutput(ns("downloadUI"))
        ) # column
      ), # fluidRow
      fluidRow(
        tabsetPanel(
          tabPanel(
            "Profil en long",
              div(
                fluidRow(
                  column(width = 9,
                         plotlyOutput(ns("long_profile"))
                  ),
                  column(
                    width = 3,
                    style = "margin-top: 20px;",
                    uiOutput(ns("profilemetricUI")),
                    uiOutput(ns("profileareaUI")),
                    uiOutput(ns("profileradiobuttonUI")),
                    uiOutput(ns("removeprofileaxeUI"),
                             style = "margin-top: 10px;"), # more space above button
                    uiOutput(ns("profileroeUI"),
                             style = "margin-top: 10px;")
                  )
                )
              )
          ), # tabPanel
          tabPanel(
            title = div("Profil en travers",
                        span(
                          style = "display: inline; align-items: center",
                          popover(
                            trigger = bs_icon("info-circle"),
                            "Profil transversal médian correspondant à la
                            médiane des valeurs des transects réalisées tout les
                            10m sur le fond de vallée du tronçon sélectionné",
                            placement = "right",
                            id = "popover_cross_section"
                          )
                        )),
            div(
              fluidRow(
                column(width = 12,
                       plotlyOutput(ns("cross_section"))
                )
              )
            )
          ) # tabPanel
        )# tabsetPanel
      ),# fluidRow
      ### UI DEV TOOLS ####

      # fluidRow(
      #   column(
      #     width = 4,
      #     actionButton(ns("browser"), "browser")
      #   ),
      #   column(
      #     width = 8,
      #     verbatimTextOutput(ns("printcheck"))
      #   )
      # ) # fluidRow DEV TOOLS

      ### END DEV TOOLS
    ) # fluidPage
  ) # tagList
}

#' explore Server Functions
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull
#' @importFrom plotly event_register event_data plotlyProxy plotlyProxyInvoke renderPlotly plotlyOutput
#' @importFrom bslib popover update_popover
#' @importFrom bsicons bs_icon
#' @importFrom sf st_write
#' @importFrom shinyjs onclick runjs
#'
mod_explore_server <- function(id, con){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    ### DEV TOOLS ####
    output$printcheck = renderPrint({
      tryCatch({
        # event_data("plotly_hover")
        print(input$exploremap_center)
        print("exists")
      },
      shiny.silent.error = function(e) {
        print("doesn't exist")
      }
      )
    })
    observeEvent(input$browser, {
      browser()
    })

    ### R_VAL ####
    r_val <- reactiveValues(
      ### reactivity controler
      region_already_clicked = FALSE,
      profile_display = FALSE, # controle if metric and axis is selected = display the profile

      ### data
      bassins = NULL, # bassins data
      regions_in_bassin = NULL, # all the regions in selected bassin
      network_region_axis = NULL, # all the axis in the selected region
      selected_region_feature = NULL, # region data clicked
      region_click = NULL, # region clicked information list
      axis_click = NULL, # axis clicked information list
      dgo_axis = NULL, # all DGO in selected axis
      axis_start_end = NULL, # start / end df coordinates to add pin on map
      strahler = NULL, # min and max strahler values to set strahler filter UI
      min_max_metric = NULL, # min and max metric values to set metric filter UI
      selected_axis_df = NULL, # DGO in axis dataframe to plot longitudinal profile
      data_section = NULL, # DGO elevation data for section profile
      roe_region = NULL, # ROE data in selected region
      roe_axis = NULL, # ROE data in selected axis
      hydro_sites_region = NULL, # hydro sites data in selected region
      data_dgo_clicked = NULL, # DGO clicked by user for cross section profile

      ### metric selected by user
      selected_metric = NULL, # select main metric column name
      selected_metric_name = NULL, # select main metric name to display for user
      selected_metric_type = NULL, # select main metric type name to display for user

      ### profile metric selected by user
      selected_profile_metric = NULL, # select second metric column name
      selected_profile_metric_name = NULL, # select second metric name to display for user
      selected_profile_metric_type = NULL, # select second metric type name to display for user

      ### render UI generator
      plot = lg_profile_empty(), # plotly render longitudinal profile output (default empty)
      section = cr_profile_empty(), # plotly render cross section output (default empty)
      ui_strahler_filter = NULL,
      ui_metric_type = NULL,
      ui_metric = NULL,
      ui_unit_area = NULL,
      ui_metric_filter = NULL,
      ui_profile_metric_type = NULL,
      ui_profile_metric = NULL,
      ui_profile_unit_area = NULL,
      ui_remove_profile_axe = NULL,
      ui_roe_profile = NULL,
      ui_download = NULL,

      ### geoserver controler
      cql_filter = NULL, # WMS filter
      sld_body = NULL, # WMS SLD symbology

      ### others variables
      opacity = list(clickable = 0.01, not_clickable = 0.10), # opacity value to inform the user about available bassins and regions
      leaflet_hover_measure = 2.5, # measure field from mesure to add vertical line on longitudinal profile
      leaflet_hover_shapes = list(shapes = list(lg_vertical_line(2.5))), # list to store vertical lines to display on longitudinal profile
      roe_vertical_line = NULL, # list with verticale line to plot on longitudinal profile
      region_name = NULL # region name file formatted to be download
    )

    ### INIT MAP & PROFILE ####

    output$exploremap <- renderLeaflet({
      r_val$bassins = data_get_bassins(opacity = r_val$opacity, con = con)
      map_init_bassins(bassins_data = r_val$bassins,
                       id_logo_ign_remonterletemps = ns("logo_ign_remonterletemps"))
    })

    onclick(id = "logo_ign_remonterletemps", expr =
              runjs(sprintf("window.open('%s', '_blank')",
                            utils_url_remonterletemps(lng = input$exploremap_center$lng,
                                                       lat = input$exploremap_center$lat,
                                                       zoom = input$exploremap_zoom)))
    )

    output$long_profile <- renderPlotly({
      return(r_val$plot)
    })

    output$cross_section <- renderPlotly({
      return(r_val$section)
    })

    ### RENDER UI ####

    #### profile ####

    # add input UI for profile additional metric
    output$profilemetricUI <- renderUI({
      r_val$ui_profile_metric_type
    })

    # add radiobutton for profile additional metric
    output$profileradiobuttonUI <- renderUI({
      r_val$ui_profile_metric
    })

    # UI switch unit area for profile additional metric
    output$profileareaUI <- renderUI({
      r_val$ui_profile_unit_area
    })

    # button to remove second axe
    output$removeprofileaxeUI <- renderUI({
      r_val$ui_remove_profile_axe
    })

    # checkbox display ROE
    output$profileroeUI <- renderUI({
      r_val$ui_roe_profile
    })

    #### metric ####

    # UI create choose metric
    output$metricUI <- renderUI({
      if (!is.null(r_val$ui_metric_type)){
        div(
          style = "display: flex; align-items: center; margin-bottom: 0px",
          r_val$ui_metric_type,
          span(
            style = "display: flex; margin-left: 10px; margin-top: 12px",
            popover(
              trigger = bsicons::bs_icon("info-circle"),
              "",
              placement = "right",
              id = ns("popover_metric_type")
            )
          )
        )
      } else {
        HTML('<label class="control-label" id="wait-metric-label">
             Cliquez sur une région hydrographique pour afficher la sélection des métriques</label>')
      }
    })

    # UI metrics radio buttons
    output$radioButtonsUI <- renderUI({
      r_val$ui_metric
    })

    # UI switch unit area
    output$areaUI <- renderUI({
      r_val$ui_unit_area
    })

    #### download ####

    # Uncomment below to create add the download button
    # output$downloadUI <- renderUI({
    #   r_val$ui_download
    # })
    #
    # output$download <- downloadHandler(
    #   filename = function() {
    #     paste0(Sys.Date(), "_", r_val$region_name, ".gpkg")
    #   },
    #   content = function(file) {
    #     data = data_get_dgo_in_region(r_val$region_click$id, con = con)
    #     st_write(obj = data, dsn = file, layer = r_val$region_name,
    #              driver = "GPKG", delete_dsn = TRUE)
    #   }
    # )

    #### filter ####

    # UI strahler filter
    output$strahlerfilterUI <- renderUI(
      {
        r_val$ui_strahler_filter
      })

    # UI dynamic filter on metric selected
    output$metricsfilterUI <- renderUI({
      r_val$ui_metric_filter
    })

    #### map legend ####

    metric_legend <- reactiveVal(NULL)

    output$legendUI <- renderUI({

      div(
        HTML('<label class="control-label" id="legend-label">Légende</label>'),
        # metric
        div(
          style = "display: flex; align-items: center;",
          metric_legend(),
        ),
        # landuse map
        if (any(input$exploremap_groups %in% params_map_group()$landuse)) {
          map_legend_wms_overlayer(wms_params = params_wms()$landuse)
        },
        # continuity map
        if (any(input$exploremap_groups %in% params_map_group()$continuity)) {
          map_legend_wms_overlayer(wms_params = params_wms()$continuity)
        },
        # valley bottom map
        if (any(input$exploremap_groups %in% params_map_group()$valley_bottom)) {
          map_legend_wms_overlayer(wms_params = params_wms()$valley_bottom)
        },
        # zone inondable
        if (any(input$exploremap_groups %in% params_map_group()$inondation)) {
          map_legend_wms_overlayer(wms_params = params_wms()$inondation)
        },
        # ouvrage de protection
        if (any(input$exploremap_groups %in% params_map_group()[["ouvrage_protection"]])) {
          map_legend_wms_overlayer(wms_params = params_wms()$ouvrage_protection)
        },
        # ROE
        if (any(input$exploremap_groups %in% params_map_group()[["roe"]])) {
          map_legend_vector_overlayer(layer_label = "Référentiel des Obstacles à l'Ecoulement",
                                      color = "#323232")
        },
        # Site hydrométrique
        if (any(input$exploremap_groups %in% params_map_group()[["hydro_sites"]])) {
          map_legend_vector_overlayer(layer_label = "Site hydrométrique",
                                      color = "#33B1FF")
        },
        style = "margin-bottom: 10px;"
      ) # div
    })

    ### EVENT MAP CLICK ####

    observeEvent(input$exploremap_shape_click,{

      # track input
      track_inputs(input = input)

      #### bassin clicked ####
      if (input$exploremap_shape_click$group == params_map_group()[["bassin"]]){
        # disable the click interactivity for the bassin selected
        r_val$bassins = r_val$bassins %>%
          mutate(click = if_else(display == TRUE,
                                 TRUE,
                                 click)) %>%
          mutate(click = if_else(display == TRUE & cdbh == input$exploremap_shape_click$id,
                                 FALSE,
                                 click))
        # get the regions data in selected bassin
        r_val$regions_in_bassin = data_get_regions_in_bassin(selected_bassin_id = input$exploremap_shape_click$id,
                                                             opacity = r_val$opacity,
                                                             con = con)
        # update map : zoom in clicked bassin, clear bassin data, display region in bassin
        leafletProxy("exploremap") %>%
          map_add_regions_in_bassin(bassin_click = input$exploremap_shape_click,
                                    regions_data = r_val$regions_in_bassin,
                                    bassins_data = r_val$bassins)
      }

      ### region clicked ####
      if (input$exploremap_shape_click$group == params_map_group()$region){
        # store the region click values
        r_val$region_click = input$exploremap_shape_click
        # disable the click interactivity for the bassin selected
        r_val$regions_in_bassin = r_val$regions_in_bassin %>%
          mutate(click = if_else(display == TRUE,
                                 TRUE,
                                 click)) %>%
          mutate(click = if_else(display == TRUE & gid == r_val$region_click$id,
                                 FALSE,
                                 click))

        # save the selected region feature for mapping
        r_val$selected_region_feature = data_get_region(region_click_id = r_val$region_click$id,
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
        # get strahler data
        r_val$strahler = isolate(data_get_min_max_strahler(selected_region_id = r_val$region_click$id,
                                                           con = con))
        # build strahler slider
        r_val$ui_strahler_filter = sliderInput(ns("strahler"),
                                       label="Ordre de strahler",
                                       min=r_val$strahler[["min"]],
                                       max=r_val$strahler[["max"]],
                                       value=c(r_val$strahler[["min"]],
                                               r_val$strahler[["max"]]),
                                       step=1)

        # map region clicked with region clicked and overlayers
        leafletProxy("exploremap") %>%
          map_region_clicked(region_click = input$exploremap_shape_click,
                             selected_region_feature = r_val$selected_region_feature,
                             regions_data = r_val$regions_in_bassin,
                             roe_region = r_val$roe_region,
                             hydro_sites_region = r_val$hydro_sites_region)

        # run only once, control with region_already_clicked
        if (r_val$region_already_clicked == FALSE){
          # build metric selectInput
          r_val$ui_metric_type =
            selectInput(ns("metric_type"), "Sélectionnez une métrique :",
                        choices = utils_get_metric_type(params_metrics_choice()),
                        selected  = utils_get_metric_type(params_metrics_choice())[1])

          # create download button
          r_val$ui_download = downloadButton(
            ns("download"),
            label = "Télécharger les données"
          )
          r_val$region_already_clicked = TRUE
        }
      }
      ### axis clicked ####

      if (input$exploremap_shape_click$group == params_map_group()$axis) {
        # save the clicked axis values
        r_val$axis_click = input$exploremap_shape_click
        # reget the axis in the region without the selected axis
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con) %>%
          filter(axis != r_val$axis_click$id)
        # get the DGO axis data
        r_val$dgo_axis = data_get_network_axis(selected_axis_id = r_val$axis_click$id,
                                               con = con) %>%
          mutate(measure = measure/1000)
        # extract axis start end point
        r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$dgo_axis)
        # get ROE in axis clicked
        r_val$roe_axis = r_val$roe_region %>%
          filter(axis == r_val$axis_click$id)

        # map dgo axis when axis clicked and metric selected
        leafletProxy("exploremap") %>%
          map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
                       main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric) %>%
          map_axis_start_end(axis_start_end = r_val$axis_start_end, region_axis = r_val$network_region_axis)

        # create or update profile dataset with new axis
        r_val$selected_axis_df = r_val$dgo_axis %>%
          as.data.frame()

        # update profile with new metric selected
        if (r_val$profile_display == TRUE){
          proxy_main_axe <-
            lg_profile_update_main(
              data = r_val$selected_axis_df,
              y = r_val$selected_axis_df[[r_val$selected_metric]],
              y_label = r_val$selected_metric_name,
              y_label_category = r_val$selected_metric_type
            )

          plotlyProxy("long_profile") %>%
            plotlyProxyInvoke("deleteTraces", 0) %>%
            plotlyProxyInvoke("addTraces", proxy_main_axe$trace, 0) %>%
            plotlyProxyInvoke("relayout", proxy_main_axe$layout)

          # update ROE vertical lines
          if (input$roe_profile == TRUE){
            if (!is.null(r_val$roe_vertical_line)){
              # remove the previous ROE vertical lines if exist
              r_val$leaflet_hover_shapes$shapes <- list(r_val$leaflet_hover_shapes$shapes[[1]])
            }
            # create the vertical line from ROE distance_axis
            r_val$roe_vertical_line <- lg_roe_vertical_line(r_val$roe_axis$distance_axis)
            # increment the vertical list shape to keep the hover map vertical line
            r_val$leaflet_hover_shapes$shapes <- c(r_val$leaflet_hover_shapes$shapes,
                                                   r_val$roe_vertical_line)
            # update profile
            plotlyProxy("long_profile") %>%
              plotlyProxyInvoke("relayout",  r_val$leaflet_hover_shapes)
          }else{
            # remove the previous ROE vertical lines if exist
            r_val$leaflet_hover_shapes$shapes <- list(r_val$leaflet_hover_shapes$shapes[[1]])
            # update profile
            plotlyProxy("long_profile") %>%
              plotlyProxyInvoke("relayout",  r_val$leaflet_hover_shapes)
          }


          if(!is.null(input$profile_metric)){ # second metric selected = update second metric profile
            # create the list to add trace and layout to change second axe plot
            proxy_second_axe <- lg_profile_second(data = r_val$selected_axis_df,
                                                  y = r_val$selected_axis_df[[r_val$selected_profile_metric]],
                                                  y_label = r_val$selected_profile_metric_name,
                                                  y_label_category = r_val$selected_profile_metric_type)

            plotlyProxy("long_profile") %>%
              plotlyProxyInvoke("deleteTraces", 1) %>%
              plotlyProxyInvoke("addTraces", proxy_second_axe$trace, 1) %>%
              plotlyProxyInvoke("relayout", proxy_second_axe$layout)
          }
        }
      }

      ### dgo clicked ####

      if (input$exploremap_shape_click$group == params_map_group()$dgo_axis) {
        # get data with dgo id
        r_val$data_section = data_get_elevation_profiles(selected_dgo_fid = input$exploremap_shape_click$id,
                                                         con = con)
        # plot cross section
        r_val$section = cr_profile_main(data = r_val$data_section,
                                        axis_toponyme = unique(r_val$selected_axis_df$toponyme))
        # get dgo clicked feature
        r_val$data_dgo_clicked = r_val$dgo_axis %>%
          filter(fid == input$exploremap_shape_click$id)
        # Highlight clicked DGO
        leafletProxy("exploremap") %>%
          map_dgo_cross_section(selected_dgo = r_val$data_dgo_clicked)
      }
    })

    ### EVENT METRIC ####

    #### metric type select ####

    observeEvent(input$metric_type, {

      # track input
      track_inputs(input = input)

      if (!is.null(input$metric_type)){
        update_popover("popover_metric_type",
                       HTML(params_metrics_choice()[[input$metric_type]]$metric_type_info))
      }


      # build metric radioButtons with popover icon
        r_val$ui_metric = radioButtons(
        inputId = ns("metric"),
        label = NULL,
        choiceNames = utils_button_label_with_popover(input$metric_type),
        choiceValues = as.list(names(utils_get_metric_name_value(input$metric_type))),
        selected = character(0)
      )

      # build selectInput unit area for landuse or continuity
      if (input$metric_type == "landuse" || input$metric_type == "continuity"){
        r_val$ui_unit_area = selectInput(ns("unit_area"), "Surfaces :",
                                         choices = params_unit_area(),
                                         selected = unname(params_unit_area()[1]))
      }else{
        r_val$ui_unit_area = NULL
      }
    })

    #### metric select ####

    observeEvent(c(input$metric, input$unit_area), ignoreInit = TRUE, {

      # track input
      track_inputs(input = input)

      # change field if unit_area in percentage
      if (!is.null(input$metric) && input$unit_area == "percent"
          && (input$metric_type %in% c("landuse", "continuity"))){
        r_val$selected_metric = paste0(input$metric,"_pc")
      } else if (!is.null(input$metric)) {
        r_val$selected_metric = input$metric
      }

      if (!is.null(input$metric)){
        r_val$selected_metric_name = params_metrics_choice()[[input$metric_type]]$metric_type_values[[input$metric]]$metric_title
        r_val$selected_metric_type = params_metrics_choice()[[input$metric_type]]$metric_type_title

        # build metric filter slider
        r_val$min_max_metric <- data_get_min_max_metric(selected_region_id = r_val$region_click$id,
                                                        selected_metric = r_val$selected_metric,
                                                        con = con)

        r_val$ui_metric_filter = sliderInput(ns("metricfilter"),
                                             label = r_val$selected_metric_name,
                                             min = isolate(r_val$min_max_metric[["min"]]),
                                             max = isolate(r_val$min_max_metric[["max"]]),
                                             value = c(
                                               isolate(r_val$min_max_metric[["min"]]),
                                               isolate(r_val$min_max_metric[["max"]])
                                             )
        )

        # update profile with new metric selected
        if (r_val$profile_display == TRUE){

          proxy_main_axe <-
            lg_profile_update_main(
              data = r_val$selected_axis_df,
              y = r_val$selected_axis_df[[r_val$selected_metric]],
              y_label = r_val$selected_metric_name,
              y_label_category = r_val$selected_metric_type
            )

          plotlyProxy("long_profile") %>%
            plotlyProxyInvoke("deleteTraces", 0) %>%
            plotlyProxyInvoke("addTraces", proxy_main_axe$trace, 0) %>%
            plotlyProxyInvoke("relayout", proxy_main_axe$layout)
        }
      }
    })

    ### EVENT METRIC & AXIS RESULTS ####

    observeEvent(c(r_val$selected_metric, r_val$axis_click), {

      if (r_val$profile_display == FALSE){

        # track input
        track_inputs(input = input)

        if (!is.null(r_val$selected_metric) && !is.null(r_val$axis_click)){

          r_val$profile_display = TRUE # this event run only one time controlled with profile_display

          # build input for profile metric type
          r_val$ui_profile_metric_type = selectInput(ns("profile_metric_type"), "Ajoutez une métrique :",
                                                     choices = utils_get_metric_type(params_metrics_choice()),
                                                     selected  = utils_get_metric_type(params_metrics_choice())[1])

          # built ROE checkboxInput and input
          r_val$ui_roe_profile = checkboxInput(ns("roe_profile"), label = "ROE", value = FALSE)

          # update dgo on axis to reset tooltip
          leafletProxy("exploremap") %>%
            map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
                         main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric)

          # plot single axe with metric selected
          r_val$plot = lg_profile_main(data = r_val$selected_axis_df,
                                       y = r_val$selected_axis_df[[r_val$selected_metric]],
                                       y_label = r_val$selected_metric_name,
                                       y_label_category = r_val$selected_metric_type) %>%
            event_register("plotly_hover")
        }
      }
    })

    ### EVENT PROFILE METRIC ####

    #### profile metric type select ####

    observeEvent(input$profile_metric_type, {

      # track input
      track_inputs(input = input)

      # build profile metric radio button
      r_val$ui_profile_metric = radioButtons(
        inputId = ns("profile_metric"),
        label = NULL,
        choiceNames = unname(utils_get_metric_name_value(input$profile_metric_type)),
        choiceValues = names(utils_get_metric_name_value(input$profile_metric_type)),
        selected = character(0)
      )

      # build profile unit area select
      if (input$profile_metric_type == "landuse" ||
          input$profile_metric_type == "continuity") {
        r_val$ui_profile_unit_area = selectInput(
          ns("profile_unit_area"),
          "Surfaces :",
          choices = params_unit_area(),
          selected = unname(params_unit_area()[1])
        )
      } else{
        r_val$ui_profile_unit_area = NULL
      }

      r_val$ui_remove_profile_axe = actionButton(
        ns("remove_profile_axe"),
        label = "Retirer le second axe"
      )
    })

    #### profile metric select ####

    observeEvent(c(input$profile_metric, input$profile_unit_area), ignoreInit = TRUE, {

      # track input
      track_inputs(input = input)

      # change field if unit_area in percentage
      if (!is.null(input$profile_metric) && input$profile_unit_area == "percent"
          && (input$profile_metric_type %in% c("landuse", "continuity"))){
        r_val$selected_profile_metric = paste0(input$profile_metric,"_pc")
      } else if (!is.null(input$profile_metric)) {
        r_val$selected_profile_metric = input$profile_metric
      }

      if (!is.null(input$profile_metric)){
        r_val$selected_profile_metric_name = params_metrics_choice()[[input$profile_metric_type]]$metric_type_values[[input$profile_metric]]$metric_title
        r_val$selected_profile_metric_type = params_metrics_choice()[[input$profile_metric_type]]$metric_type_title

        # update map to change tooltip labels
        leafletProxy("exploremap") %>%
          map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
                       main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric)

        # create the list to add trace and layout to change second axe plot
        proxy_second_axe <- lg_profile_second(data = r_val$selected_axis_df,
                                              y = r_val$selected_axis_df[[r_val$selected_profile_metric]],
                                              y_label = r_val$selected_profile_metric_name,
                                              y_label_category = r_val$selected_profile_metric_type)

        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("deleteTraces", 1) %>%
          plotlyProxyInvoke("addTraces", proxy_second_axe$trace, 1) %>%
          plotlyProxyInvoke("relayout", proxy_second_axe$layout)
      }
    })

    #### profile metric remove axe ####

    observeEvent(input$remove_profile_axe, {

      # track input
      track_inputs(input = input)

      plotlyProxy("long_profile") %>%
        plotlyProxyInvoke("deleteTraces", 1)

      updateRadioButtons(session, "profile_metric", selected = character(0))

      r_val$selected_profile_metric = NULL
      # update dgo on axis to reset tooltip
      leafletProxy("exploremap") %>%
        map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
                     main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric)

    })

    #### profile metric add ROE ####

    observeEvent(input$roe_profile, {

      # track input
      track_inputs(input = input)

      if (input$roe_profile == TRUE){
        if (!is.null(r_val$roe_vertical_line)){
          # remove the previous ROE vertical lines if exist
          r_val$leaflet_hover_shapes$shapes <- list(r_val$leaflet_hover_shapes$shapes[[1]])
        }
        # create the vertical line from ROE distance_axis
        r_val$roe_vertical_line <- lg_roe_vertical_line(r_val$roe_axis$distance_axis)
        # increment the vertical list shape to keep the hover map vertical line
        r_val$leaflet_hover_shapes$shapes <- c(r_val$leaflet_hover_shapes$shapes,
                                               r_val$roe_vertical_line)
        # update profile
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("relayout",  r_val$leaflet_hover_shapes)
      }else{
        # remove the previous ROE vertical lines if exist
        r_val$leaflet_hover_shapes$shapes <- list(r_val$leaflet_hover_shapes$shapes[[1]])
        # update profile
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("relayout",  r_val$leaflet_hover_shapes)
      }
    })

    ### EVENT FILTER ####

    observeEvent(c(input$strahler, input$metricfilter, r_val$ui_strahler_filter), {

      # track input
      track_inputs(input = input)

      if (is.null(input$metricfilter)){
        # build WMS cql_filter
        r_val$cql_filter = paste0("gid_region=", r_val$selected_region_feature[["gid"]],
                                  " AND strahler>=", input$strahler[1],
                                  " AND strahler <= ", input$strahler[2])

        r_val$sld_body = NULL

      } else {
        # build WMS cql_filter
        r_val$cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]],
                                  " AND strahler>=",input$strahler[1],
                                  " AND strahler <= ",input$strahler[2],
                                  " AND ",r_val$selected_metric,">=",input$metricfilter[1],
                                  " AND ",r_val$selected_metric,"<=",input$metricfilter[2])

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

        # update legend
        metric_legend(map_legend_metric(sld_body = r_val$sld_body))
      }
      # update map with basic style
      leafletProxy("exploremap") %>%
        map_metric(wms_params = params_wms()$metric,
                   cql_filter = r_val$cql_filter, sld_body = r_val$sld_body,
                   data_axis = r_val$network_region_axis)
    })

    ### EVENT MOUSEOVER ####

    #### plotly profile ####

    # Define an observeEvent to capture hover events
    observeEvent(event_data("plotly_hover"), {
      if (!is.null(event_data("plotly_hover"))) {
        hover_fid <- event_data("plotly_hover")$key[1]
        highlighted_feature <- r_val$dgo_axis[r_val$dgo_axis$fid == hover_fid, ]
        leafletProxy("exploremap") %>%
          addPolylines(data = highlighted_feature, color = "red", weight = 10,
                       group = params_map_group()$light)
      }
    })

    # clear previous point on map when moving along profile to not display all the point move over
    observe({
      if (is.null(event_data("plotly_hover"))) {
        leafletProxy("exploremap") %>%
          clearGroup(params_map_group()$light)
      }
    })

    #### leaflet map ####

    # add vertical line on profil on map user mouseover axis
    observeEvent(input$exploremap_shape_mouseover, {
      if (input$exploremap_shape_mouseover$group == params_map_group()$dgo_axis && !is.null(input$exploremap_shape_mouseover)){
        # extract dgo axis fid from map
        r_val$leaflet_hover_measure <- r_val$dgo_axis %>%
          filter(fid == input$exploremap_shape_mouseover$id) %>%
          pull(measure)
        # remove the first element (hover dgo vertical line)
        r_val$leaflet_hover_shapes <- list(shapes = r_val$leaflet_hover_shapes$shapes[-1])
        # add the new hover dgo vertical line
        r_val$leaflet_hover_shapes$shapes <- c(list(lg_vertical_line(r_val$leaflet_hover_measure)), r_val$leaflet_hover_shapes$shapes)
        # change profile layout with vertical line
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("relayout", r_val$leaflet_hover_shapes)
      }
    })
  })
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
