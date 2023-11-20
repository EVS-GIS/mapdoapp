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
#'
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # UI elements
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(
            HTML(".form-group{margin-bottom: 0px}") # less space below selectInput metric_type
        )
      ),
      fluidRow(
        column(
          width = 3,
          titlePanel(""),
          uiOutput(ns("metricUI")),
          uiOutput(ns("areaUI")),
          uiOutput(ns("radioButtonsUI")) # uiOutput radios buttons metrics
        ), # column
        column(
          width = 7,
          titlePanel(""),
          withSpinner(leafletOutput(ns("exploremap")))
        ), # column
        column(
          width = 2,
          titlePanel(""),
          uiOutput(ns("strahlerfilterUI")),
          uiOutput(ns("metricsfilterUI")),
          uiOutput(ns("legendUI")),
          uiOutput(ns("downloadUI")),
          verbatimTextOutput(ns("printcheck"))
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
                             style = "margin-top: 10px;") # more space above button
                  )
                )
              )
          ), # tabPanel
          tabPanel(title = "Profil en travers"
          ) # tabPanel
        )# tabsetPanel
      )# fluidRow
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
#' @importFrom dplyr filter mutate
#' @importFrom plotly event_register event_data plotlyProxy plotlyProxyInvoke renderPlotly plotlyOutput
#' @importFrom bslib popover update_popover
#' @importFrom bsicons bs_icon
#' @importFrom sf st_write
#' @importFrom shinyjs onclick runjs
#'
mod_explore_server <- function(id){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    # suppress warnings
    # options(warn = -1)

    # dev print to debug value
    # output$printcheck = renderPrint({
    #   tryCatch({
    #     # event_data("plotly_hover")
    #     print(input$exploremap_center)
    #     print("exists")
    #   },
    #   shiny.silent.error = function(e) {
    #     print("doesn't exist")
    #   }
    #   )
    # })

    ### R_VAL ####
    r_val <- reactiveValues(
      regions_in_bassin = NULL, # all the regions in selected bassin
      network_region_axis = NULL, # all the axis in the selected region
      selected_region_feature = NULL,
      region_click = NULL,
      axis_click = NULL,
      dgo_axis = NULL, # all selected axis DGO
      axis_start_end = NULL, # start / end df coordinates to add pin on map
      # metric selected by user
      selected_metric = NULL,
      selected_metric_name = NULL,
      selected_metric_type = NULL,
      # profile metric selected by user
      selected_profile_metric = NULL,
      selected_profile_metric_name = NULL,
      selected_profile_metric_type = NULL,
      strahler = NULL,
      ui_strahler_filter = NULL,
      ui_metric_type = NULL,
      ui_metric = NULL,
      ui_unit_area = NULL,
      min_max_metric = NULL,
      ui_metric_filter = NULL,
      ui_profile_metric_type = NULL,
      ui_profile_metric = NULL,
      ui_profile_unit_area = NULL,
      ui_remove_profile_axe = NULL,
      ui_download = NULL,
      cql_filter = NULL, # WMS filter
      sld_body = NULL, # WMS SLD symbology
      selected_axis_df = NULL, # dgo_axis dataframe to plot graph
      profile_display = FALSE, # controle if metric and axis is selected = display the profile
      plot = lg_profile_empty(),
      plotly_hover = NULL,
      region_name = NULL
    )

    ### INIT MAP & PROFILE ####

    output$exploremap <- renderLeaflet({
      map_init_bassins(bassins_data = data_get_bassins(),
                       id_logo_ign_remonterletemps = ns("logo_ign_remonterletemps"))
    })

    # url <- "https://remonterletemps.ign.fr/comparer/basic?x=6.869433&y=45.923690&z=16&layer1=ORTHOIMAGERY.ORTHOPHOTOS.1950-1965&layer2=ORTHOIMAGERY.ORTHOPHOTOS&mode=vSlider"

    onclick(id = "logo_ign_remonterletemps", expr =
              runjs(sprintf("window.open('%s', '_blank')",
                            params_url_remonterletemps(lng = input$exploremap_center$lng,
                                                       lat = input$exploremap_center$lat,
                                                       zoom = input$exploremap_zoom)))

              # runjs(sprintf("window.open(%s, '_blank')",
              #               params_url_remonterletemps(input$exploremap_center$lng,
              #                                          input$exploremap_center$lat)
              #               )
              #       )
    )
    # params_url_remonterletemps(input$exploremap_center$lng, input$exploremap_center$lat)

    output$long_profile <- renderPlotly({
      return(r_val$plot)
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

    output$downloadUI <- renderUI({
      r_val$ui_download
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0(Sys.Date(), "_", r_val$region_name, ".gpkg")
      },
      content = function(file) {
        data = data_get_dgo_in_region(r_val$region_click$id)
        st_write(obj = data, dsn = file, layer = r_val$region_name,
                 driver = "GPKG", delete_dsn = TRUE)
      }
    )

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
          map_legend_vector_overlayer(layer_label = "ROE")
        },
        style = "margin-bottom: 10px;"
      ) # div
    })

    ### EVENT MAP CLICK ####

    observeEvent(input$exploremap_shape_click,{
      #### bassin clicked ####
      if (input$exploremap_shape_click$group == params_map_group()[["bassin"]]){
        # get the regions data in selected bassin
        r_val$regions_in_bassin = data_get_regions_in_bassin(selected_bassin_id = input$exploremap_shape_click$id)
        # update map : zoom in clicked bassin, clear bassin data, display region in bassin
        leafletProxy("exploremap") %>%
          map_add_regions_in_bassin(bassin_click = input$exploremap_shape_click,
                                    regions_data = r_val$regions_in_bassin)
      }

      ### region clicked ####
      if (input$exploremap_shape_click$group == params_map_group()$region){

        # store the region click values
        r_val$region_click = input$exploremap_shape_click
        # save the selected region feature for mapping
        r_val$selected_region_feature = data_get_region(region_click_id = r_val$region_click$id)
        # set region name to download
        r_val$region_name = utile_normalize_string(r_val$selected_region_feature$lbregionhy)
        # get the axis in the region
        r_val$network_region_axis = data_get_axis(selected_region_id = input$exploremap_shape_click$id)
        # get strahler data
        r_val$strahler = isolate(data_get_min_max_strahler(selected_region_id = r_val$region_click$id))
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
                             selected_region_feature = r_val$selected_region_feature)

        # build metric selectInput
        r_val$ui_metric_type =
            selectInput(ns("metric_type"), "Sélectionnez une métrique :",
                        choices = utile_get_metric_type(params_metrics_choice()),
                        selected  = utile_get_metric_type(params_metrics_choice())[1])

        # create download button
        r_val$ui_download = downloadButton(
          ns("download"),
          label = "Téléchager les données"
        )

      }
      ### axis clicked ####

      if (input$exploremap_shape_click$group == params_map_group()$axis) {

        # save the clicked axis values
        r_val$axis_click = input$exploremap_shape_click
        # reget the axis in the region without the selected axis
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id) %>%
          filter(fid != r_val$axis_click$id)
        # get the DGO axis data
        r_val$dgo_axis = data_get_network_axis(selected_axis_id = r_val$axis_click$id) %>%
          mutate(measure = measure/1000)
        # extract axis start end point
        r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$dgo_axis)

        # map dgo axis when axis clicked and metric selected
        leafletProxy("exploremap") %>%
          map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis) %>%
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
    })

    ### EVENT METRIC ####

    #### metric type select ####

    observeEvent(input$metric_type, {
      browser()

      if (!is.null(input$metric_type)){
        update_popover("popover_metric_type",
                       HTML(params_metrics_choice()[[input$metric_type]]$metric_type_info))
      }


      # build metric radioButtons with popover icon
        r_val$ui_metric = radioButtons(
        inputId = ns("metric"),
        label = NULL,
        choiceNames = button_label_with_popover(input$metric_type),
        choiceValues = as.list(names(utile_get_metric_name_value(input$metric_type))),
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
        r_val$min_max_metric <- data_get_min_max_metric(selected_region_id = r_val$region_click$id, selected_metric = r_val$selected_metric)

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
        if (!is.null(r_val$selected_metric) && !is.null(r_val$axis_click)){

          r_val$profile_display = TRUE # this event run only one time controlled with profile_display

          # build input for profile metric type
          r_val$ui_profile_metric_type = selectInput(ns("profile_metric_type"), "Ajoutez une métrique :",
                                                     choices = utile_get_metric_type(params_metrics_choice()),
                                                     selected  = utile_get_metric_type(params_metrics_choice())[1])

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

      # build profile metric radio button
      r_val$ui_profile_metric = radioButtons(
        inputId = ns("profile_metric"),
        label = NULL,
        choiceNames = unname(utile_get_metric_name_value(input$profile_metric_type)),
        choiceValues = names(utile_get_metric_name_value(input$profile_metric_type)),
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
      plotlyProxy("long_profile") %>%
        plotlyProxyInvoke("deleteTraces", 1)

      updateRadioButtons(session, "profile_metric", selected = character(0))

    })

    ### EVENT FILTER ####

    observeEvent(c(input$strahler, input$metricfilter), {
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
            selected_metric = r_val$selected_metric
          ),
          colors = sld_get_quantile_colors(
            quantile_breaks = sld_get_quantile_metric(
              selected_region_id = r_val$region_click$id,
              selected_metric = r_val$selected_metric
            )
          ),
          metric = r_val$selected_metric
        )

        # update legend
        metric_legend(map_legend_metric(sld_body = r_val$sld_body))
      }
      # update map with basic style
      leafletProxy("exploremap") %>%
        map_metric(wms_params = params_wms()$metric, # metric_basic to have blue network
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
        select_measure <- r_val$dgo_axis %>%
          filter(fid == input$exploremap_shape_mouseover$id)
        # change profile layout with vertial line
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("relayout", list(shapes = list(lg_vertical_line(select_measure$measure))))
      }
    })
  })
}


## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
