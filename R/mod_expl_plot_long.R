#' expl_plot_long UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinyjs useShinyjs
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expl_plot_long_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    div(
      id = ns("longitudinal_profile"),
      fluidRow(
        style = "margin-top: 10px;",
        column(width = 10,
               plotlyOutput(ns("long_profile"))
        ),
        column(
          width = 2,
          style = "margin-top: 20px;",
          uiOutput(ns("profile_first_metricUI")),
          uiOutput(ns("profile_sec_metricUI")),
          hr(),
          uiOutput(ns("profile_roeUI")),
          uiOutput(ns("profile_backgroundUI")),
          uiOutput(ns("profile_background_smoothUI")),
          uiOutput(ns("profile_background_smooth_selUI"),
                   style = "margin-left : 23px;")
        )
      )
    )
  )
}

#' expl_plot_long Server Functions
#'
#' @import shiny
#' @importFrom dplyr filter pull
#' @importFrom plotly event_register plotlyOutput renderPlotly event_data plotlyProxy
#' @importFrom leaflet addPolylines clearGroup
#' @importFrom bsicons bs_icon
#'
#' @noRd
mod_expl_plot_long_server <- function(id, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### REACTIVES ####
    r_val_local <- reactiveValues(
      plot = lg_profile_empty(), # plotly render longitudinal profile output (default empty)
      leaflet_hover_shapes = NULL, # list to store vertical lines to display on longitudinal profile
      ui_roe_profile = NULL, # UI placeholder for ROE checkbox
      ui_background_profile = NULL, # UI placeholder for background classes checkbox
      ui_background_smooth = NULL, # UI placeholder for background classification smoothing checkbox
      ui_background_smooth_sel = NULL, # UI placeholder for background classification smoothing selectInput
      roe_vertical_line = NULL, # list with verticale line to plot on longitudinal profile
      axis_roe = NULL, # ROE data of axis

      # first metric
      profile_first_metric = NULL,
      proxy_first_axe = NULL,
      first_metric_name = NULL, # title
      first_metric_type = NULL, # metric type title


      selected_profile_metric_title = NULL, # metric title to be displayed instead of pure variable name
      selected_profile_metric_type = NULL, # metric type title

      # second metric
      proxy_second_axe = NULL,
      profile_sec_metric = NULL, # second metric selection
      sec_metric_name = NULL, # title
      sec_metric_type = NULL, # metric type title

      # plotly shapes
      shapes_dgo = NULL, # list with shapes to plot clicked dgo element as line
      shapes_roe = NULL, # list with shapes to plot ROE obstacles as lines
      shapes_background = NULL, # list with shapes to plot classes in background

      dgo_axis_classified_smoothed = NULL # smoothed classes df
    )

    #### UI ####

    output$long_profile <- renderPlotly({
      return(r_val_local$plot)
    })

    # selectinput for first metric
    output$profile_first_metricUI <- renderUI({
      if (!is.null(r_val_local$profile_first_metric)) {
        div(
          style = "display: flex; align-items: center",
          r_val_local$profile_first_metric,
          span(
            style = "display: flex; margin-left: 10px; margin-top: 20px",
            popover(
              trigger = bsicons::bs_icon("info-circle"),
              "",
              placement = "right",
              id = ns("popover_metric")
            )
          )
        )
      }
    })

    # add selectinput for second metric
    output$profile_sec_metricUI <- renderUI({

      if (!is.null(r_val_local$profile_sec_metric)) {
        div(
          style = "display: flex; align-items: center",
          r_val_local$profile_sec_metric,
          span(
            style = "margin-left: 10px; margin-top: 20px",
            popover(
              trigger = bsicons::bs_icon("info-circle"),
              "",
              placement = "right",
              id = ns("popover_metric2")
            )
          )
        )
      }
    })

    # checkbox display ROE
    output$profile_roeUI <- renderUI({
      r_val_local$ui_roe_profile
    })

    # checkbox display background based on classification
    output$profile_backgroundUI <- renderUI({
      r_val_local$ui_background_profile
    })

    # checkbox for background classification smoothing
    output$profile_background_smoothUI <- renderUI({
      r_val_local$ui_background_smooth
    })

    # selectinput for background classification smoothing
    output$profile_background_smooth_selUI <- renderUI({
      r_val_local$ui_background_smooth_sel
    })




    # make plot available to other
    observe({
      r_val$plot_long_proxy <- plotlyProxy("long_profile")
    })


    ##### Metric selectors ####

    observeEvent(r_val$axis_clicked, {

      if (!is.null(globals$axis_data()) & (r_val$axis_clicked == TRUE)) {

        # build second axis input selector
        r_val_local$profile_first_metric = selectInput(ns("profile_first_metric"), label = "Métrique :",
                                                       choices = globals$metric_choices,
                                                       selected  = globals$metric_choices[1])

        # build second axis input selector
        r_val_local$profile_sec_metric = selectInput(ns("profile_sec_metric"), label = "2éme métrique :",
                                                     choices = c("aucun", globals$metric_choices),
                                                     selected  = 1,
                                                     width = "100%")
      }
    })


    ##### Metric info ####

    # update infobutton when metric selected changes for the first and second metric
    observe({
      if (!is.null(input$profile_first_metric)) {
        update_popover("popover_metric",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$profile_first_metric) %>%
                              pull(metric_description)))
      }
    })

    observe({
      if (!is.null(input$profile_sec_metric)) {
        update_popover("popover_metric2",
                       HTML(
                         # check if no metric is selected
                         if (input$profile_sec_metric == "aucun") {
                           "Choisissez une deuxième métrique"
                         } else{
                           globals$metrics_params %>%
                             filter(metric_name == input$profile_sec_metric) %>%
                             pull(metric_description)
                         }
                         ))
      }
    })

    #### plot 1st metric ####
    observeEvent(c(!is.null(input$profile_first_metric), globals$axis_data()), {

      if (!is.null(input$profile_first_metric) & !is.null(globals$axis_data())) {

        # get metric title and type
        r_val_local$first_metric_name =
          globals$metrics_params |> filter(metric_name == input$profile_first_metric) |> pull(metric_title)
        r_val_local$first_metric_type =
          globals$metrics_params |> filter(metric_name == input$profile_first_metric) |> pull(metric_type_title)

        # create the list to add trace and layout to change second axe plot
        r_val_local$proxy_first_axe <- lg_profile_first(data = globals$axis_data(),
                                                          y = globals$axis_data()[[input$profile_first_metric]],
                                                          y_label = r_val_local$first_metric_name,
                                                          y_label_category = r_val_local$first_metric_type)
        # add second metric to plot
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("deleteTraces", 0) %>%
          plotlyProxyInvoke("addTraces", r_val_local$proxy_first_axe$trace, 0) %>%
          plotlyProxyInvoke("relayout", r_val_local$proxy_first_axe$layout)

        # build ROE and background checkboxInput
        if (is.null(r_val_local$ui_roe_profile)) {
          # build ROE checkboxInput
          r_val_local$ui_roe_profile = NULL # delete checkbox before creating new one
          r_val_local$ui_roe_profile = checkboxInput(ns("roe_profile"),
                                                     label = "Obstacles à l'Ecoulement",
                                                     value = FALSE)

          # build background classification checkboxInput
          r_val_local$ui_background_profile = NULL # delete checkbox before creating new one
          r_val_local$ui_background_profile = checkboxInput(ns("background_profile"),
                                                            label = "Classifications en arrière-plan",
                                                            value = FALSE)
        }
      } else {
        r_val_local$plot = plot = lg_profile_empty()
        r_val_local$ui_roe_profile = NULL # delete checkbox
        r_val_local$ui_background_profile = NULL # delete checkbox
      }
    })


    #### add / remove 2nd axis ####

    observeEvent(c(input$profile_sec_metric, input$profile_first_metric), {
      req(globals$axis_data())  # Ensure data exists

      # add second axis
      if (!is.null(input$profile_sec_metric) && input$profile_sec_metric != "aucun") {
        # get metric title and type
        r_val_local$sec_metric_name =
          globals$metrics_params |> filter(metric_name == input$profile_sec_metric) |> pull(metric_title)
        r_val_local$sec_metric_type =
          globals$metrics_params |> filter(metric_name == input$profile_sec_metric) |> pull(metric_type_title)

        # create the list to add trace and layout to change second axe plot
        r_val_local$proxy_second_axe <- lg_profile_second(data = globals$axis_data(),
                                                          y = globals$axis_data()[[input$profile_sec_metric]],
                                                          y_label = r_val_local$sec_metric_name,
                                                          y_label_category = r_val_local$sec_metric_type)
        # add second metric to plot
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("deleteTraces", 1) %>%
          plotlyProxyInvoke("addTraces", r_val_local$proxy_second_axe$trace, 1) %>%
          plotlyProxyInvoke("relayout", r_val_local$proxy_second_axe$layout)
      }
      # remove second axis
      else {
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("deleteTraces", 1)

        r_val_local$proxy_second_axe = NULL
      }
    })

    #### SHAPES Plotly ####

    ##### clicked dgo ####

    observe({

      if (!is.null(r_val$swath_data_dgo)) {
        # remove the previous element
        r_val_local$shapes_dgo = NULL

        # get new shapes-element for longitudinal plot marker of clicked dgo for
        r_val_local$shapes_dgo <- list(lg_vertical_line(r_val$swath_data_dgo %>% pull(measure)))
      } else if (is.null(r_val$swath_data_dgo)) {
        r_val_local$shapes_dgo = NULL
      }
    })

    ##### ROE ####

    # observe the checkbox to display ROE obstacles
    observeEvent(input$roe_profile, {

      if (input$roe_profile == TRUE) {

        # get ROE when axis clicked
        r_val_local$axis_roe = globals$roe_sites() %>%
          filter(axis == r_val$axis_id) %>%
          select(distance_axis)

        # create the vertical line from ROE distance_axis
        r_val_local$shapes_roe = lg_roe_vertical_line(r_val_local$axis_roe$distance_axis)

      } else {
        # remove the previous ROE vertical lines if exist
        r_val_local$shapes_roe = NULL
      }
    })


    ##### background classes ####

    observeEvent(c(input$background_profile, r_val$axis_data_classified),  {

      if (!is.null(input$background_profile) && !is.null(globals$axis_data())) {

        # add background classification shapes
        if (input$background_profile == TRUE && !is.null(r_val$axis_data_classified)) {

          # create bckground shapes based on classified axis data
          if (!is.null(r_val$axis_data_classified)) {
            r_val_local$shapes_background = create_classes_background(r_val$axis_data_classified)
          }

          # build smoothing of classification option
          if (is.null(r_val_local$ui_background_smooth)) {
            # checkbox
            r_val_local$ui_background_smooth  = checkboxInput(ns("background_smooth"),
                                                              label = "Homogénéisation des classifications",
                                                              value = FALSE)

            # selectinput
            r_val_local$ui_background_smooth_sel = selectInput(ns("background_smooth_sel"),
                                                               label = "supprimer les inégaux :",
                                                               choices = c("singuliers", "en paires"),
                                                               selected = "1")
          }

        }
        # remove background classification
        else if (input$background_profile == FALSE) {
          r_val_local$shapes_background = NULL
          r_val_local$ui_background_smooth = NULL
          r_val_local$ui_background_smooth_sel = NULL
        }
      }
    })

    # observe the smoothing of the background classification
    observeEvent(input$background_smooth, {

      if (input$background_smooth == TRUE) {
        # add background classification shapes
        if (input$background_profile == TRUE && !is.null(r_val$axis_data_classified)) {

          # get number of unequals to b
            n_smooth <- case_when(
              input$background_smooth_sel == "singuliers" ~ 1,
              input$background_smooth_sel == "en paires" ~ 2,
              .default = 1,
            )

          r_val_local$dgo_axis_classified_smoothed = smoothen_classes(r_val$axis_data_classified, n_smooth)
          r_val_local$shapes_background = create_classes_background(r_val_local$dgo_axis_classified_smoothed)
        }
      }
      else if(!is.null(r_val_local$dgo_axis_classified_smoothed)) {
        r_val_local$shapes_background = create_classes_background(r_val$axis_data_classified)
        r_val_local$dgo_axis_classified_smoothed = NULL
      }
    })


    #### HOVER EVENTS ####

    ##### plotly profile ####

    # capture hover events on profile-plot to display dgo on map
    observeEvent(event_data("plotly_hover", source = 'L'), {

      if(!is.null(r_val_local$plot)) {
        # event data
        hover_event <- event_data("plotly_hover", source = 'L')

        # add line to map and plot
        if (!is.null(hover_event)) {
          hover_fid <- hover_event$key[1]
          highlighted_feature <- globals$axis_data()[globals$axis_data()$fid == hover_fid, ]
          r_val$map_proxy %>%
            addPolylines(data = highlighted_feature, color = "red", weight = 10,
                         group = globals$map_group_params$light)
        }
      }

    })

    # clear previous point on map when moving along profile to not display all the point move over
    observe({
      if (!is.null(r_val_local$plot)) {
        if (is.null(event_data("plotly_hover", source = 'L'))) {
          r_val$map_proxy %>%
            clearGroup(globals$map_group_params$light)
        }
      }
    })


    ##### leaflet map dgo mouseover ####

    observe({

      if (!is.null(r_val$leaflet_hover_measure)) {
        if (!is.null(r_val_local$plot)) {
          # remove the first element (hover dgo vertical line)
          r_val_local$leaflet_hover_shapes = NULL
          # add the new hover dgo vertical line
          r_val_local$leaflet_hover_shapes = list(lg_vertical_line(r_val$leaflet_hover_measure, color = "red"))
        }
      } else if (is.null(r_val$leaflet_hover_measure)) {
        r_val_local$leaflet_hover_shapes = NULL
      }
    })


    #### COMBINE shapes ####

    # reactive that listens to all changes in shapes and returns a combined list of them
    combined_shapes <- reactive({

      # create always new an empty list to store only shapes which are really activated
      shapes_list <- list()

      # Swath/dgo clicked
      if (!is.null(r_val_local$shapes_dgo)) {
        shapes_list <- c(shapes_list, r_val_local$shapes_dgo)
      }

      # ROE sites
      if (!is.null(r_val_local$shapes_roe)) {
        shapes_list <- c(shapes_list, r_val_local$shapes_roe)
      }

      # classes
      if (!is.null(r_val_local$shapes_background)) {
        shapes_list <- c(shapes_list, r_val_local$shapes_background)
      }

      # hovered over swath/dgo
      if (!is.null(r_val_local$leaflet_hover_shapes)) {
        shapes_list <- c(shapes_list, r_val_local$leaflet_hover_shapes)
      }

      shapes_list
    })


    # observe the combined shapes and update the plotly plot
    observe( {
      req(r_val_local$plot)
      req(input$profile_first_metric)

      if (!is.null(combined_shapes())) {

        # update profile with changed shapes
        plotlyProxy("long_profile") %>%
          plotlyProxyInvoke("relayout", list(shapes = combined_shapes()))
      }
    })
  })
}
