# UI ----------------------------------------------------------------------

#' expl_classes_manual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom bslib accordion accordion_panel
#' @importFrom shinyjs useShinyjs
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expl_classes_manual_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    useShinyjs(),
    div(
      id = ns("manual_classificationUI"),
      fluidRow(
        style = "margin-top: 10px;",
        uiOutput(ns("metric_selectUI"))
      ),
      fluidRow(
        style = "margin-bottom: 0px; padding-bottom: 0px;",
        uiOutput(ns("classificationUI"))
      )
    )
  )
}


# SERVER ------------------------------------------------------------------

#' expl_classes_manual Server Functions
#'
#' @import shiny
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull bind_cols arrange add_row
#' @importFrom tibble tibble
#' @importFrom bslib sidebar page_sidebar
#' @importFrom leaflet removeControl clearGroup
#' @importFrom leaflet.extras addWMSLegend
#' @importFrom shinyjs onclick runjs
#' @importFrom colourpicker colourInput
#' @noRd
mod_expl_classes_manual_server <- function(id, con, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ### REACTIVES ####
    r_val_local <- reactiveValues(
      ui_metric = NULL, # metric selection element

      # classification
      classification_ui = NULL, # UI placeholder
      scale_selectUI = NULL, # scale selection element
      initial_classes_table = NULL, # datainput for table

      reactableUI = NULL # reactable table
    )



    ### OUTPUTS ####

    # metric select input
    output$metric_selectUI <- renderUI({
      if (!is.null(r_val_local$ui_metric)) {
        div(
          style = "display: flex; align-items: center; margin-left: 20px",
          r_val_local$ui_metric,
          span(
            style = "display: flex; margin-left: 10px; margin-top: -10px",
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

    # classification UI enabling creation of different classes based on selected metric
    output$classificationUI <- renderUI({
      r_val_local$classification_ui
    })

    # classification UI enabling creation of different classes based on selected metric
    output$scale_select_UI <- renderUI({
      r_val_local$scale_selectUI
    })

    # reactable table of classes
    output$reactable_classes <- renderUI({
      r_val_local$reactableUI
    })

    ### EVENTS ####

    # update infobutton when metric selected changes for the first and second metric
    observe({
      if (!is.null(input$metric)) {
        update_popover("popover_metric",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$metric) %>%
                              pull(metric_description)))
      }
    })

    #### build UI selectors and initial table ####
    observeEvent(r_val$tab_classes, {

      # check if tab is open and classification UI is not created
      if (r_val$tab_classes == "Classification manuelle" && is.null(r_val_local$classification_ui)) {

        # create elements of manual grouping pane
        r_val_local$ui_metric = selectInput(ns("metric"), NULL,
                                            choices = globals$metric_choices,
                                            selected  = globals$metric_choices[1],
                                            width = "90%")

        # create classification UI
        r_val_local$classification_ui <- fluidRow(
          page_sidebar(
            sidebar = sidebar(
              fluidRow(
                column(width = 7,
                       numericInput(inputId = ns("man_grouping_quantile"),
                                    "Incl. valeurs aberrantes [%]", value = 95, min = 0, max = 100)
                ),
                column(width = 5,
                       numericInput(inputId = ns("man_grouping_no_classes"),
                                    "No. classes", value = 4, min = 2, max = 10, step = 1)
                ),
                uiOutput(ns("scale_select_UI")),
                actionButton(inputId = ns("recalculate_classes_button"), "Recalculer classes")
              ), open = "closed", width = 220, position = "right"
            ),
            uiOutput(ns("reactable_classes")),
            actionButton(inputId = ns("apply_to_map_button"), "Ajouter à la carte")
          ))

        # create classes-table to initialize classes UI
        r_val_local$initial_classes_table = create_df_input(
          variable_name = globals$metric_choices[[1]],
          q_0025 =
            globals$metric_stats() %>%
            dplyr::filter(level_type == "France (total)") %>%
            dplyr::pull(paste0(globals$metric_choices[[1]], "_0025")),
          q_0975 =
            globals$metric_stats() %>%
            dplyr::filter(level_type == "France (total)") %>%
            dplyr::pull(paste0(globals$metric_choices[[1]], "_0975"))  ,
          no_classes = 4,
          quantile = 95
        )
      }

    })

    #### scale selector ####
    observeEvent(c(r_val$basin_id, r_val$region_id, r_val$axis_id, r_val$tab_classes), {

      # france, basin, region, axis as basis possible !
      # add info-button

      if (r_val$tab_classes == "Classification manuelle") {

        if (is.null(r_val$basin_id)) {
          r_val_local$scale_selectUI = selectInput(ns("man_grouping_scale_select"),
                                                   "Base de classification",
                                                   choices = c("France"),
                                                   selected = "France")

        }
        # France, Basin
        else if (!is.null(r_val$basin_id)) {
          r_val_local$scale_selectUI = selectInput(ns("man_grouping_scale_select"),
                                                   "Base de classification",
                                                   choices = c("France", "Bassin"),
                                                   selected = "France")
        }
        # France, basin, region
        else if (!is.null(r_val$basin_id) && !is.null(r_val$region_id)) {
          r_val_local$scale_selectUI = selectInput(ns("man_grouping_scale_select"),
                                                   "Base de classification",
                                                   choices = c("France", "Bassin", "Région"),
                                                   selected = "France")
        }
        # France, Basin, Region, Axis
        # else if (!is.null(r_val$basin_id) && !is.null(r_val$region_id) && !is.null(r_val$axis_id)) {
        #   r_val_local$scale_selectUI = selectInput(ns("man_grouping_scale_select"),
        #                                            "Base de classification",
        #                                            choices = c("France", "Bassin", "Région", "Axe"),
        #                                            selected = "France")
        # }
      }
    })

    #### metric change / re-calculation clicked ####
    #### changes affecting classes ####
    observeEvent(c(input$metric, input$recalculate_classes_button), {

      if (r_val$tab_classes == "Classification manuelle") {
        if (!is.null(input$metric) &&
            !is.null(input$man_grouping_scale_select)) {

          scale <- case_when(
            input$man_grouping_scale_select == "France" ~ "France (total)",
            input$man_grouping_scale_select == "Bassin" ~ "Basin (total)",
            input$man_grouping_scale_select == "Région" ~ "Région (total)",
            TRUE ~ NA_character_  # Fallback to NA if none of the conditions match
          )

          name <- case_when(
            scale == "France (total)" ~ "France",
            scale == "Basin (total)" ~ ifelse(!is.null(r_val$basin_id), as.character(r_val$basin_id), NA_character_),
            scale == "Région (total)" ~ ifelse(!is.null(r_val$region_id), as.character(r_val$region_id), NA_character_),
            TRUE ~ NA_character_  # Fallback to NA if none of the conditions match
          )

          # create classes-table to initialize classes UI
          r_val_local$initial_classes_table = create_df_input(
            variable_name = input$metric,
            q_0025 =
              globals$metric_stats() %>%
              dplyr::filter(level_type == scale & level_name == name) %>%
              dplyr::pull(paste0(input$metric, "_0025")),
            q_0975 =
              globals$metric_stats() %>%
              dplyr::filter(level_type == scale & level_name == name) %>%
              dplyr::pull(paste0(input$metric, "_0975"))  ,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }
        # when scale selection not created
        else if (!is.null(input$metric) &&
                 is.null(input$man_grouping_scale_select)){

          # create classes-table to initialize classes UI
          r_val_local$initial_classes_table = create_df_input(
            variable_name = input$metric,
            q_0025 =
              globals$metric_stats() %>%
              dplyr::filter(level_type == "France (total)") %>%
              dplyr::pull(paste0(input$metric, "_0025")),
            q_0975 =
              globals$metric_stats() %>%
              dplyr::filter(level_type == "France (total)") %>%
              dplyr::pull(paste0(input$metric, "_0975"))  ,
            no_classes = 4,
            quantile = 95
          )
        }
      }

    })


    #### UI classes creation ####
    # update input fields for classification when setting the variables in the UI
    observeEvent(r_val_local$initial_classes_table, {

      if (!is.null(r_val_local$initial_classes_table)) {

        # create classes UI
        r_val_local$reactableUI =
          renderUI({
            # Start with a list to collect UI elements
            ui_elements <- list()

            # Add the first row of inputs
            ui_elements[[1]] <- fluidRow(
              column(width = 5, textInput(ns("class1"), label = "Classe", value = r_val_local$initial_classes_table$class[1])),
              column(width = 3, colourInput(ns("color1"), label = "Couleur",
                                            value = r_val_local$initial_classes_table$color[1],
                                            showColour = "background", closeOnClick = TRUE)),
              column(width = 4, numericInput(ns("greaterthan1"), label = "supérieur à", value = r_val_local$initial_classes_table$greaterthan[1]))
            )

            # Loop through the remaining rows and add inputs
            for (row in 2:nrow(r_val_local$initial_classes_table)) {
              ui_elements[[row]] <- fluidRow(
                style = "margin-bottom: 0px; padding-bottom: 0px;",
                column(width = 5, textInput(ns(paste0("class", row)), label = NULL, value = r_val_local$initial_classes_table$class[row])),
                column(width = 3, colourInput(ns(paste0("color", row)), label = NULL,
                                              value = r_val_local$initial_classes_table$color[row],
                                              showColour = "background", closeOnClick = TRUE)),
                column(width = 4, numericInput(ns(paste0("greaterthan", row)), label = NULL, value = r_val_local$initial_classes_table$greaterthan[row]))
              )
            }

            # Return the list of UI elements wrapped in tagList
            do.call(tagList, ui_elements)
          })
      }
    })


    #### apply-to-map button clicked ####
    observeEvent(input$apply_to_map_button,{

      r_val$visualization = "manual"
    })

    #### visualisation switched to manual ####
    observeEvent(c(r_val$visualization, input$apply_to_map_button), {

      if (r_val$visualization == "manual") {

        # create classes table from input
        # Initialize an empty tibble
        r_val$manual_classes_table <- tibble(variable = character(), class = character(), greaterthan = numeric(), color = character())

        # Add rows to the tibble looping through number of classes
        if (!is.null(input$man_grouping_no_classes)) {
          for (row in 1:input$man_grouping_no_classes) {
            r_val$manual_classes_table <- r_val$manual_classes_table %>%
              add_row(variable = input$metric,
                      class = input[[paste0("class", row)]],
                      greaterthan = input[[paste0("greaterthan", row)]],
                      color = input[[paste0("color", row)]])
          }
        } else {
          for (row in 1:4) {
            r_val$manual_classes_table <- r_val$manual_classes_table %>%
              add_row(variable = input$metric,
                      class = input[[paste0("class", row)]],
                      greaterthan = input[[paste0("greaterthan", row)]],
                      color = input[[paste0("color", row)]])
          }
        }

        # sort classes
        classes <- r_val$manual_classes_table %>%
          dplyr::arrange(greaterthan) %>%
          dplyr::mutate(greaterthan = round(greaterthan, 2))

        # build SLD symbology
        r_val$sld_body = sld_get_style_legend(
          breaks = classes$greaterthan,
          colors = classes$color,
          classnames = classes$class,
          metric = classes$variable[1]
        )

        # add classified network to map
        r_val$map_proxy %>%
          map_add_network_metric(wms_params = globals$wms_params$metric,
                                 breaks = classes$greaterthan,
                                 colors = classes$color,
                                 metric = classes$variable[1],
                                 sld_legend = r_val$sld_body,
                                 group = globals$map_group_params[["network"]])

      }
    })


    # apply classification to current axis data
    observeEvent(c(globals$axis_data(), r_val$manual_classes_table), {
      # manual classification applied
      if (r_val$visualization == "manual" && !is.null(globals$axis_data())) {
        r_val$axis_data_classified = globals$axis_data() %>%
          assign_classes_manual(classes = r_val$manual_classes_table)
      }
    })

  })
}
