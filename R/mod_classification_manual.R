
# UI ----------------------------------------------------------------------

#' classification_manual UI Function
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
mod_classification_manual_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    useShinyjs(),
    fluidRow(
      style = "margin-top: 10px;",
      textOutput(ns("metric_placeholder_descriptionUI"))
    ),
    fluidRow(
      uiOutput(ns("metric_selectUI"))
    ),
    fluidRow(
      uiOutput(ns("classificationUI"))
    )
  )
}


# SERVER ------------------------------------------------------------------

#' classification_manual Server Functions
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
mod_classification_manual_server <- function(id, con, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    ### REACTIVES ####
    r_val_local <- reactiveValues(
      metric_placeholder_description = "Cliquez sur une région hydrographique pour afficher la classification de métriques. ",
      ui_metric = NULL, # metric selection element

      # classification
      classification_ui = NULL, # UI placeholder
      scale_selectUI = NULL, # scale selection element
      initial_classes_table = NULL, # datainput for table

      reactableUI = NULL # reactable table
    )

    ### OUTPUTS ####
    output$metric_placeholder_descriptionUI <- renderText({
      r_val_local$metric_placeholder_description
    })

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
                       HTML(params_metrics() %>%
                              filter(metric_name == input$metric) %>%
                              pull(metric_description)))
      }
    })

    #### region selected ####
    observeEvent(r_val$region_clicked,{

      if(r_val$region_clicked == TRUE){

        # remove placeholder text
        r_val_local$metric_placeholder_description = NULL

        # create elements of manual grouping pane
        r_val_local$ui_metric = selectInput(ns("metric"), NULL,
                                            choices = params_get_metric_choices(),
                                            selected  = params_get_metric_choices()[1],
                                            width = "100%")

        # create classification UI
        r_val_local$classification_ui <- fluidPage(
          fluidRow(
            page_sidebar(
              sidebar = sidebar(
                fluidRow(
                  column(width = 7,
                         numericInput(inputId = ns("man_grouping_quantile"),
                                      "Quantile [%]", value = 95, min = 0, max = 100)
                  ),
                  column(width = 5,
                         numericInput(inputId = ns("man_grouping_no_classes"),
                                      "Classes", value = 4, min = 2, max = 10, step = 1)
                  ),
                  uiOutput(ns("scale_select_UI")),
                  actionButton(inputId = ns("recalculate_classes_button"), "Recalculer classes")
                ), open = "closed", width = 220, position = "right"
              ),
              uiOutput(ns("reactable_classes")),
              actionButton(inputId = ns("apply_to_map_button"), "Ajouter à la carte")
            ))
        )

        r_val_local$scale_selectUI = radioButtons(ns("man_grouping_scale_select"),
                                                  "Base de classification",
                                                  c("Région"),
                                                  selected = "Région",
                                                  inline = TRUE)

        # create classes-table to initialize classes UI
        r_val_local$initial_classes_table = create_df_input(
          axis_data = r_val$network_region,
          variable_name = params_get_metric_choices()[[1]],
          no_classes = 4,
          quantile = 95
        )
      }

    })


    #### axis clicked first time ####
    observe({

      if (r_val$axis_clicked == TRUE) {

        r_val_local$scale_selectUI = radioButtons(ns("man_grouping_scale_select"),
                                                  "Base de classification",
                                                  c("Région", "Axe fluvial"),
                                                  selected = "Région",
                                                  inline = TRUE)
      }
    })

    #### metric change / re-calculation clicked ####
    observeEvent(c(input$metric, r_val$network_region), {

      # track input
      track_inputs(input = input)

      # check for valid selected metric
      if (!is.null(input$metric) &
                   !is.null(r_val$network_region)) {

        # create classes-table
        r_val_local$initial_classes_table = create_df_input(
          axis_data = r_val$network_region,
          variable_name = input$metric,
          no_classes = input$man_grouping_no_classes,
          quantile = input$man_grouping_quantile
        )
      }
    })

    #### re-calculate classes button clicked ####
    observeEvent(input$recalculate_classes_button, {

      # region selected
      if (input$man_grouping_scale_select == "Région" &
          !is.null(r_val$network_region)) {

        # create classes-table
        r_val_local$initial_classes_table = create_df_input(
          axis_data = r_val$network_region,
          variable_name = input$metric,
          no_classes = input$man_grouping_no_classes,
          quantile = input$man_grouping_quantile
        )
      }
      # axis selected
      else if (input$man_grouping_scale_select == "Axe fluvial" &
               !is.null(r_val$dgo_axis) ) {

        # create classes-table
        r_val_local$initial_classes_table = create_df_input(
          axis_data = r_val$dgo_axis,
          variable_name = input$metric,
          no_classes = input$man_grouping_no_classes,
          quantile = input$man_grouping_quantile
        )
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
      observeEvent(c(r_val$visualization, r_val$region_click, input$apply_to_map_button), {

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
          r_val$sld_body = sld_get_style(
            breaks = classes$greaterthan,
            colors = classes$color,
            metric = classes$variable[1]
          )

          # add classified network to map
          r_val$map_proxy %>%
            map_metric(wms_params = params_wms()$metric,
                       cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
                       sld_body = r_val$sld_body,
                       data_axis = r_val$network_region_axis) %>%
            addWMSLegend(uri = map_legend_metric(sld_body = r_val$sld_body),
                         position = "bottomright",
                         layerId = "legend_metric")
        }

      })
  })
}
