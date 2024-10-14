#' metric_overview UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metric_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$style(HTML("
      .reactable-table {
        font-size: 15px;  /* Adjust this value to change the text size */
      }
    ")),
    fluidRow(style = "margin-top: 10px;",
             textOutput(ns("placeholder_ui")),
             column(width = 6,
                    uiOutput(ns("description"))),
             column(width = 6,
                    uiOutput(ns("selectinput")))
    ),
    fluidRow(
      div(class = "reactable-table",
          reactableOutput(ns("table"), width = "100%")
      )
    )
  )
}

#' metric_overview Server Functions
#'
#' @import shiny
#' @import dplyr
#' @importFrom reactable renderReactable
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr pivot_longer
#'
#' @noRd
mod_metric_overview_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      characteristics_table = NULL, # reactable table
      descriptionUI = NULL, # Description/Title text
      placeholder_text = "Cliquez sur un région hydrographique pour afficher la comparaison régionale des métriques. ",
      selectinputUI = NULL, # select input for unit
      unit = NULL, # unit for metrics

      region_pivot = NULL, # pivoted df of region as input for data_df
      axis_pivot = NULL, # pivoted df of region as input for data_df
      dgo_pivot = NULL, # pivoted df of dgo as input for data_df
      data_df = NULL # df to create table
    )

    output$table <- renderReactable(
      r_val_local$characteristics_table
    )

    output$description <- renderUI(
      r_val_local$descriptionUI
    )

    # text placeholder
    output$placeholder_ui <- renderText({
      r_val_local$placeholder_text
    })

    output$selectinput <- renderUI(
      r_val_local$selectinputUI
    )

    # observe if region clicked to create UI
    observe({
      if (r_val$region_clicked == TRUE) {
        # remove placeholder text
        r_val_local$placeholder_text = NULL

        # create header
        r_val_local$descriptionUI = HTML("<p><strong>Comparaison de moyennes </strong></p>")

        # create metric-select input
        r_val_local$selectinputUI = selectInput(ns("select_unit"), label = NULL,
                                                choices = list("surface relative (%)", "surface absolute (ha)"))
      }
    })

    # create pivoted df for axis each time it changes
    observeEvent(c(r_val$network_region, r_val$tab_open1), {
      if (!is.null(r_val$network_region) && (r_val$tab_open1 == "Aperçu métriques")) {

        r_val_local$region_pivot <- fct_table_pivot_sf(r_val$network_region) %>%
          dplyr::rename(mean_region = mean, distr_region = distr)

        # set axis and dgo to NULL
        r_val_local$axis_pivot = NULL
        r_val_local$dgo_pivot = NULL
      }
    })

    # create pivoted df for axis each time it changes
    observeEvent(c(r_val$dgo_axis, r_val$tab_open1), {
      if (!is.null(r_val$dgo_axis) && (r_val$tab_open1 == "Aperçu métriques")) {

          r_val_local$axis_pivot <- fct_table_pivot_sf(r_val$dgo_axis) %>%
            dplyr::rename(mean_axis = mean, distr_axis = distr)

          # set dgo_pivot to NULL
          r_val_local$dgo_pivot = NULL
      }
    })

      # create pivoted df for dgo each time it changes
      observeEvent(c(r_val$data_dgo_clicked, r_val$tab_open1), {
        if (!is.null(r_val$data_dgo_clicked) && (r_val$tab_open1 == "Aperçu métriques")) {
          # dgo data
          r_val_local$dgo_pivot <- r_val$data_dgo_clicked %>%
            sf::st_drop_geometry() %>%
            dplyr::select(-c(fid, axis, measure, toponyme, strahler, gid_region)) %>%
            tidyr::pivot_longer(-c(), names_to = "metric_name", values_to = "segment") %>%
            dplyr::mutate(segment = round(segment, 2))
        }
      })

      # check for changes in unit, or regional and axis network or selected dgo to create the df as basis for table
      observeEvent(c(r_val_local$region_pivot, r_val_local$axis_pivot, r_val_local$dgo_pivot), {

        if (!is.null(r_val_local$region_pivot)) {
          # create data for table
          r_val_local$data_df <- fct_table_create_table_df(region_pivot = r_val_local$region_pivot,
                                                           axis_pivot = r_val_local$axis_pivot,
                                                           dgo_pivot = r_val_local$dgo_pivot)
        }
      })

      # create new table each time when df changed
      observeEvent(c(r_val_local$data_df, input$select_unit),{

        if (!is.null(r_val_local$data_df) & !is.null(input$select_unit)){
          # check which surface unit should be used for metrics
          if (input$select_unit == "surface relative (%)") {
            r_val_local$characteristics_table <- fct_table_create_reactable(r_val_local$data_df, "%")
          } else {
            r_val_local$characteristics_table <- fct_table_create_reactable(r_val_local$data_df, "ha")
          }
        }

      })
  })
}
