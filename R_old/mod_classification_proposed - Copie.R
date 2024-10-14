#' classification_proposed UI Function
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
mod_classification_proposed_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      style = "margin-top: 10px;",
      textOutput(ns("placeholder_ui")),
      reactableOutput(ns("table"), width = "100%")
    )
  )
}

#' classification_proposed Server Functions
#'
#' @import shiny
#' @importFrom reactable renderReactable getReactableState updateReactable
#'
#' @noRd
mod_classification_proposed_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      classes_tbl = NULL,
      table = NULL,
      placeholder_text = "Sélectionnez une région hydrographique sur la carte pour afficher la classification.",
      selected = NULL
    )

    # text placeholder
    output$placeholder_ui <- renderText({
      r_val_local$placeholder_text
    })

    output$table <- renderReactable(
      r_val_local$table
    )

    # build table when region first time clicked
    observeEvent(r_val$region_clicked, {

      if (!is.null(r_val$region_click)) {
        r_val_local$placeholder_text = NULL
        r_val_local$table = create_table_fluvialstyles(classes_proposed)
      }
    })

    # create table output and add classification to map when region changed or other variable selected
    observeEvent(c(input$table__reactable__selected, r_val$region_click), {

      # get actual selected classification from table
      selected <- getReactableState("table", "selected")

      # check if row is actually selected
      if (!is.null(selected)) {

        # create map styling based on selected classification
        r_val$sld_body = classes_proposed[selected,]$class_sld

        # add styling to map
        r_val$map_proxy %>%
          map_class(wms_params = params_wms()$class,
                    cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
                    sld_body = r_val$sld_body,
                    style = paste0("mapdo:", classes_proposed[selected,]$sld_style),
                    data_axis = r_val$network_region_axis)

        # set visualisation to classes to tell app that proposed classes are selected
        r_val$visualization = "classes"

        r_val_local$selected = selected
      }
    })

    observeEvent(r_val_local$selected, {
      if (!is.null(r_val_local$selected)) {
        r_val$classes_proposed_selected = r_val_local$selected
      }

    })


    # check if other visualization is applied to map and de-select all proposed classifications
    observeEvent(r_val$visualization, {
      if (r_val$visualization != "classes") {
        updateReactable("table", selected = NA)
      }
    })
  })
}
