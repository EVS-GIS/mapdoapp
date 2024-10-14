#' expl_classes_proposed UI Function
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
mod_expl_classes_proposed_ui <- function(id){
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

#' expl_classes_proposed Server Functions
#'
#' @import shiny
#' @importFrom reactable renderReactable getReactableState updateReactable
#'
#' @noRd
mod_expl_classes_proposed_server <- function(id, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      classes_tbl = NULL,
      table = NULL,
      selected = NULL
    )

    # build table
    output$table <- renderReactable(
      create_table_fluvialstyles(globals$classes_proposed)
    ) %>%
      bindCache(globals$classes_proposed$sld_style)

    ### OBSERVERS ####

    # create table output and add classification to map when other variable selected
    observeEvent(input$table__reactable__selected, {

      # get actual selected classification from table
      selected <- getReactableState("table", "selected")

      r_val$classes_proposed_selected = selected

      # check if row is actually selected
      if (!is.null(selected)) {


        # add styling to map
        r_val$map_proxy %>%
          clearGroup(globals$map_group_params[["network"]]) %>% # clear existing network layer
          map_add_network(globals$wms_params$network, group = globals$map_group_params[["network"]],
                          style = paste0("mapdo:", globals$classes_proposed[selected,]$sld_style))

        # set visualisation to classes to tell app that proposed classes are selected
        r_val$visualization = "classes"
      }
    })


    # check if other visualization is applied to map and de-select all proposed classifications
    observeEvent(r_val$visualization, {
      if (r_val$visualization != "classes") {
        updateReactable("table", selected = NA)
      }
    })


    observeEvent(c(globals$axis_data(), r_val$classes_proposed_selected), {
      # proposed classification applied
      if (r_val$visualization == "classes" && !is.null(globals$axis_data())) {

        r_val$axis_data_classified = globals$axis_data() %>%
          assign_classes_proposed(proposed_class = globals$classes_proposed[r_val$classes_proposed_selected,]$class_name,
                                  colors_df = globals$classes_proposed_colors)
      }
    })

  })
}
