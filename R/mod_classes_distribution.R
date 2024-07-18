#' classes_distribution UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom plotly plotlyOutput
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_classes_distribution_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    fluidRow(
      style = "margin-top: 10px;",
      textOutput(ns("placeholder_ui")),
      column(width = 8,
             plotlyOutput(ns("barplots_classes_metricUI"))
      )
    )
  )
}

#' classes_distribution Server Functions
#'
#' @import shiny
#' @importFrom plotly renderPlotly
#' @noRd
mod_classes_distribution_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      barplots_classes_metric = NULL,
      placeholder_text = "Sélectionnez un cours d'eau sur la carte et appliquez une classification pour afficher le graphique."
    )

    # text placeholder
    output$placeholder_ui <- renderText({
      r_val_local$placeholder_text
    })

    # barplots showing distribution of classes
    output$barplots_classes_metricUI <- renderPlotly({
      r_val_local$barplots_classes_metric
    })

    # listen to network changes and classify if new one is selected
    observeEvent(c(r_val$network_region, r_val$classes_proposed_selected, r_val$manual_classes_table), {

      if (!is.null(r_val$network_region)) {

        # classes proposed
        if ((r_val$visualization == "classes") && !is.null(r_val$classes_proposed_selected)) {

          # Create classified network by adding the classes and colors
          r_val$network_region_classified <- r_val$network_region %>%
            na.omit() %>%
            assign_classes_proposed(proposed_class = params_classes()[r_val$classes_proposed_selected,]$class_name)
        }
        # classes manually selected
        else if ((r_val$visualization == "manual") && !is.null(r_val$manual_classes_table)) {

          # Create classified network by adding the classes and colors
          r_val$network_region_classified <- r_val$network_region %>%
            na.omit() %>%
            assign_classes_manual(classes = r_val$manual_classes_table)
        }

        r_val$dgo_axis_classified = NULL
      }
    })

    # listen to axis changes and classify if new one is selected
    observeEvent(c(r_val$dgo_axis, r_val$classes_proposed_selected, r_val$manual_classes_table), {

      if (!is.null(r_val$dgo_axis)) {

        # classes proposed
        if ((r_val$visualization == "classes") && !is.null(r_val$classes_proposed_selected)){

          # create classified axis network
          r_val$dgo_axis_classified <- r_val$dgo_axis %>%
            na.omit() %>%
            assign_classes_proposed(proposed_class = params_classes()[r_val$classes_proposed_selected,]$class_name)
        }
        # classes manually selected
        else if ((r_val$visualization == "manual") && !is.null(r_val$manual_classes_table)) {

          # create classified axis network
          r_val$dgo_axis_classified <- r_val$dgo_axis %>%
            na.omit() %>%
            assign_classes_manual(classes = r_val$manual_classes_table)
        }
      }
    })

    # classify regional and axis network and merge them
    observeEvent(c(r_val$network_region_classified, r_val$dgo_axis_classified), {

      # classes proposed
      if (r_val$visualization == "classes") {
          # merge regional and axis network in one df
          r_val$merged_networks_classified <- merge_regional_axis_dfs(r_val$network_region_classified,
                                                                      r_val$dgo_axis_classified,
                                                                      "talweg_elevation_min",
                                                                      classes = TRUE)

        } else if ((r_val$visualization == "manual") && !is.null(r_val$manual_classes_table)) {

          # merge regional and axis network in one df
          r_val$merged_networks_classified <- merge_regional_axis_dfs(r_val$network_region_classified,
                                                                      r_val$dgo_axis_classified,
                                                                      r_val$manual_classes_table$variable[1],
                                                                      classes = TRUE)
        }
    })

    # create barplots of classes distribution
    observeEvent(r_val$merged_networks_classified, {
      if (!is.null(r_val$merged_networks_classified)) {
        r_val_local$barplots_classes_metric <- create_plotly_barplot(r_val$merged_networks_classified)
        r_val_local$placeholder_text = NULL
      } else {
        r_val_local$placeholder_text = "Sélectionnez une région sur la carte et appliquez une classification pour afficher le graphique."
        r_val_local$barplots_classes_metric = NULL
      }
    })


  })
}

