#' analysis_bimetric UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @import shinyWidgets
#' @importFrom plotly plotlyOutput
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_bimetric_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "margin-top: 10px; margin-bottom: 10px; margin-left: 10px;",
      column(
        width = 9,
        hr(),
        textOutput(ns("warning")),
        div(style = "margin-top: 10px;"),
        plotlyOutput(ns("plot")),
        hr(),
        uiOutput(ns("linear_dependency_text"), style = "margin-top: 10px;  margin-left: 20px;")
      ),
      column(
        width = 3,
        div(
          id = ns("analysis_settings"),
          div(
            style = "display: flex; align-items: center",
            selectInput(ns("x_metric"), label = "Métrique X:",
                        choices = globals$metric_choices,
                        selected  = globals$metric_choices[1]),
            span(
              style = "display: flex; margin-left: 10px; margin-top: 20px",
              popover(
                trigger = bsicons::bs_icon("info-circle"),
                "",
                placement = "right",
                id = ns("popover_metric_x")
              )
            )
          ),
          div(
            style = "display: flex; align-items: center",
            selectInput(ns("y_metric"), label = "Métrique Y:",
                        choices = globals$metric_choices,
                        selected  = globals$metric_choices[2]),
            span(
              style = "display: flex; margin-left: 10px; margin-top: 20px",
              popover(
                trigger = bsicons::bs_icon("info-circle"),
                "",
                placement = "right",
                id = ns("popover_metric_y")
              )
            )
          ),
          hr(),
          checkboxInput(ns("apply_classes"), "Colorier par classification", value = FALSE),
          div(
            style = "display: flex; align-items: center",
            checkboxInput(ns("apply_lm"), "Appliquer régression linéaire", value = FALSE),
            span(
              style = "display: flex; margin-left: 10px; margin-top: -5px",
              popover(
                trigger = bsicons::bs_icon("info-circle"),
                "",
                placement = "right",
                id = ns("popover_lm")
              )
            )
          ),
          actionButton(ns("create_plot"), "Créer graphe !", icon = icon("chart-line"))
        )
      )
    )
  )
}

#' analysis_bimetric Server Functions
#'
#' @import shiny
#' @importFrom plotly renderPlotly
#' @noRd
mod_analysis_bimetric_server <- function(id, con, r_val, globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### REACTIVES ####
    r_val_local <- reactiveValues(
      warning = "Selectionnez une axe hydrographique pour créer le graphe !", # warning message
      plot = NULL, # plot object
      base = NULL, #
      linear_dependency_text = "" # text describing linear dependency
    )

    #### UI ####

    output$plot <- renderPlotly({
      r_val_local$plot
    })

    output$warning <- renderText({
      r_val_local$warning
    })

    output$linear_dependency_text <- renderText({
      r_val_local$linear_dependency_text
    })


    ##### Metric info ####

    # update infobutton when metric selected changes for the first and second metric
    observe({
      if (!is.null(input$x_metric)) {
        update_popover("popover_metric_x",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$x_metric) %>%
                              pull(metric_description)))
      }
    })

    observe({
      if (!is.null(input$y_metric)) {
        update_popover("popover_metric_y",
                       HTML(globals$metrics_params %>%
                              filter(metric_name == input$y_metric) %>%
                              pull(metric_description)))
      }
    })

    observe({
      if (!is.null(input$apply_lm)) {
        update_popover("popover_lm",
                       HTML("La <b>régression linéaire</b> est une méthode statistique utilisée pour modéliser la relation entre une variable dépendante (Y) et une variable indépendante (X), dans le but de prédire Y en fonction de X. Dans ce modèle, le <i>coefficient de corrélation R</i> mesure la force et la direction de la relation linéaire entre les variables, 1 et -1 signifiant une relation positive/négative entièrement dépendante et 0 signifiant aucune relation. Le <i>coefficient de détermination R²</i> indique dans quelle mesure le modèle explique la variabilité de la variable dépendante (les valeurs proches de 1 signifiant une meilleure adéquation). La <i>valeur p</i> évalue l'importance de la relation : une faible valeur p (généralement < 0,05) indique que la variable indépendante a un impact significatif sur la variable dépendante dans le modèle."))
      }
    })

    #### ENTITY BASE UI ####
    observeEvent(c(input$plot, r_val$tab_analysis), {

      if (r_val$tab_analysis == "Analyse Bimétrique") {

        if (is.null(input$plot)) {
          r_val_local$warning = "Selectionnez une axe hydrographique et clickez sur 'Créer graphe !' pour afficher le graphe."

        }
        # Axis selected
        else if (!is.null(input$plot)) {
          r_val_local$warning = NULL
        }
      }
    })

    # create plot
    observeEvent(input$create_plot, {

      if (!is.null(r_val$axis_data_classified)) {

        r_val_local$plot <- create_analysis_biplot(df = r_val$axis_data_classified,
                                                   metric_x = input$x_metric,
                                                   metric_y = input$y_metric,
                                                   classes = input$apply_classes,
                                                   lm = input$apply_lm,
                                                   axis_name = r_val$axis_name)
      }
    })

  })
}
