#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets pickerInput pickerOptions multiInput materialSwitch
#' @importFrom htmltools HTML div img
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom reactable reactableOutput
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML("
          .form-group{margin-bottom: 10px}
          ")
      )
    ), # head

    tabsetPanel(
      id = ns("tabset"),
      tabPanel("Caractérisation de la sélection",
               # show table with France, basin, region (+ same stats but just for the strahler order of selected axis),
               # stats together with selected axis
               # below show distribution plots of selection
               fluidRow(
                 style = "margin-top: 10px; margin-bottom: 10px; margin-left: 10px;",
                 textOutput(ns("selection_textUI")),
                 column(
                   width = 9,
                   hr(), # horizontal line
                   uiOutput(ns("selact_tableUI")), # overview table
                   hr(), # horizontal line
                   fluidRow(style = "margin-top: 20px;"),
                   plotlyOutput(ns("selact_plotUI")), # distribution plot
                 ),
                 column(
                   width = 3,
                   div(
                     id = ns("selact_modifications"),
                     multiInput(
                       inputId = ns("selact_metric_select"),
                       label = "Métriques",
                       choiceNames = params_metrics()$metric_title,
                       choiceValues = params_metrics()$metric_name,
                       selected = params_metrics()$metric_name[1:5]
                     ),
                     selectInput(
                       inputId = ns("selact_strahler_select"),
                       label = "Ordre de Strahler",
                       choices = setNames(c(6,5,4,3,2,1,0), c("6","5","4","3","2","1","tous ensemble")),
                       selected = 0,
                       multiple = TRUE
                     ),
                     actionButton(inputId = ns("selact_apply_button"), "Actualiser")
                   )
                 ))),
      tabPanel("Comparaison des Régions",
               fluidRow(
                 style = "margin-top: 10px; margin-bottom: 10px; margin-left: 10px;",
                 column(
                   width = 9,
                   hr(), # horizontal line
                   reactableOutput(ns("regions_table"), width = "100%"),
                   hr(),
                   fluidRow(style = "margin-top: 20px;"),
                   plotlyOutput(ns("regions_plotUI"), width = "100%")
                 ),
                 column(
                   width = 3,
                   div(
                     id = ns("regions_modifications"),
                     multiInput(
                       inputId = ns("regions_metric_select"),
                       label = "Métriques",
                       choiceNames = params_metrics()$metric_title,
                       choiceValues = params_metrics()$metric_name,
                       selected = params_metrics()$metric_name[1:5]
                     ),
                     selectInput(
                       inputId = ns("regions_strahler_select"),
                       label = "Ordre de Strahler",
                       choices = setNames(c(6,5,4,3,2,1,0), c("6","5","4","3","2","1","tous ensemble")),
                       selected = 0,
                       multiple = TRUE
                     ),
                     actionButton(inputId = ns("regions_apply_button"), "Actualiser")
                   )
                 )
               )
      ),
      tabPanel("Analyse Bimétrique",
               mod_analysis_bimetric_ui("analysis_bimetric_1")
      ),
      type = "pills"
    ) #tabsetpanel
  )
}

#' analysis Server Functions
#'
#' @importFrom reactable renderReactable reactable
#' @importFrom plotly renderPlotly
#' @import shiny
#'
#' @noRd
mod_analysis_server <- function(id, con, r_val, globals){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_local <- reactiveValues(
      # actual selection
      selact_stats_prep = NULL,
      selact_table = NULL,
      selact_plot = NULL,

      # regions
      regions_table = NULL, # reactable
      region_stats_prep = NULL, # prepared stats for reactable
      regions_plot = NULL,

      # temporary vars to check changes
      classes_proposed_selected = NULL,
      manual_classes_table = NULL,
      basin_id = NULL,
      region_id = NULL,
      axis_id = NULL,
      selact_strahler_select = 10,
      regions_strahler_select = 10,
      selact_metric_select = NULL,
      regions_metric_select = NULL,

    )

    # selected tab identifier
    observeEvent(input$tabset, {
      r_val$tab_analysis = input$tabset
    })

    #### Description Text ####

    output$selection_textUI <- renderText({
      r_val$selection_text
    })

    ### UI ####

    ##### Selection tab ####

    # current selection table
    output$selact_tableUI <- renderUI({
      r_val_local$selact_table
    })

    # current selection distribution plot
    output$selact_plotUI <- renderPlotly({
      r_val_local$selact_plot
    })

    ##### Regions tab ####
    # render table for regions
    output$regions_table <- renderReactable({
      r_val_local$regions_table
    })

    # render regions plot
    output$regions_plotUI <- renderPlotly({
      r_val_local$regions_plot
    })  %>%
      bindCache(c(r_val_local$regions_plot, globals$regions_gids_key))


    ### Observers ####

    ##### both tabs ####
    # check only tab change and apply button?
    observeEvent(c(r_val$tab_page, input$regions_apply_button, input$selact_apply_button), {

      # check if Analysis-tab open
      if (r_val$tab_page == "Analyse") {

        # check if stats loaded
        if (exists("classes_stats", where = globals) && (exists("metric_stats", where = globals))) {

          if (!is.null(input$regions_strahler_select) && !is.null(input$regions_metric_select) &&
              !is.null(input$selact_strahler_select) && !is.null(input$selact_metric_select)) {

            ###### changes creation ####
            # check for changes in variables
            changes_scales_selact <- as.logical(
              !identical(r_val$basin_id, r_val_local$basin_id) ||
                !identical(r_val$region_id, r_val_local$region_id) ||
                !identical(r_val$axis_id, r_val_local$axis_id)
            )
            changes_metrics_selact <- as.logical(
              !utils_is_vector_identical(input$selact_metric_select, r_val_local$selact_metric_select)
            )
            changes_metrics_regions <- as.logical(
              !utils_is_vector_identical(input$regions_metric_select, r_val_local$regions_metric_select)
            )
            changes_strahler_selact <- as.logical(
              !utils_is_vector_identical(input$selact_strahler_select, r_val_local$selact_strahler_select)
            )
            changes_strahler_regions <- as.logical(
              !utils_is_vector_identical(input$regions_strahler_select, r_val_local$regions_strahler_select)
            )
            changes_classes <- as.logical(
              !identical(r_val$classes_proposed_selected, r_val_local$classes_proposed_selected) ||
                !utils_is_vector_identical(r_val$manual_classes_table, r_val_local$manual_classes_table)
            )

            # run code only when any changes detected
            if (changes_scales_selact || changes_metrics_selact || changes_metrics_regions ||
                changes_strahler_selact || changes_strahler_regions || changes_classes) {



              ##### Tables ####
              # REGIONS TAB - change table only when changes that apply to them are detected
              if (changes_metrics_regions || changes_strahler_regions) {
                # prepare stats for reactable
                r_val_local$region_stats_prep = prepare_regions_stats_for_table(globals$metric_stats(),
                                                                                region_names = globals$regions)

                # create table
                r_val_local$regions_table = create_analysis_table(r_val_local$region_stats_prep %>%
                                                                    filter(strahler == input$regions_strahler_select),
                                                                  input$regions_metric_select,
                                                                  scale_name = "Région")
              }

              # SELACT TAB - change table only when changes that apply to them are detected
              if (changes_metrics_selact || changes_strahler_selact || changes_scales_selact) {
                # get dataset of actual selection
                r_val_local$selact_stats_prep = prepare_selact_stats_for_table(globals$metric_stats(),
                                                                               basin_id = r_val$basin_id,
                                                                               region_id = r_val$region_id,
                                                                               axis_data = r_val$axis_data_classified)

                # create table
                r_val_local$selact_table = create_analysis_table(r_val_local$selact_stats_prep  %>%
                                                                   filter(strahler %in% input$selact_strahler_select | name == "Axe"),
                                                                 input$selact_metric_select,
                                                                 scale_name = "Sélection"
                )
              }


              ##### Plots #####
              # Plots - create manual classes stats
              if (changes_classes && r_val$visualization == "manual") {
                # obtain metric stats for manual classification
                r_val$classes_man_stats = data_get_distr_class_man(con = con, manual_classes_table = r_val$manual_classes_table)
              }

              # Plots - create SELACT plots
              if (changes_classes || changes_scales_selact || changes_strahler_selact) {

                # proposed classification
                if (r_val$visualization == "classes") {
                  r_val_local$selact_plot = analysis_plot_classes_distr(
                    df = prepare_selact_data_for_plot(globals$classes_stats(),
                                                      basin_id = r_val$basin_id,
                                                      region_id = r_val$region_id,
                                                      strahler = input$selact_strahler_select,
                                                      axis_data = r_val$axis_data_classified)
                  )
                }

                # manual classification
                else if (r_val$visualization == "manual") {

                  r_val_local$selact_plot = analysis_plot_classes_distr(
                    df = prepare_selact_data_for_plot(r_val$classes_man_stats,
                                                      classification_type = "manual",
                                                      manual_classes_table = r_val$manual_classes_table,
                                                      basin_id = r_val$basin_id,
                                                      region_id = r_val$region_id,
                                                      strahler = input$selact_strahler_select,
                                                      axis_data = r_val$axis_data_classified)
                  )
                }
              }

              # Plots - create REGIONS plots
              if (changes_classes || changes_scales_regions || changes_strahler_regions) {

                # proposed classification
                if (r_val$visualization == "classes") {
                  r_val_local$regions_plot = analysis_plot_classes_distr(
                    df = prepare_regions_data_for_plot(globals$classes_stats(),
                                                       region_id = globals$regions[globals$regions$click == TRUE,]$gid,
                                                       region_strahler = input$regions_strahler_select,
                                                       region_names = globals$regions)
                  )
                }

                # manual classification
                else if (r_val$visualization == "manual") {

                  r_val_local$regions_plot = analysis_plot_classes_distr(
                    df = prepare_regions_data_for_plot(r_val$classes_man_stats,
                                                       classification_type = "manual",
                                                       manual_classes_table = r_val$manual_classes_table,
                                                       region_id = globals$regions[globals$regions$click == TRUE,]$gid,
                                                       region_strahler = input$regions_strahler_select,
                                                       region_names = globals$regions)
                  )
                }
              } # plots regions


              # update reactive values with current selection
              r_val_local$regions_strahler_select = input$regions_strahler_select
              r_val_local$regions_metric_select = input$regions_metric_select
              r_val_local$selact_strahler_select = input$selact_strahler_select
              r_val_local$selact_metric_select = input$selact_metric_select
              r_val_local$manual_classes_table = r_val$manual_classes_table
              r_val_local$basin_id = r_val$basin_id
              r_val_local$region_id = r_val$region_id
              r_val_local$axis_id = r_val$axis_id
            } # general changes detected
          } # check if all inputs are not null
        } # check if stats are loaded
      } # check if Analysis-tab open
    })
  })
}
