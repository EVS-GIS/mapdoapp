#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id) {
  ns <- NS(id)
  tagList(

    tags$head(
      tags$style(
        HTML("
          .form-group{margin-bottom: 10px}
          ")
      )
    ), # head

    fluidRow(
      column(
        width = 3,
        div(
          id = ns("download_selection"),
          # Input: choose dataset type
          selectInput(
            ns("select_type"),
            "Sélectionnez un ensemble de données :",
            choices = c("Réseau hydrographique", "Obstacles à l'écoulement", "Sites hydrométriques")
          ),

          # Input: choose scale
          uiOutput(ns("select_scaleUI")),

          # apply button
          actionButton(ns("prepare_download_button"), "Préparer et visualiser les données", icon = icon("magnifying-glass-chart"))
        )
      ),
      column(
        width = 9,
        div(
          id = ns("download_info"),
          fluidRow(
            column(
              width = 8,

              # Output: download info
              htmlOutput(ns("download_info"))
            ),
            column(
              width = 4,

              # Output: download button
              div(style = "display: flex; justify-content: flex-end;",
                  downloadButton(ns("download_button"), "Télécharger les données", icon = icon("download")))
            )
          ),
          hr(),
        ),

        # table
        div(style = 'overflow-x: auto; width: 100%;',  # Horizontal scroll and full-width div
            tableOutput(ns("table_data"))
        ),
      )
    )



  )
}

#' download Server Functions
#'
#' @import shiny
#' @importFrom sf st_as_text st_drop_geometry st_intersection
#'
#' @noRd
mod_download_server <- function(id, con, r_val, globals){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # reactive values ----
    r_val_local <- reactiveValues(
      scale = NULL, # scale selectInput
      dataset_input_name = NULL # dataset input name
    )


    # UI ----

    output$select_scaleUI <- renderUI({
      r_val_local$scale
    })

    # create select-input based on selected hydrographic entity
    observeEvent(c(r_val$tab_page, r_val$basin_id, r_val$region_id, r_val$axis_id, input$select_type), {

      # check if tab open
      if (r_val$tab_page == "Télechargement" ) {


        ### basin selected ####
        if (!is.null(r_val$basin_id) && is.null(r_val$region_id) && input$select_type != "Réseau hydrographique") {
          r_val_local$scale <- selectInput(
            ns("select_scale"),
            "Échelle :",
            choices = c("Bassin"),
            selected = "Bassin"
          )
        }
        else if (!is.null(r_val$basin_id) && is.null(r_val$region_id) && input$select_type == "Réseau hydrographique") {
          r_val_local$scale <- HTML("Sélectionnez une région ou axe pour continuer.")
        }


        ### region selected ####
        else if (!is.null(r_val$region_id) && is.null(r_val$axis_id)) {
          if (input$select_type != "Réseau hydrographique") {
            r_val_local$scale <- selectInput(
              ns("select_scale"),
              "Échelle :",
              choices = c("Bassin", "Région"),
              selected = "Région"
            )
          } else {
            r_val_local$scale <- selectInput(
              ns("select_scale"),
              "Échelle (Attention : charger la région peut prendre du temps)",
              choices = c("Région"),
              selected = "Région"
            )
          }
        }

        ### axis selected ####
        else if (!is.null(r_val$axis_id)) {

          if (input$select_type == "Obstacles à l'écoulement") {
            r_val_local$scale <- selectInput(
              ns("select_scale"),
              "Échelle :",
              choices = c("Bassin", "Région", "Axe"),
              selected = "Axe"
            )
          } else if (input$select_type == "Réseau hydrographique") {
            r_val_local$scale <- selectInput(
              ns("select_scale"),
              "Échelle (Attention : charger la région peut prendre du temps)",
              choices = c("Région", "Axe"),
              selected = "Axe"
            )
          } else if (input$select_type == "Sites hydrométriques") {
            r_val_local$scale <- selectInput(
              ns("select_scale"),
              "Échelle :",
              choices = c("Bassin", "Région"),
              selected = "Région"
            )
          }

        } else {
          r_val_local$scale <- HTML("Sélectionnez une entité hydrographique (bassin / région / axe) pour continuer.")
        }

      }
    })


    # Load data ----

    # prepare data for download
    observeEvent(input$prepare_download_button, {

      # check if tab open
      if (r_val$tab_page == "Télechargement" && input$select_scale %in% c("Bassin", "Région", "Axe")) {

        ### network ####
        if (input$select_type == "Réseau hydrographique") {
          if (input$select_scale == "Axe") {
            r_val$dataset_input <- r_val$axis_data_classified
            r_val_local$dataset_input_name <- paste0("axe_", r_val$axis_id, "_réseau_classifié")
            r_val_local$dataset_download_info <- paste0("Axe ", r_val$axis_id, ", ", r_val$axis_name)
          }
          else if (input$select_scale == "Région") {
            # set region id to load data from server
            r_val$region_id_data = r_val$region_id

            r_val$regions_data <- data_get_axis_dgos_from_region(selected_region_id = r_val$region_id_data, con)

            # proposed classification applied
            if (r_val$visualization == "classes" && !is.null(r_val$regions_data)) {
              r_val$dataset_input = r_val$regions_data %>%
                assign_classes_proposed(proposed_class = globals$classes_proposed[r_val$classes_proposed_selected,]$class_name,
                                        colors_df = globals$classes_proposed_colors)
            }

            # manual classification applied
            else if (r_val$visualization == "manual" && !is.null(r_val$regions_data)) {
              r_val$dataset_input = r_val$regions_data %>%
                assign_classes_manual(classes = r_val$manual_classes_table)
            }

            r_val_local$dataset_input_name <- paste0("région_", r_val$region_id, "_réseau_classifié")
            r_val_local$dataset_download_info <- paste0("Région ", r_val$region_id, ", ", r_val$region_name)
          }
        }

        ### ROE ####
        else if (input$select_type == "Obstacles à l'écoulement") {
          if (input$select_scale == "Axe") {
            r_val$dataset_input <- globals$roe_sites() %>%
              filter(axis == r_val$axis_id)
            r_val$dataset_input$geom_wkt <- st_as_text(r_val$dataset_input$geom)
            r_val$dataset_input <- r_val$dataset_input %>%
              st_drop_geometry()
            r_val_local$dataset_input_name <- paste0("axe_", r_val$axis_id, "_roe")
            r_val_local$dataset_download_info <- paste0("Axe ", r_val$axis_id, ", ", r_val$axis_name)
          }

          else if (input$select_scale == "Région") {
            r_val$dataset_input <- globals$roe_sites() %>%
              filter(gid_region == r_val$region_id)
            r_val$dataset_input$geom_wkt <- st_as_text(r_val$dataset_input$geom)
            r_val$dataset_input <- r_val$dataset_input %>%
              st_drop_geometry()
            r_val_local$dataset_input_name <- paste0("région_", r_val$region_id, "_roe")
            r_val_local$dataset_download_info <- paste0("Région ", r_val$region_id, ", ", r_val$region_name)
          }

          else if (input$select_scale == "Bassin") {
            # get region ids in basin
            region_ids_in_basin <- globals$regions |> filter(cdbh == r_val$basin_id) |> pull(gid)

            r_val$dataset_input <- globals$roe_sites() %>%
              filter(gid_region %in% region_ids_in_basin)
            r_val$dataset_input$geom_wkt <- st_as_text(r_val$dataset_input$geom)
            r_val$dataset_input <- r_val$dataset_input %>%
              st_drop_geometry()

            r_val_local$dataset_input_name <- paste0("basin_", r_val$basin_id, "_roe")
            r_val_local$dataset_download_info <- paste0("Bassin ", r_val$basin_id, ", ", r_val$basin_name)
          }
        }

        ### Hydro sites ####
        else if (input$select_type == "Sites hydrométriques") {

          if (input$select_scale == "Bassin") {
            r_val$dataset_input <- globals$hydro_sites() |>
              sf::st_intersection(globals$basins() |> filter(cdbh == r_val$basin_id))
            r_val$dataset_input$geom_wkt <- st_as_text(r_val$dataset_input$geom)
            r_val$dataset_input <- r_val$dataset_input %>%
              st_drop_geometry()

            r_val_local$dataset_input_name <- paste0("bassin_", r_val$basin_id, "_hydrosites")
            r_val_local$dataset_download_info <- paste0("Bassin ", r_val$basin_id, ", ", r_val$basin_name)
          }

          else if (input$select_scale == "Région") {
            r_val$dataset_input <- globals$hydro_sites() |>
              sf::st_intersection(globals$regions |> filter(gid == r_val$region_id))
            r_val$dataset_input$geom_wkt <- st_as_text(r_val$dataset_input$geom)
            r_val$dataset_input <- r_val$dataset_input %>%
              st_drop_geometry()

            r_val_local$dataset_input_name <- paste0("région_", r_val$region_id, "_hydrosites")
            r_val_local$dataset_download_info <- paste0("Région ", r_val$region_id, ", ", r_val$region_name)

          }

        }
      }
    })

    # Download ----
    observe({

      if (!is.null(r_val_local$dataset_input)) {

        # prepare table
        output$table_data <- renderTable(
          head(r_val_local$dataset_input, n = 10),
          striped = TRUE,
          hover = TRUE,
          bordered = TRUE,
          width = "100%"
        )

        # download info
        output$download_info <- renderUI({
          HTML(paste0("<b>Aperçu du jeu de données - ", input$select_type, "</b><br>",
                      "<i>", r_val_local$dataset_download_info, " (", nrow(r_val_local$dataset_input), " lignes)</i>"))
        })

        # download handling
        output$download_button <- downloadHandler(
          filename = function() {
            paste0(r_val_local$dataset_input_name, ".csv")
          },
          content = function(file) {
            write.csv(r_val_local$dataset_input, file, row.names = FALSE)
          }
        )
      }

    })


  })
}
