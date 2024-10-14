#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom waiter useWaitress
#'
#' @noRd
app_ui <- function(request){

  tagList(
    # Inject custom CSS into the app for tab styling
    tags$style(
      HTML("
      /* Hover effect */
      .nav-pills > li > a:hover {
        background-color: #E3EBF7; /* Background color on hover */
      }

      /* Help-guide button placement */
      .help-btn {
          position: absolute;
          right: 18px;
          top: 18px;
          z-index: 1000; /* Make sure it's above other UI elements */
        }

      .navbar {
        position: relative;
      }
    ")
    ),

    useWaitress(color = "#366EC0"), # Add a loading screen to the app

    # Define the help button outside of navbarPage, but position it inside with CSS
    div(class = "help-btn",
        mod_help_guide_ui("help_guide_1") # Use the cicerone UI module here
    ),

    navbarPage(
      id = "navbarPage",
      title =
        img(src = "www/logos_mapdo_evs_ofb.png"),
      windowTitle = "Mapd'O App",
      tabPanel("Exploration & Classification", icon = icon("compass"), # find more icons here: https://fontawesome.com/search?q=info&o=r&m=free
               mod_explore_ui("explore_1")
      ),
      tabPanel("Analyse", icon = icon("magnifying-glass-chart"),
               mod_analysis_ui("analysis_1")
      ),
      tabPanel("Télechargement", icon = icon("download"),
               mod_download_ui("download_1")
      ),
      tabPanel("Informations",
               icon = icon("info"),
               mod_documentation_ui("documentation_1")
      ),
      theme = bs_theme(version = 5, bootswatch = "simplex", primary = "#366EC0")
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mapdoapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
