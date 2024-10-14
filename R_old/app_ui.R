#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @noRd
app_ui <- function(request){
  navbarPage(
    theme = bs_theme(version = 5, bootswatch = "simplex"),
    title =
      img(src = "www/logos_mapdo_evs_ofb.png"),
    tabPanel("Mapd'O App",
             mod_mapdo_app_ui("mapdo_app_1")
    ),
    tabPanel("Documentation",
             icon = icon("info"),
             mod_documentation_ui("documentation_1")
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
