#' profil_transverse UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profil_transverse_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "margin-top: 10px;",
      column(width = 12,
             plotlyOutput(ns("cross_section"))
      )
    )
  )
}

#' profil_transverse Server Functions
#'
#' @noRd
mod_profil_transverse_server <- function(id, r_val){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_val_locals <- reactiveValues(
      section = cr_profile_empty() # plotly render cross section output (default empty)
    )

    # output
    output$cross_section <- renderPlotly({
      return(r_val_locals$section)
    })

    # plot cross section when dgo clicked
    observe({

      if (!is.null(r_val$data_section)) {
        r_val_locals$section = cr_profile_main(data = r_val$data_section,
                                               axis_toponyme = unique(r_val$selected_axis_df$toponyme))
      } else if (is.null(r_val$data_section)){
        r_val_locals$section = cr_profile_empty()
      }
    })
  })
}

## To be copied in the UI
# mod_profil_transverse_ui("profil_transverse_1")

## To be copied in the server
# mod_profil_transverse_server("profil_transverse_1")
