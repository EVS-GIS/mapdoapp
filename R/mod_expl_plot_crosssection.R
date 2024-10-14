#' expl_plot_crosssection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_expl_plot_crosssection_ui <- function(id){
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

#' expl_plot_crosssection Server Functions
#'
#' @noRd
mod_expl_plot_crosssection_server <- function(id, r_val){
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

      # check if tab is open
      if (r_val$tab_plots == "Profil transversal") {
        # check if dgo is selected
        if (!is.null(r_val$swath_data_section)) {
          r_val_locals$section = cr_profile_main(data = r_val$swath_data_section,
                                                 axis_toponyme = r_val$axis_name)
        }

        else if (is.null(r_val$swath_data_section)){
          r_val_locals$section = cr_profile_empty()
        }
      }
    })
  })
}
