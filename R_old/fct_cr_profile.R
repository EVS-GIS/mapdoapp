#' Create an empty cross section plot.
#'
#' This function generates an empty cross section plot using the 'plot_ly'
#' function from the 'plotly' package.
#'
#' @return An empty longitudinal profile plot with a specified title.
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples
#' # Create an empty longitudinal profile plot
#' empty_plot <- lg_profile_empty()
#' empty_plot
#'
#' @export
cr_profile_empty <- function() {
  temp <- data.frame()
  plot <- plot_ly(data = temp, source = "plot_pg") %>%
    layout(
      title = list(
        text = "Sélectionnez un cours d'eau et un tronçon sur la carte pour afficher la section",
        y = 0.80,  # y title position
        x = 0.3,   # x title position
        font = list(size = 15)
      ),
      xaxis = list(
        zeroline = FALSE
      ),
      yaxis = list(
        zeroline = FALSE
      ))
  return(plot)
}

#' Create a cross section plot for selected DGO data.
#'
#' @param data data.frame elevation profiles from selected dgo.
#' @param axis_toponyme text axis toponyme name.
#'
#' @importFrom plotly plot_ly layout
#'
#' @return plotly cross section plot.
#' @export
cr_profile_main <- function(data, axis_toponyme){
  section <- plot_ly(data = data, x = ~distance, y = ~profile, type = 'scatter',
                     yaxis = 'y1', key = data$id, # the "id" column for hover text
                     mode = 'lines+markers', fill = 'tozeroy', fillcolor = '#B0B0B0',
                     line = list(color = '#2C2C2C'), marker = list(opacity=0),
                     name = "elevation",
                     source = "T") %>%
    layout(yaxis = list(title = "Elévation (m)",
                        range = c(min(data$profile, na.rm = TRUE), max(data$profile, na.rm = TRUE))),
           xaxis = list(title = "Distance au talweg (m)"),
           margin = list(l = 50, r = 30, t = 70, b = 70),
           title= "Profil transversal médian",
           hovermode = "x unified",
           annotations = list(
             list(
               text = "Rive gauche", x = 0, y = 1.1,
               xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(size = 14, weight = "bold")
             ),
             list(
               text = "Rive droite", x = 1, y = 1.1,
               xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(size = 14, weight = "bold")
             ),
             list(
               text = axis_toponyme, x = 1, y = -0.18,
               xref = "paper", yref = "paper", showarrow = FALSE,
               font = list(size = 14, weight = "bold")
             )
           )
    )
  return(section)
}
