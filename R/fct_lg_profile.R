#' Create an empty longitudinal profile plot
#'
#' This function generates an empty longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package.
#'
#' @return An empty longitudinal profile plot with a specified title.
#'
#' @importFrom plotly plot_ly layout toWebGL
#'
#' @examples
#' # Create an empty longitudinal profile plot
#' empty_plot <- lg_profile_empty()
#' empty_plot
#'
#' @export
lg_profile_empty <- function() {
  temp <- data.frame()
  plot <- plot_ly(data = temp, source = "plot_pg") %>%
    layout(title = list(
      text = "Sélectionnez un cours d'eau sur la carte et une métrique pour afficher le graphique",
      y = 0.80,  # y title position
      x = 0.3,   # x title position
      font = list(size = 15)
    )) %>% toWebGL()
  return(plot)
}


#' Create a vertical dashed line annotation for longitudinal profile plots
#'
#' This function generates a vertical dashed line annotation for longitudinal profile
#' plots using the 'plotly' package.
#'
#' @param x The x-coordinate where the vertical line should be positioned.
#' @param color The color of the vertical dashed line (default is "green").
#'
#' @return A list object representing a vertical dashed line annotation.
#'
#' @examples
#' # see lg_profile_main() function to use it in a plotly graph
#' # Create a vertical dashed line annotation at x = 10 with a red color
#' vertical_line_annotation <- lg_vertical_line(x = 10, color = "red")
#'
#' @export
lg_vertical_line <- function(x = 0, color = "green") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

#' Create a longitudinal profile plot for selected axis data
#'
#' This function generates a longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package. It allows you to visualize a specific
#' metric along the selected axis.
#'
#' @param data A data frame containing the selected axis data.
#' @param y The metric to be plotted on the y-axis.
#' @param y_label The name of the metric plotted.
#' @param y_label_category The metric category name.
#'
#' @return A longitudinal profile plot with the specified metric.
#'
#' @importFrom plotly plot_ly layout
#'
#' @examples
#' # Create a longitudinal profile plot for active channel width
#' selected_axis_df <- as.data.frame(network_dgo)
# profile_plot <- lg_profile_main(data = selected_axis_df, y = "active_channel_width",
#                                  y_label = "Chenal actif",
#                                  y_label_category = "Largeurs")
# profile_plot
#'
#' @export
lg_profile_main <- function(data, y, y_label, y_label_category) {

  plot <- plot_ly(x = data$measure, y = y, yaxis = 'y1',
                  key = data$fid,  # the "id" column for hover text
                  type = 'scatter', mode = 'lines', name = y_label) %>%
    layout(
      xaxis = list(title = 'Distance depuis l\'exutoire (km)'),
      yaxis = list(
        title = paste0(y_label_category, " - ", y_label),
        side = 'left'
      ),
      showlegend=TRUE,
      legend = list(orientation = 'h'),
      hovermode = "x unified",
      shapes = list(lg_vertical_line(2.5)),
      margin = list(
        t = 20,
        b = 10,
        l = 50,
        r = 80  # create space for the second y-axis title
      )
    )
  return(plot)
}

#' Generate list to update main plot in plotlyProxy
#'
#' This function generates a list to update the main axe in existing plotly graph with plotlyProxy.
#'
#' @param data A data frame containing the data to be plotted.
#' @param y The name of the y-axis variable to plot.
#' @param y_label The label for the y-axis.
#' @param y_label_category The category label for the y-axis.
#'
#' @return A list containing trace and layout lists to plot with plotlyProxy.
#'
#' @details
#' This function generates a main profile update plot with the specified data and axis labels.
# The plot is returned as a list containing both the trace and layout information.
# The trace contains x and y data for the plot, while the layout specifies the y-axis title.
#
#' @examples
#' \dontrun{
#' data <- data.frame(
#' measure = 1:10,
#' selected_metric = rnorm(10, mean = 1)
#' )
#' output$scatter_plot <- renderPlotly({
#'   plot_ly(data, x = ~measure, y = ~selected_metric, type = 'scatter', mode = 'line')
#' })
#' data_updated <- data.frame(
#'   measure = 1:10,
#'   selected_metric = rnorm(10, mean = 1)
#' )
#' selected_metric_name <-  "my metric"
#' select_metric_category <-  "my metric category"
#' update_main_axe <-
#'   lg_profile_update_main(
#'     data = data_updated,
#'     y = data_updated[[selected_metric]],
#'     y_label = selected_metric_name,
#'     y_label_category = r_val$select_metric_category
#'   )
#' plotlyProxy("scatter_plot") %>%
#'   plotlyProxyInvoke("deleteTraces", 0) %>%
#'   plotlyProxyInvoke("addTraces", update_main_axe$trace, 0) %>%
#'   plotlyProxyInvoke("relayout", update_main_axe$layout)
#' }
#'
#' @export
lg_profile_update_main <- function(data, y, y_label, y_label_category){
  proxy_trace <- list(
    x = data$measure,
    y = y,
    key = data$fid,  # the "id" column for hover text
    type = 'scatter',
    mode = 'lines',
    name = y_label,
    yaxis = 'y1'
  )

  proxy_layout <- list(
    yaxis = list(
      title = paste0(y_label_category, " - ", y_label),
      side = 'left'
    )
  )
  proxy <- list("trace" = proxy_trace,
                "layout" = proxy_layout)
  return(proxy)
}

#' Create a dual-axis longitudinal profile plot for selected axis data
#'
#' This function generates a dual-axis longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package. It allows you to visualize two different
#' metrics along the selected axis.
#'
#' @param data A data frame containing the selected axis data.
#' @param y The primary metric to be plotted on the left y-axis.
#' @param y_label The name of the metric plotted.
#' @param y_label_category The metric category name.
#'
#' @return A dual-axis longitudinal profile plot with the specified metrics.
#'
#' @examples
#' \dontrun{
#' # like lg_profile_update_main function, see example in documentation
#'}
#'
#' @export
lg_profile_second <- function(data, y, y_label, y_label_category){
  proxy_trace <- list(
    x = data$measure,
    y = y,
    key = data$fid,  # the "id" column for hover text
    type = 'scatter',
    mode = 'lines',
    name = y_label,
    yaxis = 'y2'
  )

  proxy_layout <- list(
    yaxis2 = list(
      title = list(text = paste0( y_label_category, " - ",
                                  y_label)
      ),
      overlaying = 'y',
      side = 'right',
      showgrid = FALSE,  # Hide the gridlines for the second y-axis
      showline = FALSE  # Hide the axis line for the second y-axis
    )
  )
  proxy <- list("trace" = proxy_trace,
                "layout" = proxy_layout)
  return(proxy)
}


