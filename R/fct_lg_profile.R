#' Create an empty longitudinal profile plot.
#'
#' This function generates an empty longitudinal profile plot using the 'plot_ly'
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
lg_profile_empty <- function() {
  temp <- data.frame()
  plot <- plot_ly(data = temp, source = "plot_pg") %>%
    layout(
      title = list(
        text = "Sélectionnez un cours d'eau sur la carte pour afficher le graphique",
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
lg_vertical_line <- function(x = 0, color = "purple") {
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

#' Create the ROE vertical lines for plotly shapes
#'
#' @param roe_distance_axis vector ROE distance on axis.
#'
#' @return list of each vertical lines
#' @export
lg_roe_vertical_line <- function(roe_distance_axis){
  shapes_list <- lapply(roe_distance_axis, function(x) {
    lg_vertical_line(x = x/1000, color = "#323232")
  })
  return (shapes_list)
}

#' plotly xaxis layout.
#'
#' @param data data.frame dgo from axis.
#'
#' @return list
#' @export
lg_xaxis_layout <- function(data){
  xaxis <- list(
    title = 'Distance depuis l\'exutoire (km)',
    range = c(0, max(data$measure)),
    zeroline = FALSE)
  return(xaxis)
}

#' plotly yaxis layout.
#'
#' @param y_label text name of the metric plotted.
#' @param y_label_category text metric category name.
#'
#' @return list
#' @export
lg_yaxis_layout <- function(y_label_category, y_label){
  yaxis <- list(
    title = paste0(y_label_category, " - ", y_label),
    side = 'left',
    zeroline = FALSE
  )
  return(yaxis)
}

#' plotly annotations layout.
#'
#' @param data data.frame dgo from axis.
#'
#' @return list
#' @export
lg_annotations_layout <- function(data){
  annotations = list(
    text = unique(data$toponyme),
    x = 1,  # x-coordinate (0 to 1, where 0 is left and 1 is right)
    y = -0.18,  # y-coordinate (0 to 1, where 0 is bottom and 1 is top)
    xref = "paper",  # "paper" to specify coordinates relative to the entire plot
    yref = "paper",
    showarrow = FALSE,  # Don't show the arrow
    font = list(
      # family = "Open Sans",
      size = 14,
      # color = "black"
      weight = "bold"
    )
  )
  return(annotations)
}

#' plotly add trace.
#'
#' @param data data frame containing the selected axis data.
#' @param y text metric to be plotted on the y-axis.
#' @param y_label text name of the metric plotted.
#' @param yaxis text axis id.
#'
#' @return list
#' @export
lg_add_trace <- function(data, y, y_label, yaxis = 'y1'){
  trace <- list(
    x = data$measure,
    y = y,
    key = data$fid,  # the "id" column for hover text
    type = 'scatter',
    mode = 'lines',
    line = list(color = "#7209b7"),
    name = y_label,
    yaxis = yaxis
  )
  return(trace)
}

#' Create a longitudinal profile plot for selected axis data.
#'
#' This function generates a longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package. It allows you to visualize a specific
#' metric along the selected axis.
#'
#' @param data data frame containing the selected axis data.
#' @param y text metric to be plotted on the y-axis.
#' @param y_label text name of the metric plotted.
#' @param y_label_category text metric category name.
#'
#' @return plotly A longitudinal profile plot with the specified metric.
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
                  type = 'scatter', mode = 'lines', name = y_label,
                  source = 'L', line = list(color = "#22223b")) %>%
    layout(
      xaxis = lg_xaxis_layout(data),
      yaxis = lg_yaxis_layout(y_label_category, y_label),
      # river name
      annotations = lg_annotations_layout(data),
      showlegend = TRUE,
      legend = list(orientation = 'h'),
      hovermode = "x unified",
      # shapes = list(shapes = NULL),
      margin = list(t = 20, b = 10, l = 50, r = 80)  # create space for the second y-axis title
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

  proxy_trace <- lg_add_trace(data, y, y_label, yaxis = 'y1')

  proxy_layout <- list(
    xaxis = lg_xaxis_layout(data),
    yaxis = lg_yaxis_layout(y_label_category, y_label),
    # put all the annotation options to replace the river name
    annotations = lg_annotations_layout(data)
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

  proxy_trace <- lg_add_trace(data, y, y_label, yaxis = 'y2')

  proxy_layout <- list(
    yaxis2 = list(
      title = list(text = paste0( y_label_category, " - ",
                                  y_label)
      ),
      overlaying = 'y',
      side = 'right',
      showgrid = FALSE,  # Hide the gridlines for the second y-axis
      zeroline = FALSE,
      showline = FALSE  # Hide the axis line for the second y-axis
    )
  )
  proxy <- list("trace" = proxy_trace,
                "layout" = proxy_layout)
  return(proxy)
}



#' Create background-layout of  classes for longitudinal profile
#'
#' @param classified_axis
#'
#' @return list ready to be used for plotly::layout()-function
create_classes_background <- function(classified_axis) {

  # Create empty list to store shapes in
  shapes <- list()

  for (i in 1:(nrow(classified_axis) - 1)) {

    # set x-axis limits
    start <- classified_axis$measure[i]
    if (!is.na(classified_axis$measure[i + 1])) {
      end <- classified_axis$measure[i + 1]
    } else {
      end <- start + 200
    }

    # set color
    color <- classified_axis$color[i]

    # create unique shape for each step
    shapes <- append(shapes, list(
      list(
        type = "rect",
        fillcolor = color,
        line = list(color = color, width = 0),
        opacity = 0.4,
        x0 = start,
        x1 = end,
        xref = "x",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        layer = "below"
      )
    ))
  }

  return(shapes)
}
