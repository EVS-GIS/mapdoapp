#' Create an empty longitudinal profile plot.
#'
#' This function generates an empty longitudinal profile plot using the 'plot_ly'
#' function from the 'plotly' package.
#'
#' @return An empty longitudinal profile plot with a specified title.
#'
#' @importFrom plotly plot_ly layout config
#'
#' @examples
#' # Create an empty longitudinal profile plot
#' empty_plot <- lg_profile_empty()
#' empty_plot
#'
#' @export
lg_profile_empty <- function() {
  temp <- data.frame()
  plot <- plot_ly(data = temp, source = "L", type = 'scatter', mode = 'lines') %>%
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
      ),
      modebar = list(remove = c("select2d", "lasso2d", "autoscale", "zoomIn2d", "zoomOut2d"))) %>% # Remove select and lasso tools
    plotly::config(displaylogo = FALSE) %>% # Remove plotly logo) %>%
    event_register('plotly_hover')

  return(plot)
}

#' Add first metric for longitudinal profile plot for selected axis data
#'
#' This function adds the first metric trace to the plot
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
lg_profile_first <- function(data, y, y_label, y_label_category){

  proxy_trace <- lg_add_trace(data, y, y_label, yaxis = 'y1')

  proxy_layout <- list(
    xaxis = lg_xaxis_layout(data),
    yaxis = lg_yaxis_layout(y_label_category, y_label),
    # river name
    annotations = lg_annotations_layout(data),
    showlegend = TRUE,
    legend = list(orientation = 'h'),
    hovermode = "x unified",
    # shapes = list(shapes = NULL),
    margin = list(t = 20, b = 10, l = 50, r = 80),  # create space for the second y-axis title
    title = NULL
  )
  proxy <- list("trace" = proxy_trace,
                "layout" = proxy_layout)
  return(proxy)
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

  proxy_trace <- lg_add_trace(data, y, y_label, yaxis = 'y2', color = "#7209b7")

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


#' plotly add trace.
#'
#' @param data data frame containing the selected axis data.
#' @param y text metric to be plotted on the y-axis.
#' @param y_label text name of the metric plotted.
#' @param yaxis text axis id.
#'
#' @return list
#' @export
lg_add_trace <- function(data, y, y_label, yaxis = 'y1', color = "#22223b"){
  trace <- list(
    x = data$measure,
    y = y,
    key = data$fid,  # the "id" column for hover text
    type = 'scatter',
    mode = 'lines',
    line = list(color = color),
    name = y_label,
    yaxis = yaxis
  )
  return(trace)
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
    lg_vertical_line(x = x, color = "#323232")
  })
  return(shapes_list)
}

#' Assign classification with colors to network dgos
#'
#' @param data dataframe or sf object containing all dgos of an axis
#' @param proposed_class string indicating the type of classification which should be applied to the data
#'
#' @return classified dataframe/sf object with additional variables: class_name and color
#'
#' @importFrom dplyr mutate case_when left_join join_by rowwise ungroup c_across select
#' @importFrom tidyr replace_na
#' @importFrom sf st_drop_geometry
#'
#' @examples
#' classified_network <- network_dgo %>%
#'     assign_classes(proposed_class = "class_strahler",
#'     colors_df = globals$classes_proposed_colors)
#'
assign_classes_proposed <- function(data, proposed_class, colors_df) {

  # Extract the relevant color mapping for the main_class
  color_mapping <- colors_df[[proposed_class]]

  # Add color column based on the class_name
  df <- data %>%
    sf::st_drop_geometry() %>%
    mutate(
      class_name = data[[proposed_class]],
      color = color_mapping[as.character(data[[proposed_class]])]) %>%
    replace_na(list(color = "#f8f8ff"))  # Default color for unvalid classes or NAs

  return(df)
}

#' Assign classes to network dgos
#'
#' @param data dataframe or sf object which contains dgos of axis or region
#' @param classes df containing columns variables, greater_thans, class_names, colors which define the classification of the network
#'
#' @return classified dataframe/sf object with additional variables: class_name and color
#' @importFrom rlang parse_exprs
#' @importFrom dplyr mutate case_when left_join join_by
#' @importFrom sf st_as_sf
#'
#' @examples
#' classified_network <- network_dgo %>%
#'     assign_classes(variables = as.character(r_val$grouping_table_data$variable),
#'     greater_thans = r_val$grouping_table_data$greaterthan,
#'     class_names = r_val$grouping_table_data$class)
#'
assign_classes_manual <- function(data, classes) {

  classes <- classes %>%
    arrange(desc(greaterthan))

  variables <- as.character(classes$variable)
  greater_thans <- classes$greaterthan
  class_names <- classes$class
  colors <- classes %>% select(class, color)

  df <-
    data %>%
    sf::st_drop_geometry() %>%
    mutate(
      class_name = case_when(
        !!!parse_exprs(paste0(variables, ' >= ', greater_thans, ' ~ "', class_names, '"')
        ))
    ) %>%
    left_join(colors, by = join_by(class_name == class)) %>%
    replace_na(list(color = "#f8f8ff"))  # Default color for unvalid classes or NAs

  return(df)
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
    color <- unique(classified_axis$color[i])

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


#' Smoothing of classified df
#'
#' @param df classified axis data
#' @param n_smooth integer number of values to smooth out
#'
#' @import dplyr
#'
#' @return df_smooth smoothed classified axis data
#'
smoothen_classes <- function(df, n_smooth) {

  # get colors for classes
  df_colors <- tibble(class_name = unique(df$class_name), color = unique(df$color))

  # 1 - smooth out one value if surrounded by group of at least 3 equal values
  if (n_smooth == 1) {

    df_smooth <- df %>%
      mutate(
        lag = lag(class_name),
        lead = lead(class_name),
        lag2 = lag(class_name, n = 2),
        lead2 = lead(class_name, n = 2),
        lag3 = lag(class_name, n = 3),
        lead3 = lead(class_name, n = 3),
        # condition1 - two equal before and one equal after one unequal (B) in middle, e.g. AABA -> AAAA
        condition1 = (lag != "unvalid") & (lag2 != "unvalid") & (lead != "unvalid") &
          (lag != class_name) &
          (lag == lead) &
          (lag2 == lag),
        # condition2 - one equal before and two equal after one unequal (B) in middle, e.g. CABAA -> CAAAA
        condition2 = (lag != "unvalid") & (lead != "unvalid") & (lead2 != "unvalid") &
          (lag != class_name) &
          (lag == lead) &
          (lead2 == lead) &
          !condition1,
        # condition3 - three equal before (A) and one or more unequal (C) after one unequal (B) in middle, e.g. AAABCC -> AAAACC
        condition3 = (lag != "unvalid") & (lag2 != "unvalid") & (lag3 != "unvalid") &
          (lag != class_name) &
          (lag != lead) &
          (lead != class_name) &
          (lag2 == lag) &
          (lag3 == lag) &
          !condition1 & !condition2,
        # condition4 - one or more unequal (C) before and three equal (A) after one unequal (B) in middle, e.g. CCBAAA -> CCAAAA
        condition4 = (lead != "unvalid") & (lead2 != "unvalid") & (lead3 != "unvalid") &
          (lag != class_name) &
          (lag != lead) &
          (lead != class_name) &
          (lead2 == lead) &
          (lead3 == lead) &
          !condition1 & !condition2 & !condition3,
      ) %>%
      mutate(
        group = case_when(condition1 ~ lag2,
                          condition2 ~ lead2,
                          condition3 ~ lag2,
                          condition4 ~ lead2,
                          .default = class_name)
      ) %>%
      select(-condition4)
  }


  # 2 - smooth out two unequal values if surrounded by group of at least 4 equal values
  else if (n_smooth == 2) {

    df_smooth <- df %>%
      mutate(
        lag = lag(class_name),
        lead = lead(class_name),
        lag2 = lag(class_name, n = 2),
        lead2 = lead(class_name, n = 2),
        lag3 = lag(class_name, n = 3),
        lead3 = lead(class_name, n = 3),
        lag4 = lag(class_name, n = 4),
        lead4 = lead(class_name, n = 4),
        # condition1 - 3 equal (A) before and 1 equal after two unequal (B, C) in middle, e.g. AAABCA -> AAAAAA
        condition1 = (lag != "unvalid") & (lag2 != "unvalid") & (lead2 != "unvalid") & (lag3 != "unvalid") &
          (lag != class_name) &
          (lag == lag2) &
          (lag2 == lag3) &
          (lag == lead2),
        condition1_2 = (lag2 != "unvalid") & (lag3 != "unvalid") & (lag4 != "unvalid") & (lead != "unvalid") &
          (lag2 != class_name) &
          (lag2 == lag3) &
          (lag3 == lag4) &
          (lag2 == lead) & !condition1,
        # condition2 - 1 equal (A) before and 3 equal after two unequal (B, C) in middle, e.g. CABCAAA -> CAAAAAA
        condition2 = (lag != "unvalid") & (lead2 != "unvalid") & (lead3 != "unvalid") & (lead4 != "unvalid") &
          (lag != class_name) &
          (lead2 == lag) &
          (lead3 == lag) &
          (lead4 == lag) & !condition1 & !condition1_2,
        condition2_2 = (lag2 != "unvalid") & (lead != "unvalid") & (lead2 != "unvalid") & (lead3 != "unvalid") &
          (lag2 != class_name) &
          (lead == lag2) &
          (lead2 == lag2) &
          (lead3 == lag2) &
          !condition1 & !condition1_2 & !condition2,
        # condition3 - 2 equal (A) before and 2 equal after two unequal (B, C) in middle, e.g. CAABCAA -> CAAAAAA
        condition3 = (lag != "unvalid") & (lag2 != "unvalid") & (lead2 != "unvalid") & (lead3 != "unvalid") &
          (lag != class_name) &
          (lag2 == lag) &
          (lead2 == lag) &
          (lead3 == lag) &
          !condition1 & !condition2 & !condition1_2 & !condition2_2,
        condition3_2 = (lag2 != "unvalid") & (lag3 != "unvalid") & (lead != "unvalid") & (lead2 != "unvalid") &
          (lag2 != class_name) &
          (lag3 == lag2) &
          (lead == lag2) &
          (lead2 == lag2) &
          !condition1 & !condition2 & !condition1_2 & !condition2_2 & !condition3,
      ) %>%
      mutate(
        group = case_when(condition1 ~ lag2,
                          condition1_2 ~ lag2,
                          condition2 ~ lead2,
                          condition2_2 ~ lead2,
                          condition3 ~ lag2,
                          condition3_2 ~ lag2,
                          .default = class_name)
      ) %>%
      select(-lag4, -lead4, -condition1_2, -condition2_2, -condition3_2)
  }

  df_smooth <- df_smooth %>%
    select(-lag, -lag2, -lag3, -lead, -lead2, -lead3, -condition1, -condition2, -condition3, -color) %>%
    left_join(df_colors, by = join_by(group == class_name)) %>%
    select(-group)

  return(df_smooth)
}
