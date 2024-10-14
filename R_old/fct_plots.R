#' Combine classified regional network and axis network in one frame
#'
#' @param data_region sf-df with all dgos of selected region
#' @param data_axis sf-df with all dgos of selected axis
#' @param var variable for which classification was undertaken
#' @param classes binary stating whether networks contain classes which should be selected as well
#'
#' @importFrom dplyr mutate add_row select
#' @importFrom sf st_drop_geometry
#' @importFrom stats na.omit
#'
#' @return merged df with regional and axis dgos, identifiable by factor-variable "scale"
#'
#' @examples
#' merge_regional_axis_dfs(data_classified,
#'                         data_classified %>% filter(toponyme == "l'Isère"),
#'                         "forest_pc")
merge_regional_axis_dfs <- function(data_region, data_axis, var, classes = FALSE){

  # check if axis data exist
  if (is.null(data_axis)) {
    df <-
      data_region %>%
      mutate(scale = as.factor("Region")) %>%
      sf::st_drop_geometry()
  }
  else {
    df <-
      data_region %>%
      mutate(scale = as.factor("Region")) %>%
      add_row(
        data_axis %>%
          mutate(scale = as.factor("Axe fluvial"))
      ) %>%
      sf::st_drop_geometry()
  }


  if (classes == TRUE) {
    df <- df %>%
      select(fid, class_name, color, scale, {{var}}) %>%
      na.omit()
  } else {
    df <- df %>%
      select(fid, scale, {{var}}) %>%
      na.omit()
  }

  return(df)
}


#' create dataframe of color-classes and values
#'
#' @param data classified network with corresponding colors for each class
#'
#' @importFrom dplyr select
#' @importFrom tibble deframe
#' @importFrom sf st_drop_geometry
#'
#' @return color vector
#'
#' @examples
#' get_colors_char_df(network)
get_colors_char_df <- function(data){
  df <-
    data %>%
    sf::st_drop_geometry() %>% # remove geometry if sf-object
    dplyr::select(class_name, color) %>%
    unique() %>%
    tibble::deframe()

  return(df)
}


#' Create interactive stacked barplots of class-distribution for region and axis
#'
#' @param data classified network data with entries for regional and axis dgos
#' @param colors color vector
#'
#' @importFrom dplyr count group_by mutate ungroup
#' @importFrom plotly plot_ly layout event_register
#'
#' @return interactive stacked barplot with class distribution in % for each scale-group (region and axis)
#'
#' @examples
#' create_plotly_barplot(data_plots)
create_plotly_barplot <- function(data){

  # create color-vector
  colors <- get_colors_char_df(data)

  # create summary df
  data_plots_summarized <- data %>%
    count(class_name, scale) %>%
    group_by(scale) %>%
    mutate(share = round((n / sum(n) * 100), 2)) %>%
    ungroup()


  # Create the stacked bar plot
  plot <-
    plotly::plot_ly(data = data_plots_summarized,
                    x = ~scale,
                    y = ~share,
                    color = ~class_name,
                    colors = colors,
                    type = 'bar',
                    text = ~paste0("Classe ", class_name, ": ", share, " % \n (", n, " tronçons)"),
                    hoverinfo = 'text',
                    marker = list(line = list(color = 'white', width = 2)),
                    source = 'B'
    ) %>%
    plotly::layout(
      barmode = 'stack',
      bargap = 0.5,
      title = "Proportion de classes",
      xaxis = list(title = "", showgrid = F),
      yaxis = list(title = "Pourcentage", showgrid = T, showticklabels = T),
      showlegend = FALSE
    )

  return(plot)
}

#' Create interactive violinplots for a specific variable for region and axis
#'
#' @param data classified network data with entries for regional and axis dgos
#' @param var variable based on which the violinplots should be created
#'
#' @importFrom plotly plot_ly layout event_register
#' @importFrom stats as.formula
#'
#' @return plotly interactive violinplots for each scale-group (region and axis)
#'
#' @examples
#' violinplot_plotly <- create_plotly_violinplot(data_plots, "forest_pc")
create_plotly_violinplot <- function(data, var, var_title){

  plot <- plotly::plot_ly(data = data,
                          x = ~scale,
                          y = as.formula(paste0("~`", var, "`")),
                          type = 'violin',
                          meanline = list(visible = TRUE),
                          points = 'all',
                          jitter = 0.1,
                          color = I("black"),
                          alpha = 0.1,
                          scalemode = 'width',
                          marker = list(size = 1, color = "black"),
                          spanmode = "hard",
                          hoverinfo = 'y',
                          source = 'V') %>%
    plotly::layout(
      xaxis = list(title = "", showgrid = FALSE,
                   categoryorder = "array", categoryarray = c('Région', 'Axe')),
      yaxis = list(title = var_title, side = 'left'),
      showlegend = FALSE
    )

  return(plot)
}


#' Create a Biplot with Linear Regression and Statistical Annotations
#'
#' This function creates a biplot using the `echarts4r` library that displays a scatterplot
#' of two metrics from a given dataframe, fits a linear regression line, and shows key
#' statistical measures (correlation, R², p-value) in a floating text box.
#'
#' @param df A data frame containing the metrics to be plotted.
#' @param metric_x A string specifying the name of the x-axis metric (column in the dataframe).
#' @param metric_y A string specifying the name of the y-axis metric (column in the dataframe).
#'
#' @importFrom echarts4r e_charts_ e_scatter_ e_line e_axis_labels e_x_axis e_y_axis e_legend e_tooltip e_text_g e_toolbox_feature e_show_loading e_visual_map_
#' @return An `echarts4r` plot object showing the biplot, regression line, and statistical annotations.
#' @examples
#' create_analysis_biplot_echarts(df = mtcars, metric_x = "mpg", metric_y = "wt")
#' @export
create_analysis_biplot_echarts <- function(df, metric_x, metric_y, classes = FALSE, lm = FALSE) {

  browser()

  # Get metric titles
  metric_x_title <- globals$metrics_params |> filter(metric_name == metric_x) |> pull(metric_title)
  metric_y_title <- globals$metrics_params |> filter(metric_name == metric_y) |> pull(metric_title)

  # removing any rows with missing values (NA)
  data <- df %>% na.omit()

  if (lm) {
    # Compute linear regression
    lm_model <- lm(data[[metric_y]] ~ data[[metric_x]], data = data)
    data$lm <- predict(lm_model)

    # Compute correlation, R², and p-value
    correlation <- cor.test(data[[metric_x]], data[[metric_y]])
    r_value <- round(correlation$estimate, 2)
    p_value <- round(correlation$p.value, 4)
    r_squared <- round(r_value^2, 2)

    # Extract the coefficients for the linear model and create the equation
    coefficients <- coef(lm_model)
    intercept <- round(coefficients[1], 2)  # Intercept (b)
    slope <- round(coefficients[2], 2)      # Slope (m)

    # Format the formula as "y = mx + b"
    formula_text <- sprintf("y = %sx + %s", slope, intercept)

    linear_dependency_text <- sprintf("%s, R = %s, R² = %s, p-value = %s",
                                      formula_text, r_value, r_squared, p_value)
  }

  # Create the biplot using echarts4r with either z-metric selected or not
  if (classes) {

    # proposition by chatgpt
    plot <- data %>%
      group_by(class_name) %>%  # Group by color to create separate series
      e_charts_(metric_x) %>%  # Initialize the plot and specify the x-axis metric
      # Add scatter series for each color group
      e_scatter_(metric_y, bind = "measure", symbol_size = 6) %>%
      e_color(color = rev(unique(unname(data$color))))

  }

  else {
    plot <- data %>%
      e_charts_(metric_x) %>%  # Initialize the plot and specify the x-axis metric
      e_scatter_(metric_y, bind = "measure", symbol_size = 6, itemStyle = list(color = "#1b263b"), legend = FALSE)  # Add scatter plot with points
  }

  # Add axis labels and tooltips
  plot <- plot %>%
    e_axis_labels(x = metric_x_title, y = metric_y_title) %>%  # Set axis labels using metric titles
    e_x_axis(nameLocation = "middle", nameGap = 30) %>%  # Center the x-axis title and move it below the axis
    e_y_axis(nameLocation = "middle", nameGap = 50) %>%  # Center the y-axis title and move it to the left
    # Configure tooltips that display data points info on hover
    e_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
        sprintf("function(params) {
                return('<b>%s: </b>' + params.value[0].toFixed(2) + '<br/>' +
                '<b>%s: </b>' + params.value[1].toFixed(2) + '<br/>' +
                '<b>Position: </b>' + params.name + ' km'
                );
              }", metric_x_title, metric_y_title)
      )
    ) %>%
    #Add toolbox features (e.g., zooming, saving the plot)
    e_toolbox_feature(feature = c("dataZoom", "saveAsImage")) %>%
    # Show a loading animation while the plot is rendered
    e_show_loading()


  # add lm elements to plot
  if (lm) {
    plot <- plot %>%
      e_line(lm, name = "Modèle linéaire", lineStyle = list(color = "red"), symbol = 'none') %>%  # Add the linear regression line
      e_legend(show = TRUE, itemStyle = list(color = "transparent")) %>%  # Show legend (transparent)
      # Add a floating text box with the statistical summary (R, R², p-value)
      e_text_g(
        left = "10%",               # Position text horizontally
        top = "9%",                # Position text vertically
        style = list(
          text = linear_dependency_text,  # Display correlation text
          fontSize = 12,                  # Font size for the text
          z = 1000,                       # Set z-index to bring the text to the foreground
          backgroundColor = "#ffccd5",    # Set background color of the text box
          borderRadius = 5,               # Add rounded corners to the text box
          padding = 5                     # Add padding around the text within the box
        )
      )
  }



  return(plot)
}
