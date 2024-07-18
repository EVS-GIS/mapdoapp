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
