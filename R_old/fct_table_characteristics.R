# example for statistics-derival from database
# SELECT
# AVG(talweg_elevation_min) AS average,
# MIN(talweg_elevation_min) AS low_outlier,
# percentile_cont(0.025) WITHIN GROUP (ORDER BY talweg_elevation_min) AS low_whisker,
# percentile_cont(0.25) WITHIN GROUP (ORDER BY talweg_elevation_min) AS q1,
# percentile_cont(0.5) WITHIN GROUP (ORDER BY talweg_elevation_min) AS median,
# percentile_cont(0.75) WITHIN GROUP (ORDER BY talweg_elevation_min) AS q2,
# percentile_cont(0.975) WITHIN GROUP (ORDER BY talweg_elevation_min) AS high_whisker,
# MAX(talweg_elevation_min) AS high_outlier
# FROM network_metrics



#' convert sf to pivoted dataframe with variables as rows
#'
#' @import dplyr
#' @importFrom sf st_drop_geometry
#'
#' @param df_sf sf object with one or several dgos
#'
#' @return dataframe with metrics as rows
#' @export
#'
fct_table_pivot_sf <- function(df_sf) {
  df <- df_sf %>%
    sf::st_drop_geometry() %>%
    na.omit() %>%
    tidyr::pivot_longer(-c(fid, axis, measure, toponyme, strahler, gid_region), names_to = "metric_name", values_to = "value") %>%
    dplyr::mutate(value = round(value, 2)) %>%
    dplyr::group_by(metric_name) %>%
    dplyr::summarize(
      mean = round(mean(value), 2),
      distr = list(value)
    )

  return(df)
}

#' Merge regional, axis, and dgo dataframes together for reactable table
#'
#' @import dplyr
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr pivot_longer
#'
#' @param region_pivot pivoted df from selected regional network
#' @param axis_pivot pivoted df from selected axis network
#' @param dgo_pivot pivoted df from selected dgo
#'
#' @return dataframe with metrics as rows
#' @export
#'
#' @examples
#' fct_table_create_table_df(network_dgo, network_dgo, network_dgo[1,])
fct_table_create_table_df <- function(region_pivot, axis_pivot, dgo_pivot){

  # region selected, but axis and dgo not
  if (is.null(dgo_pivot) && is.null(axis_pivot)) {

    # merge datasets
    merged <-
      dplyr::left_join(params_metrics() %>% select(metric_name, metric_title), region_pivot,
                       by = join_by(metric_name)) %>%
      dplyr::mutate(mean_axis = "", distr_axis = "", segment = "") %>%
      dplyr::select(!metric_name) %>%
      dplyr::relocate(metric_title, mean_region, distr_region, mean_axis, distr_axis, segment) %>%
      dplyr::group_by(metric_title)
  }

  # region and axis selected, but dgo not
  else if (is.null(dgo_pivot) && !is.null(axis_pivot)) {

    # merge datasets
    merged <-
      dplyr::left_join(params_metrics() %>% select(metric_name, metric_title), region_pivot,
                       by = join_by(metric_name)) %>%
      dplyr::left_join(axis_pivot,
                       by = join_by(metric_name)) %>%
      dplyr::mutate(segment = "") %>%
      dplyr::select(!metric_name) %>%
      dplyr::relocate(metric_title, mean_region, distr_region, mean_axis, distr_axis, segment) %>%
      dplyr::group_by(metric_title)
  }
  # region, axis, and dgo selected
  else {

    # merge datasets
    merged <-
      dplyr::left_join(params_metrics() %>% select(metric_name, metric_title), region_pivot,
                       by = join_by(metric_name)) %>%
      dplyr::left_join(axis_pivot,
                       by = join_by(metric_name),
                       suffix = c("_region", "_axis")) %>%
      dplyr::left_join(dgo_pivot,
                       by = join_by(metric_name)) %>%
      dplyr::select(!metric_name) %>%
      dplyr::relocate(metric_title, mean_region, distr_region, mean_axis, distr_axis, segment) %>%
      dplyr::group_by(metric_title)
  }

  return(merged)
}


#' create a table to compare metric-characteristics of region, axis, and segment
#'
#' @import dplyr
#' @importFrom reactable reactable colDef
#' @importFrom sparkline sparkline
#' @importFrom tibble tibble
#'
#' @param df dataframe of merged regional, axis and dgo data
#' @param unit
#'
#' @return reactable table object
#' fct_table_create_table_df(region_sf = network_dgo,
#'  axis_sf = network_dgo,
#'  dgo_sf = NULL)
#'
fct_table_create_reactable <- function(df, unit, details = FALSE){

  # filter for ha or % units
  if (unit == "ha") {
    data <- df %>%
      dplyr::filter(metric_title %in% grep('(%)', metric_title, value = TRUE, invert = TRUE) | metric_title %in% c("Pente talweg (%)", "Pente fond de vallée (%)"))
  } else if (unit == "%") {
    data <- df %>%
      dplyr::filter(!grepl('(ha)', metric_title))
  }


  # convert data table and extract only values for a selected variable to create violinplots with plotly
  convert_data <- function(data, i){
    data_converted <- tibble::tibble(
      scale = "Axe",
      values = data$distr_axis[[i]]) %>%
      dplyr::add_row(
        tibble::tibble(
          scale = "Région",
          values = data$distr_region[[i]]
        )
      ) %>%
      dplyr::rename(!!data$metric_title[i] := values)

    return(data_converted)
  }

  if (details == TRUE) {
    # create reactable table with violinplots in details
    table <- reactable(
      data = data,
      columns = list(
        metric_title = colDef(name = "Métrique", width = 180),
        mean_region = colDef(name = "Région", width = 100),
        distr_region = colDef(name = "", width = 80,
                              cell = function(value, index) {
                                sparkline(data$distr_region[[index]], type = "box")
                              }),
        mean_axis = colDef(name = "Axe", width = 100),
        distr_axis = colDef(name = "", width = 80,
                            cell = function(value, index) {
                              sparkline(data$distr_axis[[index]], type = "box")
                            }),
        segment = colDef(name = "Tronçon", width = 80)
      ),
      # add violinplots of distribution from axis and region into details
      details = function(index) {
        htmltools::div(
          style = "padding: 10px; margin-left: 40px; white-space: pre-wrap;",  # Add text indentation
          create_plotly_violinplot(convert_data(data, index), data$metric_title[index], data$metric_title[index])
        )},
      height = 420,
      defaultPageSize = 9,
      highlight = TRUE, # highlight rows on hover
      compact = TRUE
    )
  } else {

    # create reactable table without violinplots
    table <- reactable(
      data = data,
      columns = list(
        metric_title = colDef(name = "Métrique", width = 150),
        mean_region = colDef(name = "Région", width = 80),
        distr_region = colDef(name = "", width = 70,
                              cell = function(value, index) {
                                sparkline(data$distr_region[[index]], type = "box")
                              }),
        mean_axis = colDef(name = "Axe", width = 80),
        distr_axis = colDef(name = "", width = 70,
                            cell = function(value, index) {
                              sparkline(data$distr_axis[[index]], type = "box")
                            }),
        segment = colDef(name = "Tronçon", width = 80)
      ),
      height = 400,
      pagination = FALSE,
      outlined = TRUE,
      highlight = TRUE, # highlight rows on hover
      compact = TRUE
    )

  }

  return(table)
}















#' create a table to compare characteristics of region and segment
#'
#' @description A fct function
#'
#' @import dplyr
#' @importFrom reactable reactable colDef
#' @importFrom sparkline sparkline
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr pivot_longer
#' #'
#' #' @return The return value, if any, from executing the function.
#' #'
#' #' @noRd
#' create_reactable_characteristics <- function(region_network, dgo){
#'
#'   # get data
#'   region_df <- region_network %>% sf::st_drop_geometry()
#'   # axis_df <- axis_network %>% sf::st_drop_geometry()
#'   dgo_df <- dgo %>% sf::st_drop_geometry()
#'
#'   # convert data
#'   network_pivot <- region_df %>%
#'     tidyr::pivot_longer(-c(fid, axis, measure, toponyme, strahler, gid_region),
#'                         names_to = "variable", values_to = "value") %>%
#'     dplyr::mutate(value = round(value, 2)) %>%
#'     dplyr::group_by(variable) %>%
#'     dplyr::summarize(
#'       region_mean = mean(value),
#'       region_distr = list(value)
#'     )
#'
#'   dgo_pivot <- dgo_df %>%
#'     dplyr::select(-c(fid, axis, measure, toponyme, strahler, gid_region)) %>%
#'     tidyr::pivot_longer(-c(), names_to = "variable", values_to = "segment") %>%
#'     dplyr::mutate(segment = round(segment, 2))
#'
#'   # merge datasets
#'   merged <- left_join(network_pivot, dgo_pivot, by = join_by(variable)) %>%
#'     dplyr::left_join(params_metrics() %>% select(metric_name, metric_title, metric_type_title),
#'                      by = dplyr::join_by(variable == metric_name)) %>%
#'     dplyr::select(!variable) %>%
#'     dplyr::relocate(metric_type_title, metric_title, region_mean, region_distr, segment) %>%
#'     # na.omit() %>%
#'     dplyr::arrange(factor(metric_type_title, levels = unique(params_metrics()$metric_type_title))) %>%
#'     dplyr::select(!metric_type_title) %>%
#'     dplyr::group_by(metric_title)
#'
#'   # create table
#'   table <- reactable(
#'     data = merged,
#'     columns = list(
#'       # metric_type_title = colDef(name = "Groupe"),
#'       metric_title = colDef(name = "Métrique"),
#'       region_mean = colDef(
#'         name = "Région (moyenne)",
#'         style = function(value, index) {
#'           reactable_style_function(value, merged$segment[index])
#'         }
#'         ),
#'       region_distr = colDef(
#'         name = "Distribution",
#'         cell = function(value, index) {
#'           sparkline(merged$region_distr[[index]], type = "box")
#'         }),
#'       segment = colDef(
#'         name = "Tronçon",
#'         style = function(value, index) {
#'           reactable_style_function(value, merged$region_mean[index])
#'         })
#'     ),
#'     # groupBy = "metric_type_title",
#'     # paginateSubRows = TRUE,
#'     defaultExpanded = TRUE, # categories expanded rather than closed
#'     defaultPageSize = 10, # set rows per page
#'     highlight = TRUE, # highlight rows on hover
#'     compact = TRUE
#'   )
#'
#'   return(table)
#' }



#' #' Helper function to style reactable table
#' #'
#' #' @param value actual value which should be coloured
#' #' @param other_value value to which the actual value should be compared
#' #'
#' #' @return styling for reactable row
#' #' @export
#' #'
#' #' @examples
#' #' # inside reactable function:
#' #' colDef(
#' #' name = "Tronçon",
#' #' style = function(value, index) {
#' #'   reactable_style_function(value, merged$region_mean[index])
#' #' })
#' reactable_style_function <- function(value, other_value) {
#'   if (is.na(value) || is.na(other_value)) {
#'     color <- "#212529"  # Default color for NA values
#'   } else if (value > other_value) {
#'     color <- "#283618"
#'   } else if (value < other_value) {
#'     color <- "#780000"
#'   } else {
#'     color <- "#212529"
#'   }
#'   list(color = color, fontWeight = "bold")
#' }
