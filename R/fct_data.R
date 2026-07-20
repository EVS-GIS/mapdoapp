

# START load --------------------------------------------------------------

#' Get Hydrographic Basins
#'
#' This function retrieves hydrographic basins.
#'
#' @param opacity list that contain numeric values clickable and not_clickable to inform the user the non available features.
#' @param con Connection to Postgresql database.
#'
#' @return sf data frame containing information about hydrographic basins.
#'
#' @examples
#' con <- db_con()
#' opacity = list(clickable = 0.01,
#'                not_clickable = 0.10)
#'
#' data <- data_get_basins(con = con, opacity = opacity)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr mutate if_else
#'
#' @export
data_get_basins <- function(con, opacity=list(clickable = 0.01, not_clickable = 0.10)) {

  query <- "SELECT * FROM bassin_hydrographique"

  data <- sf::st_read(dsn = con, query = query) %>%
    mutate(click = if_else(display == TRUE, TRUE, FALSE)) %>%
    mutate(opacity = if_else(display == TRUE, opacity$clickable, opacity$not_clickable))

  return(data)
}

#' Get hydrological regions
#'
#' This function retrieves all hydrological regions available on the server and converts them to sf
#'
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing hydrological regions.
#'
#' @examples
#' con <- db_con()
#' data <- data_get_regions(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_regions <- function(con, opacity=list(clickable = 0.01, not_clickable = 0.10)) {

  query <- "SELECT * FROM region_hydrographique"

  data <- sf::st_read(dsn = con, query = query) %>%
    mutate(click = if_else(display == TRUE, TRUE, FALSE)) %>%
    mutate(opacity = if_else(display == TRUE, opacity$clickable, opacity$not_clickable))

  return(data)
}

#' Get Network Axes Data
#'
#' This function retrieves data about the network axes available on the server
#'
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing the network axes.
#'
#' @examples
#' con <- db_con()
#' axis_data <- data_get_axes(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_axes <- function(con) {
  query <- "SELECT network_axis.fid, axis, toponyme, gid_region, network_axis.geom FROM network_axis"

  data <- sf::st_read(dsn = con, query = query)
  return(data)
}

#' Get Referentiel des Obstacles aux Ecoulement Data
#'
#' This function retrieves the datapoints of the Referentiel des Obstacles aux Ecoulement (ROE).
#'
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing the ROE datapoints.
#'
#' @examples
#' con <- db_con()
#' roe_data <- data_get_roe_sites(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_roe_sites <- function(con) {
  query <- "
      SELECT roe.gid, axis, distance_axis, nomprincip, lbtypeouvr, lbhautchut, gid_region, roe.geom
      FROM roe
      WHERE (roe.cdetouvrag LIKE '2') AND (roe.stobstecou LIKE 'Validé')"

  data <- sf::st_read(dsn = con, query = query)

  return(data)
}


#' Get Stations Carhyce
#'
#' This function retrieves the datapoints of the CarHyCe hydrological stations.
#'
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing the CarHyCe datapoints.
#'
#' @examples
#' con <- db_con()
#' carhyce_stations <- data_get_carhyce_stations(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_carhyce_stations <- function(con) {
  query <- "
      SELECT code_station, name_station, geometry
      FROM carhyce_stations"

  data <- sf::st_read(dsn = con, query = query)

  return(data)
}


#' Get hydrometric sites.
#'
#' This function retrieves the locations of the hydrometric sites from Hubeau.
#'
#' @param con Connection to Postgresql database.
#'
#' @return sf data frame containing the datapoints of the hydrometric sites available on the server
#'
#' @examples
#' con <- db_con()
#' hydro_sites <- data_get_hydro_sites(con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_hydro_sites <- function(con){

  query <- "
          SELECT code_site, libelle_site, url_site, geom
          FROM hydro_sites"

  data <- sf::st_read(dsn = con, query = query)

  return(data)
}


#' This function retrieves the aggregated metrics for the whole network.
#'
#' @param con Connection to Postgresql database.
#' @param filter_by_basin_id if not NULL, filter the data by the specified basin id (cdbh)
#' @param filter_by_region_id if not NULL, filter the data by the specified region id (gid)
#' @param axis_id if not NULL, add a column "selected" to the data, with TRUE for the datapoints of the specified axis and FALSE for the others
#' @return sf data frame containing the datapoints of the homogenous segments' metrics
#'
#' @examples
#' con <- db_con()
#' data_get_metrics(con, "01","12","2000810055")
#' DBI::dbDisconnect(con)
#'
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_metrics=function(con,
                          filter_by_basin_id=NULL,
                          filter_by_region_id=NULL,
                          axis_id=NULL){
  query <-"
          SELECT *
          FROM network_metrics_aggregated"
  data_axes_metrics= DBI::dbGetQuery(conn = con, statement = query) %>%
    sf::st_drop_geometry() %>%
    select(-geom)
  query = "SELECT gid, cdbh FROM region_hydrographique"
  data_regions=DBI::dbGetQuery(conn = con, statement=query)
  data=data_axes_metrics %>%
    left_join(data_regions, by=c("gid_region"="gid"))
  if(!is.null(filter_by_basin_id)){
    data=data %>%
      filter(cdbh==filter_by_basin_id)
  }
  if(!is.null(filter_by_region_id)){
    data=data %>%
      filter(gid_region==filter_by_region_id)
  }
  if(!is.null(axis_id)){
    data=data %>%
      mutate(selected=case_when(axis==axis_id~TRUE,
                                TRUE~ FALSE))
  }
  return(data)
}


#' Get statistics on network metrics for different levels (france, basin, region)
#'
#' @param con Connection to Postgresql database.
#'
#' @import dplyr
#' @importFrom DBI dbGetQuery
#' @importFrom purrr pmap
#'
#' @return Dataframe which contains the statistics for all metrics for different entities: France, Basins, Regions
data_get_stats_metrics <- function(con) {
  data(metric_info)
  variables <- metric_info$metric_name
  query_stats <-
    paste0(
      paste(
        lapply(variables, function(var) {
          paste0(
            "  AVG(", var, ") AS ", var, "_avg,\n",
            "  MIN(", var, ") AS ", var, "_min,\n",
            "  percentile_cont(0.025) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_0025,\n",
            "  percentile_cont(0.25) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_025,\n",
            "  percentile_cont(0.5) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_05,\n",
            "  percentile_cont(0.75) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_075,\n",
            "  percentile_cont(0.975) WITHIN GROUP (ORDER BY ", var, ") AS ", var, "_0975,\n",
            "  MAX(", var, ") AS ", var, "_max"
          )
        }),
        collapse = ",\n"
      ),
      "\nFROM network_metrics_aggregated AS network_metrics\n"
    )

  # Constructing the SQL query
  query <- paste0(
    "SELECT\n",
    "'France (total)' AS level_type,\n",
    "'France' AS level_name,\n",
    "0 AS strahler, \n",
    query_stats,
    "WHERE network_metrics.gid_region IS NOT NULL\n",

    "\nUNION ALL\n",

    "SELECT\n",
    "'France' AS level_type,\n",
    "'France' AS level_name,\n",
    "network_metrics.strahler AS strahler,\n",
    query_stats,
    "WHERE network_metrics.gid_region IS NOT NULL\n",
    "GROUP BY network_metrics.strahler\n",

    "\nUNION ALL\n",

    # Basins
    "SELECT\n",
    "'Basin (total)' AS level_type,\n",
    "region_hydrographique.cdbh AS level_name,\n",
    "0 AS strahler,\n",
    query_stats,
    "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
    "WHERE network_metrics.gid_region IS NOT NULL\n",
    "GROUP BY region_hydrographique.cdbh\n",

    "\nUNION ALL\n",

    "SELECT\n",
    "'Basin' AS level_type,\n",
    "region_hydrographique.cdbh AS level_name,\n",
    "network_metrics.strahler AS strahler,\n",
    query_stats,
    "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
    "WHERE network_metrics.gid_region IS NOT NULL\n",
    "GROUP BY region_hydrographique.cdbh, network_metrics.strahler\n",

    "\nUNION ALL\n",

    # Regions
    "SELECT\n",
    "'Région (total)' AS level_type,\n",
    "CAST(network_metrics.gid_region as varchar(10)) AS level_name,\n",
    "0 AS strahler,\n",
    query_stats,
    "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
    "WHERE network_metrics.gid_region IS NOT NULL\n",
    "GROUP BY network_metrics.gid_region\n",

    "\nUNION ALL\n",

    "SELECT\n",
    "'Région' AS level_type,\n",
    "CAST(network_metrics.gid_region as varchar(10)) AS level_name,\n",
    "network_metrics.strahler AS strahler,\n",
    query_stats,
    "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
    "WHERE network_metrics.gid_region IS NOT NULL\n",
    "GROUP BY network_metrics.gid_region, network_metrics.strahler"
  )

  suffixes <- c("_min", "_0025", "_025", "_05", "_075", "_0975", "_max")

  data <- DBI::dbGetQuery(conn = con, statement = query) %>%
    na.omit() %>%
    mutate(across(ends_with("_avg"),
                  .fns = list(distr = function(x) {
                    suffix_base <- sub("_avg$", "", cur_column())
                    suffix_cols <- paste0(suffix_base, suffixes)
                    pmap(select(cur_data(), all_of(suffix_cols)), ~ as.numeric(round(c(...), 2)))  # Strip names and return as numeric
                  }),
                  .names = "{.col}_distr")) %>%
    rename_with(~ sub("_avg_distr$", "_distr", .), ends_with("_avg_distr")) %>%
    mutate(across(ends_with("_avg"), ~ round(., 2)))

  return(data)
}


#' Get statistics on classes distributions for different levels (france, basin, region) and a specified class
#'
#' @param con Connection to Postgresql database.
#' @param class_name Name of the proposed class for which stats should be generated
#'
#' @importFrom dplyr case_when
#' @importFrom DBI dbGetQuery
#'
#' @return a dataframe which contains the statistics for the specified class for different entities: France, Basins, Regions available on the serve
#' @examples
#' con=db_con()
#' data_get_distr_class(con,"class_style")
data_get_distr_class <- function(con, class_name) {

  if (!is.null(class_name)) {

    classification_query <- case_when(
      class_name == "class_strahler" ~
        "CASE
          WHEN strahler IS NULL THEN 'unvalid'
          WHEN strahler = 1 THEN '1'
          WHEN strahler = 2 THEN '2'
          WHEN strahler = 3 THEN '3'
          WHEN strahler = 4 THEN '4'
          WHEN strahler = 5 THEN '5'
          WHEN strahler = 6 THEN '6'
          ELSE 'unvalid'
        END AS class_name",
      class_name == "class_topographie" ~
        "CASE
        WHEN talweg_elevation_min IS NULL OR talweg_slope IS NULL THEN 'unvalid'
        WHEN talweg_elevation_min >= 1000 AND talweg_slope >= 0.05 THEN 'Pentes de montagne'
        WHEN talweg_elevation_min >= 1000 AND talweg_slope < 0.05 THEN 'Plaines de montagne'
        WHEN talweg_elevation_min >= 300 AND talweg_slope >= 0.05 THEN 'Pentes de moyenne altitude'
        WHEN talweg_elevation_min >= 300 AND talweg_slope < 0.05 THEN 'Plaines de moyenne altitude'
        WHEN talweg_elevation_min >= -50 AND talweg_slope >= 0.05 THEN 'Pentes de basse altitude'
        WHEN talweg_elevation_min >= -50 AND talweg_slope < 0.05 THEN 'Plaines de basse altitude'
        ELSE 'unvalid'
      END AS class_name",
      class_name == "class_lu_dominante" ~
        "CASE
          WHEN forest_pc IS NULL OR grassland_pc IS NULL OR natural_open_pc IS NULL OR crops_pc IS NULL OR built_environment_pc IS NULL THEN 'unvalid'
          WHEN forest_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Forêt'
          WHEN grassland_pc + natural_open_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Prairies et sols nus'
          WHEN crops_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Cultures'
          WHEN built_environment_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Espace construit'
          ELSE 'unvalid'
        END AS class_name",
      class_name == "class_urban" ~
        "CASE
          WHEN built_environment_pc IS NULL THEN 'unvalid'
          WHEN built_environment_pc >= 70 THEN 'Fortement urbanisé'
          WHEN built_environment_pc >= 40 THEN 'Urbanisé'
          WHEN built_environment_pc >= 10 THEN 'Modérément urbanisé'
          WHEN built_environment_pc >= 0 THEN 'Presque pas/Pas urbanisé'
          ELSE 'unvalid'
        END AS class_name",
      class_name == "class_agriculture" ~
        "CASE
          WHEN crops_pc IS NULL THEN 'unvalid'
          WHEN crops_pc >= 70 THEN 'Très Forte'
          WHEN crops_pc >= 40 THEN 'Forte'
          WHEN crops_pc >= 10 THEN 'Modéré'
          WHEN crops_pc >= 0 THEN 'Basse/Absente'
          ELSE 'unvalid'
        END AS class_name",
      class_name == "class_nature" ~
        "CASE
          WHEN natural_open_pc IS NULL OR forest_pc IS NULL OR grassland_pc IS NULL THEN 'unvalid'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 70 THEN 'Très forte'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 40 THEN 'Forte'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 10 THEN 'Modérée'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 0 THEN 'Presque pas/Pas naturelle'
          ELSE 'unvalid'
        END AS class_name",
      class_name == "class_gravel" ~
        "CASE
          WHEN gravel_bars IS NULL OR water_channel IS NULL THEN 'unvalid'
          WHEN (gravel_bars / NULLIF(water_channel + gravel_bars, 0)) >= 0.5 THEN 'Fréquent'
          WHEN (gravel_bars / NULLIF(water_channel + gravel_bars, 0)) > 0 THEN 'Occasionnel'
          WHEN (gravel_bars / NULLIF(water_channel + gravel_bars, 0)) = 0 THEN 'Absent'
          ELSE 'unvalid'
        END AS class_name",
      class_name == "class_confinement" ~
        "CASE
          WHEN idx_confinement IS NULL THEN 'unvalid'
          WHEN idx_confinement >= 0.7 THEN 'Peu confiné'
          WHEN idx_confinement >= 0.4 THEN 'Modérement confiné'
          WHEN idx_confinement >= 0.1 THEN 'Confiné'
          WHEN idx_confinement >= 0 THEN 'Très confiné'
          ELSE 'unvalid'
        END AS class_name",
      class_name == "class_habitat" ~
        "CASE
          WHEN riparian_corridor_pc IS NULL OR semi_natural_pc IS NULL THEN 'unvalid'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 70 THEN 'Élevée'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 40 THEN 'Bonne'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 10 THEN 'Moyenne'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 0 THEN 'Faible/Absente'
          ELSE 'unvalid'
        END AS class_name",
      .default = NULL
    )

    query <- paste0(
      "SELECT\n",
      "'France (total)' AS level_type,\n",
      "'France' AS level_name,\n",
      "0 AS strahler, \n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'France (total)' AS level_type,\n",
      "'France' AS level_name,\n",
      "0 AS strahler, \n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY class_name",

      "\nUNION ALL\n",

      "SELECT\n",
      "'France' AS level_type,\n",
      "'France' AS level_name,\n",
      "strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'France' AS level_type,\n",
      "'France' AS level_name,\n",
      "network_metrics.strahler AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY strahler, class_name",

      "\nUNION ALL\n",

      # Basins
      "SELECT\n",
      "'Basin (total)' AS level_type,\n",
      "level_name,\n",
      "0 AS strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Basin (total)' AS level_type,\n",
      "region_hydrographique.cdbh AS level_name,\n",
      "0 AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, class_name\n",

      "\nUNION ALL\n",

      "SELECT\n",
      "'Basin' AS level_type,\n",
      "level_name,\n",
      "strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Basin' AS level_type,\n",
      "region_hydrographique.cdbh AS level_name,\n",
      "network_metrics.strahler AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, strahler, class_name\n",

      "\nUNION ALL\n",

      # Regions
      "SELECT\n",
      "'Région (total)' AS level_type,\n",
      "level_name,\n",
      "0 AS strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Région (total)' AS level_type,\n",
      "CAST(network_metrics.gid_region as varchar(10)) AS level_name,\n",
      "0 AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, class_name\n",

      "\nUNION ALL\n",

      "SELECT\n",
      "'Région' AS level_type,\n",
      "level_name,\n",
      "strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Région' AS level_type,\n",
      "CAST(network_metrics.gid_region as varchar(10)) AS level_name,\n",
      "network_metrics.strahler AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, strahler, class_name;\n"
    )

    data <- DBI::dbGetQuery(conn = con, statement = query) %>%
      na.omit()

    return(data)
  } else {
    return(NULL)
  }

}

#' Get statistics on classes distributions for different levels (france, basin, region) and manually specified classes
#'
#' @param con Connection to Postgresql database.
#' @param manual_classes_table Dataframe of manually defined classes, with 4 columns: variable, class, greaterthan, color
#'
#' @importFrom dplyr case_when
#' @importFrom DBI dbGetQuery
#'
#' @return
#' Dataframe which contains the statistics for the specified class for different entities: France, Basins, Regions available on the server
data_get_distr_class_man <- function(con, manual_classes_table) {

  if (!is.null(manual_classes_table)) {

    # get upper bounds for each class
    upper_bound <- c(manual_classes_table$greaterthan[-1], NA)

    # create classification query
    classification_query <- paste0(
      "CASE \n",
      paste0(if_else(!is.na(upper_bound),
                     paste0("WHEN ", manual_classes_table$variable, " >= ", manual_classes_table$greaterthan, " AND ",
                            manual_classes_table$variable, " < ", upper_bound, " THEN '", manual_classes_table$class, "'"),
                     paste0("WHEN ", manual_classes_table$variable, " >= ", manual_classes_table$greaterthan, " THEN '", manual_classes_table$class, "'")
      ),
      collapse = "\n"
      ),
      "\n ELSE 'unvalid'
      END AS class_name",
      collapse = "\n"
    )


    query <- paste0(
      "SELECT\n",
      "'France (total)' AS level_type,\n",
      "'France' AS level_name,\n",
      "0 AS strahler, \n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'France (total)' AS level_type,\n",
      "'France' AS level_name,\n",
      "0 AS strahler, \n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY class_name",

      "\nUNION ALL\n",

      "SELECT\n",
      "'France' AS level_type,\n",
      "'France' AS level_name,\n",
      "strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'France' AS level_type,\n",
      "'France' AS level_name,\n",
      "network_metrics.strahler AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY strahler, class_name",

      "\nUNION ALL\n",

      # Basins
      "SELECT\n",
      "'Basin (total)' AS level_type,\n",
      "level_name,\n",
      "0 AS strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Basin (total)' AS level_type,\n",
      "region_hydrographique.cdbh AS level_name,\n",
      "0 AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, class_name\n",

      "\nUNION ALL\n",

      "SELECT\n",
      "'Basin' AS level_type,\n",
      "level_name,\n",
      "strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Basin' AS level_type,\n",
      "region_hydrographique.cdbh AS level_name,\n",
      "network_metrics.strahler AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, strahler, class_name\n",

      "\nUNION ALL\n",

      # Regions
      "SELECT\n",
      "'Région (total)' AS level_type,\n",
      "level_name,\n",
      "0 AS strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Région (total)' AS level_type,\n",
      "CAST(network_metrics.gid_region as varchar(10)) AS level_name,\n",
      "0 AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, class_name\n",

      "\nUNION ALL\n",

      "SELECT\n",
      "'Région' AS level_type,\n",
      "level_name,\n",
      "strahler,\n",
      "class_name, \n",
      "COUNT(class_name) AS class_count\n",
      "FROM (\n",
      "SELECT\n",
      "'Région' AS level_type,\n",
      "CAST(network_metrics.gid_region as varchar(10)) AS level_name,\n",
      "network_metrics.strahler AS strahler,\n",
      classification_query, "\n",
      "FROM network_metrics\n",
      "LEFT JOIN region_hydrographique ON region_hydrographique.gid = network_metrics.gid_region\n",
      "WHERE network_metrics.gid_region IS NOT NULL\n",
      ") AS subquery\n",
      "GROUP BY level_name, strahler, class_name;\n"
    )

    data <- DBI::dbGetQuery(conn = con, statement = query) %>%
      na.omit()

    return(data)
  } else {
    return(NULL)
  }

}

# data_get_stats_classes_proposed <- function(con) {
#
# }
#
# data_get_stats_classes_manual <- function(con) {
#
# }



# names -------------------------------------------------------------------

#' Get the names of the hydrographic basins and regions
#'
#' @param con Connection to Postgresql database.
data_get_levels_names <- function(con) {

  query <- "SELECT DISTINCT *
            FROM region_hydrographique
            WHERE display = TRUE
  "

  data <- DBI::dbGetQuery(conn = con, statement = query)

  return(data)

}



# EVENT load --------------------------------------------------------------
"GROUP BY level_name, strahler, class_name;\n"


#' Get Network Metrics Data for a Specific Network Axis
#'
#' This function retrieves data about network metrics for a specific network axis based on its ID.
#'
#' @param selected_axis_id The ID of the selected network axis.
#' @param aggregated A boolean indicating whether to retrieve aggregated data (TRUE) or detailed data (FALSE). Default is FALSE.
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing information about network metrics for the specified network axis.
#'
#' @examples
#' con <- db_con()
#' network_metrics_data <- data_get_axis_dgos(selected_axis_id = 2000796122, con = con, aggregated=TRUE)
#' network_metrics_data <- data_get_axis_dgos(selected_axis_id = 2000796122, con = con, aggregated=FALSE)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr arrange
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_axis_dgos <- function(selected_axis_id, aggregated=FALSE, con) {
  data(metric_info)
  if (!is.null(selected_axis_id)) {
    if(aggregated==FALSE){
      sql <- paste0("SELECT network_metrics.fid, gid_region, axis, ids, measure, toponyme, strahler, geom,",
                    paste(metric_info$metric_name,collapse=", "), ", style AS class_style, ",
                    "-- Strahler Classification
        CASE
          WHEN strahler IS NULL THEN 'unvalid'
          WHEN strahler = 1 THEN '1'
          WHEN strahler = 2 THEN '2'
          WHEN strahler = 3 THEN '3'
          WHEN strahler = 4 THEN '4'
          WHEN strahler = 5 THEN '5'
          WHEN strahler = 6 THEN '6'
          ELSE 'unvalid'
        END AS class_strahler,

        -- Topography Classification
        CASE
          WHEN talweg_elevation_min IS NULL OR talweg_slope IS NULL THEN 'unvalid'
          WHEN talweg_elevation_min >= 1000 AND talweg_slope >= 0.05 THEN 'Pentes de montagne'
          WHEN talweg_elevation_min >= 1000 AND talweg_slope < 0.05 THEN 'Plaines de montagne'
          WHEN talweg_elevation_min >= 300 AND talweg_slope >= 0.05 THEN 'Pentes de moyenne altitude'
          WHEN talweg_elevation_min >= 300 AND talweg_slope < 0.05 THEN 'Plaines de moyenne altitude'
          WHEN talweg_elevation_min >= -50 AND talweg_slope >= 0.05 THEN 'Pentes de basse altitude'
          WHEN talweg_elevation_min >= -50 AND talweg_slope < 0.05 THEN 'Plaines de basse altitude'
          ELSE 'unvalid'
        END AS class_topographie,

        -- Dominant Land Use Classification
        CASE
          WHEN forest_pc IS NULL OR grassland_pc IS NULL OR natural_open_pc IS NULL OR crops_pc IS NULL OR built_environment_pc IS NULL THEN 'unvalid'
          WHEN forest_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Forêt'
          WHEN grassland_pc + natural_open_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Prairies et sols nus'
          WHEN crops_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Cultures'
          WHEN built_environment_pc >= GREATEST(forest_pc, grassland_pc + natural_open_pc, crops_pc, built_environment_pc) THEN 'Espace construit'
          ELSE 'unvalid'
        END AS class_lu_dominante,

        -- Urban Land Use Classification
        CASE
          WHEN built_environment_pc IS NULL THEN 'unvalid'
          WHEN built_environment_pc >= 70 THEN 'Fortement urbanisé'
          WHEN built_environment_pc >= 40 THEN 'Urbanisé'
          WHEN built_environment_pc >= 10 THEN 'Modérément urbanisé'
          WHEN built_environment_pc >= 0 THEN 'Presque pas/Pas urbanisé'
          ELSE 'unvalid'
        END AS class_urban,

        -- Agricultural Land Use Classification
        CASE
          WHEN crops_pc IS NULL THEN 'unvalid'
          WHEN crops_pc >= 70 THEN 'Très Forte'
          WHEN crops_pc >= 40 THEN 'Forte'
          WHEN crops_pc >= 10 THEN 'Modérée'
          WHEN crops_pc >= 0 THEN 'Basse/Absente'
          ELSE 'unvalid'
        END AS class_agriculture,

        -- Natural Land Use Classification
        CASE
          WHEN natural_open_pc IS NULL OR forest_pc IS NULL OR grassland_pc IS NULL THEN 'unvalid'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 70 THEN 'Très forte'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 40 THEN 'Forte'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 10 THEN 'Modérée'
          WHEN (natural_open_pc + forest_pc + grassland_pc) >= 0 THEN 'Presque pas/Pas naturelle'
          ELSE 'unvalid'
        END AS class_nature,

        -- Gravel Bars Classification
        CASE
          WHEN gravel_bars IS NULL OR water_channel IS NULL THEN 'unvalid'
          WHEN (gravel_bars / NULLIF(water_channel + gravel_bars, 0)) >= 0.5 THEN 'Fréquents'
          WHEN (gravel_bars / NULLIF(water_channel + gravel_bars, 0)) > 0 THEN 'Occasionnels'
          WHEN (gravel_bars / NULLIF(water_channel + gravel_bars, 0)) = 0 THEN 'Absents'
          ELSE 'unvalid'
        END AS class_gravel,

        -- Confinement Classification
        CASE
          WHEN idx_confinement IS NULL THEN 'unvalid'
          WHEN idx_confinement >= 0.7 THEN 'Peu confiné'
          WHEN idx_confinement >= 0.4 THEN 'Modérément confiné'
          WHEN idx_confinement >= 0.1 THEN 'Confiné'
          WHEN idx_confinement >= 0 THEN 'Très confiné'
          ELSE 'unvalid'
        END AS class_confinement,

        -- Habitat Classification
        CASE
          WHEN riparian_corridor_pc IS NULL OR semi_natural_pc IS NULL THEN 'unvalid'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 70 THEN 'Élevée'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 40 THEN 'Bonne'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 10 THEN 'Moyenne'
          WHEN (riparian_corridor_pc + semi_natural_pc) >= 0 THEN 'Faible/Absente'
          ELSE 'unvalid'
        END AS class_habitat
      FROM network_metrics
      WHERE  axis = ?selected_axis_id")
    }
    if(aggregated==TRUE){
      sql <- paste0("SELECT fid, axis, ids, minmeasure, maxmeasure, toponyme, strahler, gid_region, geom,",
                    paste(metric_info$metric_name,collapse=", "),
                    ", class_strahler, class_topographie, class_lu_dominante, class_urban,
                     class_agriculture, class_nature, class_gravel, class_confinement, class_habitat, class_style
                    FROM network_metrics_aggregated AS network_metrics
                    WHERE  axis = ?selected_axis_id")
    }
    query <- sqlInterpolate(con, sql, selected_axis_id = selected_axis_id)

    data <- sf::st_read(dsn = con, query = query)

    if(aggregated==TRUE){
      data= data %>%
        mutate(maxmeasure=case_when(ids<max(ids)~lead(minmeasure,1),
                                    TRUE~maxmeasure)) %>%
        tidyr::pivot_longer(cols=minmeasure:maxmeasure, names_to="measure_type",values_to="measure") %>%
        dplyr::arrange(ids,measure) %>%
        mutate(class_agriculture=case_when(class_agriculture=="Impact agricole modéré" ~ "Modérée",
                                           class_agriculture=="Impact agricole élevé" ~ "Forte",
                                           class_agriculture=="Forte impact agricole" ~ "Très Forte",
                                           class_agriculture=="Presque pas/pas d'impact agricole"~"Basse/Absente",
                                           TRUE~class_agriculture),
               class_urban=case_when(class_urban=="Presque pas/pas urbanisé"~"Presque pas/Pas urbanisé",
                                     class_urban=="modérément urbanisé"~"Modérément urbanisé",
                                     class_urban=="urbanisé"~"Urbanisé",
                                     class_urban=="fortement urbanisé"~"Fortement urbanisé",
                                     TRUE~class_urban),
               class_nature=case_when(class_nature=="Presque pas/pas naturelle"~"Presque pas/Pas naturelle",
                                      class_nature=="Utilisation naturelle modérée"~"Modérée",
                                      class_nature=="Forte utilisation naturelle"~"Forte",
                                      class_nature=="Très forte utilisation naturelle"~"Très forte",
                                      TRUE~class_nature),
               class_gravel=case_when(class_gravel=="abundant"~"Fréquents",
                                      class_gravel=="moyennement présente"~"Occasionnels",
                                      class_gravel=="absent"~"Absents",
                                      TRUE~class_gravel),
               class_confinement=case_when(class_confinement=="confiné"~"Confiné",
                                           class_confinement=="très confiné"~"Très confiné",
                                           class_confinement=="modérement espace"~"Modérément confiné",
                                           class_confinement=="espace abondant"~"Peu confiné",
                                           TRUE~class_confinement),
               class_habitat=case_when(class_habitat=="très bien connecté"~"Elevée",
                                       class_habitat=="bien connecté"~"Bonne",
                                       class_habitat=="moyen connecté"~"Moyenne",
                                       class_habitat=="faible / absente"~"Faible/Absente",
                                       TRUE~class_habitat)
        )

    }else{
      data= data %>%
        arrange(measure)
    }

  } else { # if axis is null
    data <- NULL
  }
  return(data)
}


#' Get Network Metrics Data for a Specific Region
#'
#' This function retrieves data about network metrics for a specific region based on its ID.
#'
#' @param selected_axis_id The ID of the selected region.
#' @param con Connection to Postgresql database.
#'
#' @return A sf data frame containing information about network metrics for the specified region.
#'
#' @examples
#' con <- db_con()
#' network_metrics_data <- data_get_axis_dgos_from_region(selected_region_id = 33, con = con)
#' DBI::dbDisconnect(con)
#'
#' @importFrom sf st_read
#' @importFrom dplyr arrange
#' @importFrom DBI sqlInterpolate
#'
#' @export
data_get_axis_dgos_from_region <- function(selected_region_id, con) {

  if (!is.null(selected_region_id)) {

    sql <-paste0("
      SELECT
        fid, axis, ids, minmeasure, maxmeasure, toponyme, strahler, gid_region, geom,",
                 paste(metric_info$metric_name,collapse=", "),
                 ", class_strahler, class_topographie, class_lu_dominante, class_urban,
        class_agriculture, class_nature, class_gravel, class_confinement, class_habitat, class_style
      FROM network_metrics_aggregated AS network_metrics
      WHERE  network_metrics.gid_region = ?selected_region_id")
    query <- sqlInterpolate(con, sql, selected_region_id = selected_region_id)
    data <- sf::st_read(dsn = con, query = query) %>%
      arrange(minmeasure)
  }
  else {
    data <- NULL
  }
  return(data)
}
#' Get the start and end coordinates of a spatial object's axis
#'
#' This function takes a spatial object with a LINESTRING geometry and returns
#' a data frame containing the start and end coordinates of the axis.
#'
#' @param selected_axis_id The ID of the selected axis for which to retrieve the start and end coordinates.
#' @param con Connection to Postgresql database.
#'
#' @return A data frame with two rows, where the first row contains the start
#'         coordinates (x and y) and the second row contains the end coordinates (x and y).
#'
#' @importFrom sf st_coordinates st_cast st_sf st_linestring st_geometry st_sfc
#' @importFrom utils tail head
#'
#' @examples
#'
#' df <- data_get_axis_start_end(line_sf)
#'
#' @export
data_get_axis_start_end <- function(selected_axis_id, con) {

  sql <- "SELECT axis, geom
    FROM hydro_axis
    WHERE  axis = ?selected_axis_id"
  query <- sqlInterpolate(con, sql, selected_axis_id=as.character(selected_axis_id))

  coords <- sf::st_read(dsn = con, query = query) %>%
    st_geometry() %>%
    st_coordinates()
  axis_start_end <- rbind(head(coords, 1),
                          tail(coords, 1)) %>%
    as.data.frame() %>%
    select(X,Y)
  #
  #   # Extract the first and last point coordinates of the LINESTRING
  #   start_coords <- st_coordinates(st_geometry(data)[[1]])[1, ]
  #   end_coords <- st_coordinates(st_geometry(data)[[length(st_geometry(dgo_axis))]])[nrow(st_coordinates(st_geometry(dgo_axis)[[length(st_geometry(dgo_axis))]])), ]
  #
  #   # Combine the start and end coordinates into a data frame
  #
  #   # Assign meaningful column names

  return(axis_start_end)
}

#' Get elevation profiles data from selected dgo fid.
#'
#' @param selected_dgo_fid integer selected dgo fid.
#' @param con PqConnection to Postgresql database.
#'
#' @importFrom DBI dbGetQuery sqlInterpolate
#' @importFrom dplyr arrange mutate
#'
#' @return data.frame
#' @export
#'
#' @examples
#' con <- db_con()
#' data_get_elevation_profiles(selected_dgo_fid = 95, con = con)
#' DBI::dbDisconnect(con)
data_get_elevation_profiles <- function(selected_dgo_fid, con){

  sql <- "
          SELECT
          	id, hydro_swaths_gid, axis, measure_medial_axis, distance, profile
          FROM elevation_profiles
          WHERE hydro_swaths_gid = ?selected_dgo_fid"
  query <- sqlInterpolate(con, sql, selected_dgo_fid = selected_dgo_fid)

  data <- DBI::dbGetQuery(conn = con, statement = query) %>%
    arrange(distance) %>%
    mutate(profile = round(profile, digits = 2))
  return(data)
}
