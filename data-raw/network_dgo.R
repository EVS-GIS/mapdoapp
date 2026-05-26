## code to prepare `network_dgo` dataset goes here

query <- "SELECT
            network_metrics.fid, axis, measure, toponyme, strahler, talweg_elevation_min,
            active_channel_width, natural_corridor_width,
            connected_corridor_width, valley_bottom_width, talweg_slope, floodplain_slope,
            water_channel, gravel_bars, natural_open, forest, grassland, crops,
            diffuse_urban, dense_urban, infrastructures, active_channel, riparian_corridor,
            semi_natural, reversible, disconnected, built_environment,
            water_channel_pc, gravel_bars_pc, natural_open_pc, forest_pc, grassland_pc, crops_pc,
            diffuse_urban_pc, dense_urban_pc, infrastructures_pc, active_channel_pc,
            riparian_corridor_pc, semi_natural_pc, reversible_pc, disconnected_pc,
            built_environment_pc, sum_area, idx_confinement, gid_region, network_metrics.geom, confinement, ID_segment
          FROM network_metrics_v2 as network_metrics
          WHERE  axis = 9"

network_dgo <- sf::st_read(dsn = db_con(), query = query) %>%
  dplyr::arrange(measure)

# usethis::use_data(network_dgo, overwrite = TRUE)
# checkhelper::use_data_doc(name = "network_dgo")
# attachment::att_amend_desc()
