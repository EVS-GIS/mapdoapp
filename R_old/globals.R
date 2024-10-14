globalVariables(unique(c(
  # data_get_axis:
  "region_click",
  # data_get_network_axis:
  "measure",
  # data_get_regions_in_bassin:
  "bassin_click",
  # data_get_roe_in_region:
  "region_click",
  # lg_profile_main:
  "selected_axis_df",
  # lg_profile_second:
  "selected_axis_df",
  # map_add_regions_in_bassin:
  "region_hydro",
  # mod_explore_server : <anonymous>:
  "fid", "measure", "axis", "cdbh", "click", "gid",
  # data_get_elevation_profiles:
  "distance", "profile"
)))

metrics_params <<- params_metrics()

# save classes once to be able to access it directly
classes_proposed <<- params_classes()

# save classes colors once to be able to access it directly
colors_classes_proposed <<- params_classes_colors()
