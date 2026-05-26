test_that("data_get_bassins works", {
  con <- db_con()
  opacity <-  list(clickable = 0.01,
                 not_clickable = 0.10)
  data <- data_get_bassins(opacity = opacity,
                           con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_regions_in_bassin works", {
  con <- db_con()
  opacity <-  list(clickable = 0.01,
                   not_clickable = 0.10,
                   con = con)
  data <- data_get_regions_in_bassin(selected_bassin_id = "06",
                                     opacity = opacity,
                                     con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_region works", {
  con <- db_con()
  data <- data_get_region(region_click_id = 11,
                          con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_min_max_strahler works", {
  con <- db_con()
  data <- data_get_min_max_strahler(selected_region_id = 11,
                                    con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "data.frame"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
  expect_named(data, c("min", "max"),
               info = "Data frame has columns named 'min' and 'max'")
})

test_that("data_get_min_max_metric works", {
  con <- db_con()
  data <- data_get_min_max_metric(selected_region_id = 11,
                                  selected_metric = "active_channel_width",
                                  con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "data.frame"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
  expect_named(data, c("min", "max"),
               info = "Data frame has columns named 'min' and 'max'")
})

test_that("data_get_roe_in_region works", {
  con <- db_con()
  data <- data_get_roe_in_region(selected_region_id = 11,
                                 con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_axis works", {
  con <- db_con()
  data <- data_get_axis(selected_region_id = 11,
                        con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_network_axis works", {
  con <- db_con()
  data <- data_get_network_axis(selected_axis_id = 2000796122,
                                con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_axis_start_end works", {
  line_coords <- matrix(c(0, 0, 1, 1), ncol = 2)
  line_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(line_coords)))
  data <- data_get_axis_start_end(line_sf)
  expect_true(inherits(data, "data.frame"),
              "df data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_dgo_in_region works", {
  con <- db_con()
  data <- data_get_dgo_in_region(selected_region_id = 11,
                                 con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_hydro_sites works", {
  con <- db_con()
  data <- data_get_hydro_sites(selected_region_id = 11,
                               con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})
test_that("data_get_carhyce_stations works", {
  con <- db_con()
  data <- data_get_carhyce_stations(con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "sf"),
              "sf data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})

test_that("data_get_elevation_profiles works", {
  con <- db_con()
  data <- data_get_elevation_profiles(selected_dgo_fid = 95,
                                      con = con)
  DBI::dbDisconnect(con)
  expect_true(inherits(data, "data.frame"),
              "df data loaded")
  expect_true(nrow(data) > 0,
              info = "Data is not empty")
})
