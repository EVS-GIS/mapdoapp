testServer(
  mod_explore_server,
  # Add here your module params
  args = list()
  , {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )

    # plot init when app open
    expect_true(inherits(r_val$plot, "plotly"))

    # bassin clicked
    session$setInputs(exploremap_shape_click = list(id = "06", .nonce = 0.848898801317209, group = "BASSIN",
                                                    lat = 45.0704171568597, lng = 5.18574748500369))
    expect_true(input$exploremap_shape_click$group == "BASSIN")
    expect_true(input$exploremap_shape_click$id == "06")

    # check region data import
    expect_true(!is.null(r_val$regions_in_bassin))
    expect_equal(unique(r_val$regions_in_bassin$cdbh), "06")

    # region clicked
    session$setInputs(exploremap_shape_click = list(id = 11L, .nonce = 0.240790184375964, group = "REGION",
                                                    lat = 45.586365291486, lng = 6.43802871921718))

    expect_true(input$exploremap_shape_click$group == "REGION")
    expect_true(input$exploremap_shape_click$id == "11")

    # store region select data
    expect_equal(r_val$selected_region_feature$gid, 11)
    # check axis data
    expect_equal(unique(r_val$network_region_axis$gid_region), 11)
    # check strahler df creation
    expect_true(inherits(r_val$strahler, "data.frame"))
    # check strahler ui
    expect_true(inherits(r_val$ui_strahler_filter, "shiny.tag"))
    # check metric type ui
    expect_true(inherits(r_val$ui_metric_type, "shiny.tag"))
    # check download type ui
    expect_true(inherits(r_val$ui_download, "shiny.tag"))

    # input metric type selected
    session$setInputs(metric_type = "largeur")
    # metric type default select
    expect_true(input$metric_type == "largeur")
    # check metric ui
    expect_true(inherits(r_val$ui_metric, "shiny.tag"))
    # unit area is null
    expect_true(is.null(r_val$ui_unit_area))

    # test filter strahler only rank = 1
    session$setInputs(strahler = c(1,1))
    expect_true(!is.null(r_val$cql_filter))
    # SLD style NULL if no metric select
    expect_true(is.null(r_val$sld_body))

    # input metric selected
    session$setInputs(metric = "active_channel_width")
    expect_true(input$metric == "active_channel_width")
    expect_true(r_val$selected_metric == "active_channel_width")
    expect_true(inherits(r_val$min_max_metric, "data.frame"))
    expect_true(inherits(r_val$ui_metric_filter, "shiny.tag"))

    # filter by metric value with 1-100 width active channel
    session$setInputs(metricfilter = c(0,100))
    expect_true(!is.null(r_val$sld_body))

    # change metric selected to landuse
    session$setInputs(metric_type = "landuse")
    # select water channel from landuse metric type
    session$setInputs(metric = "water_channel")
    expect_true(input$metric == "water_channel")
    # expect_true(r_val$selected_metric == "water_channel")
    # # change unit area to percent
    # session$setInputs(unit_area = "percent")
    # expect_true(input$metric == "water_channel")
    # # selected_metric change to percent!
    # expect_true(r_val$selected_metric == "water_channel_pc")



})

test_that("module ui works", {
  ui <- mod_explore_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_explore_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

