testServer(
  mod_explore_server,
  # Add here your module params
  args = list()
  , {
    ns <- session$ns
    rv <- reactiveValues(regions_in_bassin = NULL,
                         selected_region_feature = NULL,
                         network_region_axis = NULL,
                         network_region_axis = NULL,
                         strahler = NULL,
                         ui_strahler_filter = NULL,
                         ui_metric_type = NULL,
                         ui_download = NULL,
                         ui_metric = NULL)
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )

    # bassin clicked
    session$setInputs(exploremap_shape_click = list(id = "06", .nonce = 0.848898801317209, group = "BASSIN",
                                                    lat = 45.0704171568597, lng = 5.18574748500369))
    expect_true(input$exploremap_shape_click$group == "BASSIN")
    expect_true(input$exploremap_shape_click$id == "06")

    # check region data import
    expect_true(is.null(rv$regions_in_bassin))
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

    session$setInputs(metric_type = "largeur")
    # metric type default select
    expect_true(input$metric_type == "largeur")
    # check metric ui
    expect_true(inherits(r_val$ui_metric, "shiny.tag"))
    # unit area is null
    expect_true(is.null(r_val$ui_unit_area))



    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
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

