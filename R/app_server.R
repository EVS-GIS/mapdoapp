# Set limits for cache
shinyOptions(cache = cachem::cache_mem(max_size = 4e9, # 4 GB memory cache limit
                                       max_age = 86400)) # 1 day age cache limit

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbDisconnect
#' @importFrom waiter Waitress
#'
#'
#' @noRd
app_server <- function(input, output, session) {

  # track session
  track_session(session = session)

  ### DB connection ####
  con <- db_con()

  ### WAITER SCREEN ####

  # call the waitress
  waitress <- Waitress$
    new(theme = "overlay-percent")$
    start(h3("Charger Mapd'O...")) # start

  # Simulate the progress based on how many steps (reactive elements) you're loading
  total_steps <- 10
  step_progress <- 100 / total_steps  # Progress per step


  ### R_VAL ####
  r_val <- reactiveValues(

    # UI
    selection_text = "", # description text indicating basin, region, axis
    visualization = "classes", # stating which visualization is currently active ("classes" vs "manual")
    classes_proposed_selected = NULL, # selected class in proposed classes tab
    tab_page = NULL, # selected tab in main navbarpage
    tab_classes = NULL, # selected tab in classes tabset
    tab_plots = NULL, # selected tab in plots tabset
    tab_analysis = NULL, # selected tab in analysis tabset

    # map
    map_proxy = NULL, # proxy object for map
    opacity_basins = list(clickable = 0.01, not_clickable = 0.10), # opacity value to inform the user about available bassins and regions
    leaflet_hover_measure = NULL, # measure to be displayed in the leaflet hover

    # geo objects
    basin_name = NULL, # name of selected basin
    basin_id = NULL, # id of selected basin
    region_name = NULL, # name of selected region
    region_id = NULL, # id of selected region
    region_id_data = NULL,
    region_data_classified = NULL, # data of selected region
    axis_name = NULL, # name of selected axis
    axis_id = NULL, # id of selected axis
    axis_data = NULL, # data of selected axis
    axis_data_classified = NULL, # data of selected axis classified
    axis_strahler = NULL, # strahler order of selected axis
    swath_id = NULL, # id of selected swath
    swath_data_section = NULL, # data of selected swath crosssection
    swath_data_dgo = NULL, # data of selected swath dgo

    # download
    dataset_input = NULL, # dataset input to be download

    # first time clicked
    axis_clicked = FALSE, # if axis was clicked

    manual_classes_table = NULL, # values of classes and assigned colors from manual classification
    classes_man_stats = NULL, # metric statistics for manual classes
  )
  waitress$inc(step_progress)  # Increment progress 1
  Sys.sleep(.3)

  ### GLOBALS ####
  # create empty list to store fixed global values which can be accessed by other modules
  # globals <- list()

  # load regions sf data
  globals$regions = data_get_regions(con, opacity = list(clickable = 0.01, not_clickable = 0.10))
  waitress$inc(step_progress)  # Increment progress 2
  Sys.sleep(.3)

  # Create a unique key based on the regions_gids content
  globals$regions_gids_key = paste(collapse = "_", sort(globals$regions$gid))
  waitress$inc(step_progress)  # Increment progress 3
  Sys.sleep(.3)

  # load basins sf data (cached)
  globals$basins <- reactive({
    data_get_basins(con, opacity = list(clickable = 0.01, not_clickable = 0.10))
  }) %>%
    bindCache(globals$regions_gids_key)
  waitress$inc(step_progress)  # Increment progress 4
  Sys.sleep(.3)

  # load axes sf data (cached)
  globals$axes <- reactive({
    data_get_axes(con)
  }) %>%
    bindCache(globals$regions_gids_key)
  waitress$inc(step_progress)  # Increment progress 5
  Sys.sleep(.3)

  # load roe sf data (cached)
  globals$roe_sites <- reactive({
    data_get_roe_sites(con)
  }) %>%
    bindCache(globals$regions_gids_key)
  waitress$inc(step_progress)  # Increment progress 6
  Sys.sleep(.3)

  # load discharge stations sf data (cached)
  globals$hydro_sites <- reactive({
    data_get_hydro_sites(con)
  }) %>%
    bindCache(globals$regions_gids_key)
  waitress$inc(step_progress)  # Increment progress 7
  Sys.sleep(.3)


  #### Metric stats caching ####
  globals$metric_stats <- reactive({
    data_get_stats_metrics(con)
  }) %>%
    bindCache(globals$regions_gids_key)
  waitress$inc(step_progress)  # Increment progress 8
  Sys.sleep(.3)

  #### Axis data caching ####
  globals$axis_data <- reactive({
    data_get_axis_dgos(selected_axis_id = r_val$axis_id, con)
  }) %>%
    bindCache(c(r_val$axis_id, globals$regions_gids_key))

  #### Classes stats caching ####
  globals$classes_stats <- reactive({
    if (!is.null(r_val$classes_proposed_selected)) {
      data_get_distr_class(con = con, class_name = globals$classes_proposed[r_val$classes_proposed_selected,]$class_name)
    } else {
      NULL
    }
  }) %>%
    bindCache(globals$regions_gids_key, r_val$classes_proposed_selected)

  # navbarPage identifier
  observeEvent(input$navbarPage, {
    r_val$tab_page = input$navbarPage
  })


  ### Server activation ####
  # main servers
  mod_explore_server("explore_1", con, r_val, globals, waitress)
  mod_analysis_server("analysis_1", con, r_val, globals)
  mod_download_server("download_1" , con, r_val, globals)
  mod_documentation_server("documentation_1")
  mod_help_guide_server("help_guide_1", r_val)

  # tabs
  mod_expl_classes_proposed_server("expl_classes_proposed_1", r_val, globals)
  mod_expl_classes_manual_server("expl_classes_manual_1", con, r_val, globals)

  mod_expl_plot_long_server("expl_plot_long_1", r_val, globals)
  mod_expl_plot_crosssection_server("expl_plot_crosssection_1", r_val)

  mod_analysis_bimetric_server("analysis_bimetric_1", con, r_val, globals)
  waitress$inc(step_progress)  # Increment progress 10
  Sys.sleep(.3)


  # All tasks are done, hide the loading screen
  waitress$close()



  #### Region data caching ####
  # globals$region_data <- reactive({
  #   data_get_axis_dgos_from_region(selected_region_id = r_val$region_id_data, con)
  # }) %>%
  #   bindCache(c(r_val$region_id_data, globals$regions_gids_key))


  ### DB disconnect when closing session ####
  onStop(function() {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
}
