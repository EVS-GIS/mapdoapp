#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbDisconnect
#'
#'
#' @noRd
app_server <- function(input, output, session) {

  # track session
  track_session(session = session)

  ### R_VAL ####
  r_val <- reactiveValues(

    tab_open1 = NULL, # descriptor of the opened tab on right side
    tab_open2 = NULL, # descriptor of the opened tab below

    # UI
    selection_text = "", # description text indicating basin, region, axis

    # map
    map_proxy = NULL,
    leaflet_hover_measure = NULL, # metric y-value to add vertical line on longitudinal profile
    visualization = "classes",

    selected_metric = "talweg_elevation_min", # select main metric column name

    # data hydrography
    bassins = NULL,
    bassin_name = NULL,
    regions_in_bassin = NULL,
    region_click = NULL,
    region_clicked = FALSE, # boolean stating whether first region was clicked
    region_name = NULL,
    selected_region_feature = NULL,
    network_region_axis = NULL,
    network_region = NULL,
    axis_name = NULL,
    axis_click = NULL,
    axis_clicked = FALSE, # boolean stating whether first axis was clicked
    dgo_axis = NULL,
    axis_start_end = NULL,
    data_dgo_clicked = NULL,
    data_section = NULL,

    manual_classes_table = NULL, # manual classification table

    # classified networks
    network_region_classified = NULL, # classified dgos of selected region
    dgo_axis_classified = NULL, # classified dgos of selected axis
    merged_networks_classified = NULL, # merged classified networks of region and axis, with additional scale-variable to distinguish them

    # selected class from proposed classes
    classes_proposed_selected = NULL,

    # metric info
    selected_metric_title = NULL,
    selected_metric_type = NULL,
    selected_metric_info = NULL,

    # data external
    roe_region = NULL, # ROE data in selected region
    roe_axis = NULL, # ROE data in selected axis
    hydro_sites_region = NULL,

    # styling of wms-layer
    sld_body = NULL,

    # longitudinal plot
    plot_long_proxy = NULL
,
    # others variables
    opacity = list(clickable = 0.01, not_clickable = 0.10) # opacity value to inform the user about available bassins and regions

  )

  ### DB connection ####
  con <- db_con()

  ### Server activation ####
  # main servers
  mod_mapdo_app_server("mapdo_app_1", con, r_val)
  mod_documentation_server("documentation_1")

  # tabs
  mod_classification_proposed_server("classification_proposed_1", r_val)
  mod_classification_manual_server("classification_manual_1", con, r_val)
  mod_metric_overview_server("metric_overview_1", r_val)
  mod_profil_long_server("profil_long_1", r_val)
  mod_profil_transverse_server("profil_transverse_1", r_val)
  mod_classes_distribution_server("classes_distribution_1", r_val)

  ### DB disconnect when closing session ####
  onStop(function() {
    if (!is.null(con)) {
      DBI::dbDisconnect(con)
    }
  })
}
