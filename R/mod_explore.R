
# UI ----------------------------------------------------------------------



#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom bslib popover
#' @importFrom bsicons bs_icon
#' @importFrom leaflet leafletOutput
#' @importFrom htmltools HTML div img
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explore_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
      useShinyjs(),
      tags$head(
        tags$style(
          HTML("
          .form-group{margin-bottom: 10px}
          ")
        )
      ), # head
      fluidRow(
        column(
          width = 8,
          leafletOutput(ns("map"), height = 500),
          fluidRow(
            column(
              width = 10,
              textOutput(ns("selection_textUI"))),
            column(
              width = 2,
              div(class = "text-right",
                  uiOutput(ns("deselect_buttonUI"))))
          )),
        column(
          width = 4,
          tabsetPanel(
            id = ns("tabset_classes"),
            tabPanel("Classes proposées",
                     mod_expl_classes_proposed_ui("expl_classes_proposed_1")
            ),
            tabPanel("Classification manuelle",
                     div(style = "height: 450px; overflow-y: auto;",  # Set fixed height for this panel
                         mod_expl_classes_manual_ui("expl_classes_manual_1"))
            ), type = "pills"
          ) #tabsetpanel
        ) #column
      ), #row
      hr(),
      fluidRow(
        style = "margin-top: 10px;
        margin-bottom: 10px;",
        tabsetPanel(
          id = ns("tabset_plots"),
          tabPanel(
            "Évolution longitudinale",
            mod_expl_plot_long_ui("expl_plot_long_1")
          ),
          tabPanel("Profil transversal",
                   mod_expl_plot_crosssection_ui("expl_plot_crosssection_1")
          ), type = "pills"
        )
      )
    ) #page

  )
}



# SERVER ------------------------------------------------------------------


#' explore Server Functions
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet removeControl addLegend addControl
#' @importFrom leaflet.extras addWMSLegend
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull
#' @importFrom plotly event_register event_data plotlyProxy plotlyProxyInvoke renderPlotly plotlyOutput
#' @importFrom bslib popover update_popover
#' @importFrom bsicons bs_icon
#' @importFrom sf st_write
#' @importFrom shinyjs onclick runjs
#' @noRd
mod_explore_server <- function(id, con, r_val, globals, waitress){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### REACTIVES ####
    r_val_local <- reactiveValues(
      deselect_button = NULL
    )

    #### Map ####
    # create main map
    output$map <- renderLeaflet({
      map_initialize(params_wms = globals$wms_params,
                     params_map_group = globals$map_group_params,
                     id_logo_ign_remonterletemps = ns("logo_ign_remonterletemps"),
                     basins_data = globals$basins(),
                     regions_data = globals$regions,
                     axes_data = globals$axes(),
                     roe_sites = globals$roe_sites(),
                     hydro_sites = globals$hydro_sites()
      )
    }) %>%
      bindCache(globals$regions_gids_key)
    waitress$inc(10)  # Increment progress 9
    Sys.sleep(.3)

    # add remonter-le-temps-button functionality
    onclick(id = "logo_ign_remonterletemps", expr =
              runjs(sprintf("window.open('%s', '_blank')",
                            utils_url_remonterletemps(lng = input$map_center$lng,
                                                      lat = input$map_center$lat,
                                                      zoom = input$map_zoom)))
    )

    #### Map Proxy ####
    observe({
      r_val$map_proxy <- leafletProxy("map")
    })

    #### Map Legends ####
    observeEvent(input$map_groups, {

      # landuse map
      if (any(input$map_groups %in% globals$map_group_params$landuse)) {
        r_val$map_proxy %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = globals$wms_params$landuse),
                       title = "Occupation du sol",
                       position = "bottomright",
                       layerId = "landuse")
      } else {
        r_val$map_proxy %>%
          removeControl(layerId = "landuse")
      }

      # lateral continuity map
      if (any(input$map_groups %in% globals$map_group_params$continuity)) {
        r_val$map_proxy %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = globals$wms_params$continuity),
                       title = "Continuité latérale",
                       position = "bottomright",
                       layerId = "continuity")
      } else {
        r_val$map_proxy %>%
          removeControl(layerId = "continuity")
      }

      # valley bottom map
      if (any(input$map_groups %in% globals$map_group_params$valley_bottom)) {
        r_val$map_proxy %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = globals$wms_params$valley_bottom),
                       title = "Fond de vallée",
                       position = "bottomright",
                       layerId = "valley_bottom")
      } else {
        r_val$map_proxy %>%
          removeControl(layerId = "valley_bottom")
      }

      # zone inondable map
      if (any(input$map_groups %in% globals$map_group_params$inondation)) {
        r_val$map_proxy %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = globals$wms_params$inondation),
                       title = "Zone inondable centennale",
                       position = "bottomright",
                       layerId = "inondation")
      } else {
        r_val$map_proxy %>%
          removeControl(layerId = "inondation")
      }

      # ouvrage de protection map
      if (any(input$map_groups %in% globals$map_group_params[["ouvrage_protection"]])) {
        r_val$map_proxy %>%
          addWMSLegend(map_legend_wms_overlayer(wms_params = globals$wms_params$ouvrage_protection),
                       position = "bottomright",
                       layerId = "ouvrage_protection")
      } else {
        r_val$map_proxy %>%
          removeControl(layerId = "ouvrage_protection")
      }

      # Custom legend HTML
      # custom_legend <-

      # ROE
      if (any(input$map_groups %in% globals$map_group_params[["roe"]])) {
        r_val$map_proxy %>%
          addControl(
            HTML("
                <div style='display: flex; align-items: center;'>
                  <div style='background-color: #323232; border-radius: 50%; width: 12px; height: 12px; margin-right: 8px;'></div>
                  <div>Obstacles à l'Ecoulement (ROE)</div>
                </div>"),
            position = "bottomright", layerId = "roe"
          )
        # addLegend(
        #   position = "bottomright",
        #   colors = "#323232",
        #   labels = "Obstacles à l'Ecoulement (ROE)",
        #   layerId = "roe"
        # )
      } else {
        leafletProxy("map") %>%
          removeControl(layerId = "roe")
      }

      # Site hydrométrique
      if (any(input$map_groups %in% globals$map_group_params[["hydro_sites"]])) {
        r_val$map_proxy %>%
          addControl(
            HTML("
                <div style='display: flex; align-items: center;'>
                  <div style='background-color: #33B1FF; border-radius: 50%; width: 12px; height: 12px; margin-right: 8px;'></div>
                  <div>Sites hydrométriques</div>
                </div>"),
            position = "bottomright", layerId = "hydro_sites"
          )
        # addLegend(
        #   position = "bottomright",
        #   colors = "#33B1FF",
        #   labels = "Obstacles à l'Ecoulement (ROE)",
        #   layerId = "hydro_sites"
        # )
      } else {
        r_val$map_proxy %>%
          removeControl(layerId = "hydro_sites")
      }

      # Site hydrométrique
      # if (any(input$map_groups %in% globals$map_group_params[["hydro_sites"]])) {
      #   map_legend_vector_overlayer(layer_label = "Site hydrométrique",
      #                               color = "#33B1FF")
      # }

    })

    #### Description Text ####

    output$selection_textUI <- renderText({
      r_val$selection_text
    })

    output$deselect_buttonUI <- renderUI({
      r_val_local$deselect_button
    })




    ### TABSET LISTENERS ####
    # save current tabs in reactive values

    # classification tab
    observeEvent(input$tabset_classes, {
      r_val$tab_classes = input$tabset_classes
    })

    # plots tab
    observeEvent(input$tabset_plots, {
      r_val$tab_plots = input$tabset_plots
    })


    ### description text ####
    observe({

      r_val$selection_text = dplyr::case_when(
        !is.null(r_val$basin_name) & is.null(r_val$region_name) & is.null(r_val$axis_name) ~ paste0("Bassin :", r_val$basin_name),
        !is.null(r_val$basin_name) & !is.null(r_val$region_name) & is.null(r_val$axis_name) ~ paste0("Bassin : ", r_val$basin_name,
                                                                                                     "  |  Région : ", r_val$region_name),
        !is.null(r_val$basin_name) & !is.null(r_val$region_name) & !is.null(r_val$axis_name) ~ paste0("Bassin : ", r_val$basin_name,
                                                                                                      "  |  Région : ", r_val$region_name,
                                                                                                      "  |  Axe : ", r_val$axis_name, " (Ordre de Strahler : ", r_val$axis_strahler, ")"),
        .default = ""
      )
    })

    # deselect button
    observeEvent(r_val$selection_text, {
      if (r_val$selection_text != "") {
        r_val_local$deselect_button = actionButton(ns("deselect_button"), "Désélectionner")
      } else {
        r_val_local$deselect_button = NULL
      }
    })


    ### CLICK EVENTS ####

    observeEvent(input$map_shape_click, {

      ##### Basin ####
      if (input$map_shape_click$group == globals$map_group_params[["bassin"]]) {

        # check if basin not already selected
        if (!is.null(r_val$basin_id) && (r_val$basin_id == input$map_shape_click$id)) {

        } else {

          # get basin name
          r_val$basin_name = globals$basins() %>%
            filter(cdbh == input$map_shape_click$id) %>%
            pull(lbbh)

          # get basin id
          r_val$basin_id = input$map_shape_click$id

          # reset others
          r_val$region_name = NULL
          r_val$region_id = NULL
          r_val$axis_name = NULL
          r_val$axis_id = NULL
          r_val$axis_strahler = NULL
          r_val$swath_id = NULL
          r_val$swath_data_section = NULL
          r_val$swath_data_dgo = NULL

          # clean map
          r_val$map_proxy %>%
            clearGroup(globals$map_group_params[["dgo_axis"]]) %>%
            clearGroup(globals$map_group_params[["axis_start_end"]]) %>%
            clearGroup(globals$map_group_params[["dgo"]])
        }
      }

      #### Region ####
      if (input$map_shape_click$group == globals$map_group_params[["region"]]) {

        # check if region not already selected
        if (!is.null(r_val$region_id) && (r_val$region_id == input$map_shape_click$id)) {

        } else {

          # get region id
          r_val$region_id = input$map_shape_click$id

          # get region name
          r_val$region_name = globals$regions %>%
            filter(gid == r_val$region_id) %>%
            pull(lbregionhy)

          # check if region is in other basin than the one that is selected
          if (!is.null(r_val$basin_id) && (r_val$basin_id == (globals$regions %>%
                                                              filter(gid == input$map_shape_click$id) %>%
                                                              pull(cdbh)))) {
          } else {

            r_val$basin_name = globals$basins() %>%
              filter(cdbh == globals$regions %>%
                       filter(gid == input$map_shape_click$id) %>%
                       pull(cdbh)) %>%
              pull(lbbh)

            r_val$basin_id = globals$regions %>%
              filter(gid == input$map_shape_click$id) %>%
              pull(cdbh)
          }

          # reset others
          r_val$axis_name = NULL
          r_val$axis_strahler = NULL
          r_val$axis_id = NULL
          r_val$swath_id = NULL

          # clean map
          r_val$map_proxy %>%
            clearGroup(globals$map_group_params[["dgo_axis"]]) %>%
            clearGroup(globals$map_group_params[["axis_start_end"]]) %>%
            clearGroup(globals$map_group_params[["dgo"]])

        }
      }

      #### Axis ####
      if (input$map_shape_click$group == globals$map_group_params[["axis"]]) {

        # signal that axis is clicked for first time
        r_val$axis_clicked = TRUE

        # check if axis not already selected
        if (!is.null(r_val$axis_id) && (r_val$axis_id == input$map_shape_click$id)) {

        } else {

          # get axis id
          r_val$axis_id = input$map_shape_click$id

          # get axis name
          r_val$axis_name = globals$axes() %>%
            filter(axis == r_val$axis_id) %>%
            pull(toponyme)

          # get axis strahler order
          r_val$axis_strahler = max(globals$axis_data() |> pull(strahler))

          # reset others
          r_val$swath_id = NULL
          r_val$swath_data_section = NULL
          r_val$swath_data_dgo = NULL


          # get start- and end-coordinates of axis
          r_val$axis_start_end = data_get_axis_start_end(dgo_axis = globals$axis_data())

          # add axis to map
          r_val$map_proxy %>%
            map_add_axes(globals$axes(), group = globals$map_group_params[["axis"]], selected_axis_id = r_val$axis_id) %>%
            map_add_axis_dgos(globals$axis_data(), group = globals$map_group_params[["dgo_axis"]]) %>%
            map_add_axis_start_end(axis_start_end = r_val$axis_start_end,
                                   group = globals$map_group_params[["axis_start_end"]]) %>%
            clearGroup(globals$map_group_params[["dgo"]])

          # check if axis is in other region than the one that is selected
          if (!is.null(r_val$region_id) && (r_val$region_id == (globals$axes() %>%
                                                                filter(axis == r_val$axis_id) %>%
                                                                pull(gid_region)))) {
          } else {

            # set region id
            r_val$region_id = globals$axes() %>%
              filter(axis == r_val$axis_id) %>%
              pull(gid_region)

            # set region name
            r_val$region_name = globals$regions %>%
              filter(gid == r_val$region_id) %>%
              pull(lbregionhy)

            # check if region is in other basin than the one that is selected
            if (!is.null(r_val$basin_id) && (r_val$basin_id == (globals$regions %>%
                                                                filter(gid == r_val$region_id) %>%
                                                                pull(cdbh)))) {
            } else {

              r_val$basin_name = globals$basins() %>%
                filter(cdbh == globals$regions %>%
                         filter(gid == r_val$region_id) %>%
                         pull(cdbh)) %>%
                pull(lbbh)

              r_val$basin_id = globals$regions %>%
                filter(gid == r_val$region_id) %>%
                pull(cdbh)
            }
          }
        }
      }

      #### Swath ####

      if (input$map_shape_click$group == globals$map_group_params[["dgo_axis"]]) {


        # check if axis not already selected
        if (!is.null(r_val$swath_id) && (r_val$swath_id == input$map_shape_click$id)) {

        } else {

          # get swath id
          r_val$swath_id = input$map_shape_click$id

          # get data with dgo id
          r_val$swath_data_section = data_get_elevation_profiles(selected_dgo_fid = r_val$swath_id, con = con)

          # get dgo clicked feature
          r_val$swath_data_dgo = globals$axis_data() %>%
            filter(fid == r_val$swath_id)

          # Highlight clicked DGO
          r_val$map_proxy %>%
            map_dgo_cross_section(selected_dgo = r_val$swath_data_dgo, group = globals$map_group_params[["dgo"]])
        }
      }
    })

    ##### Deselect button ####
    observeEvent(input$deselect_button, {
      r_val$basin_name = NULL
      r_val$basin_id = NULL
      r_val$region_name = NULL
      r_val$region_id = NULL
      r_val$axis_name = NULL
      r_val$axis_id = NULL
      r_val$axis_strahler = NULL
      r_val$axis_data_classified = NULL
      r_val$swath_id = NULL
      r_val$swath_data_section = NULL
      r_val$swath_data_dgo = NULL
      r_val$selection_text = NULL
      r_val$map_proxy %>%
        clearGroup(globals$map_group_params[["axis"]]) %>%
        clearGroup(globals$map_group_params[["dgo_axis"]]) %>%
        clearGroup(globals$map_group_params[["axis_start_end"]]) %>%
        clearGroup(globals$map_group_params[["dgo"]]) %>%
        map_add_axes(globals$axes(), group = globals$map_group_params[["axis"]])
    })


    #### DGO Mouseover ####

    # check for hover over dgo event
    observeEvent(input$map_shape_mouseover, {
      if (input$map_shape_mouseover$group == globals$map_group_params$dgo_axis && !is.null(input$map_shape_mouseover)){

        # extract dgo axis fid from map
        r_val$leaflet_hover_measure = globals$axis_data() %>%
          filter(fid == input$map_shape_mouseover$id) %>%
          pull(measure)
      } else {
        r_val$leaflet_hover_measure = NULL
      }
    })
  })
}
