#' Get Quantile Metrics for a Selected Region
#'
#' This function calculates quantile metrics (Q1, Q2, Q3, Q4, Q5) for a selected metric within a specified region.
#'
#' @param selected_region_id The ID of the selected region.
#' @param selected_metric The name of the selected metric.
#' @param con PqConnection to Postgresql database.
#'
#' @return A numeric vector containing quantile metrics (Q1, Q2, Q3, Q4, Q5) for the selected metric within the specified region.
#'
#' @examples
#' con <- db_con()
#' # get quantiles from active_channel_width metric
#' quantile_metrics <- sld_get_quantile_metric(selected_region_id = 11,
#'                                             selected_metric = "active_channel_width",
#'                                             con = con)
#' quantile_metrics
#' DBI::dbDisconnect(con)
#'
#' @importFrom DBI dbGetQuery sqlInterpolate dbQuoteIdentifier SQL
#'
#' @export
sld_get_quantile_metric <- function(selected_region_id, selected_metric, con) {
  sql <- "
      SELECT
        ROUND(percentile_cont(0) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q1,
        ROUND(percentile_cont(0.25) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q2,
        ROUND(percentile_cont(0.50) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q3,
        ROUND(percentile_cont(0.75) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q4,
        ROUND(percentile_cont(1) WITHIN GROUP (ORDER BY ?selected_metric ASC)::numeric, 1) AS q5
    FROM public.network_metrics
    WHERE gid_region = ?selected_region_id"

  query <- sqlInterpolate(con, sql,
                          selected_metric = DBI::dbQuoteIdentifier(con, selected_metric),
                          selected_region_id = DBI::SQL(selected_region_id))

  data <- DBI::dbGetQuery(conn = con, statement = query)
  vector <- c(data$q1, data$q2, data$q3, data$q3, data$q4, data$q5)

  return(vector)
}


#' Get Quantile Colors
#'
#' This function generates a color palette based on quantile breaks for data mapping.
#'
#' @param quantile_breaks A numeric vector containing quantile breaks.
#'
#' @return A character vector of colors generated based on quantile breaks.
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' con <- db_con()
#' # get quantiles from active_channel_width metric
#' quantile_metrics <- sld_get_quantile_metric(selected_region_id = 11,
#'                                             selected_metric = "active_channel_width",
#'                                             con = con)
#' DBI::dbDisconnect(con)
#' # get color from quantile
#' quantile_colors <- sld_get_quantile_colors(quantile_breaks = quantile_metrics)
#' quantile_colors
#'
#' @export
sld_get_quantile_colors <- function(quantile_breaks) {
  colors_palette <- colorRampPalette(c("#03045e", "#90e0ef"))(length(quantile_breaks))
  return(colors_palette)
}


#' Get Styled Layer Descriptor (SLD) for network metric layer
#'
#' This function generates a Styled Layer Descriptor (SLD) XML for styling network metric layer based on quantile breaks colors.
#'
#' @param breaks A numeric vector containing quantile breaks.
#' @param colors A character vector of colors generated based on quantile breaks.
#' @param metric The name of the selected metric.
#'
#' @return A character string containing the SLD XML for styling data visualization.
#'
#' @examples
#' con <- db_con()
#' # get quantiles from active_channel_width metric
#' quantile_metrics <- sld_get_quantile_metric(selected_region_id = 11,
#'                                             selected_metric = "active_channel_width",
#'                                             con = con)
#' DBI::dbDisconnect(con)
#'
#' # get color from quantile
#' quantile_colors <- sld_get_quantile_colors(quantile_breaks = quantile_metrics)
#' # create sld style
#' sld_style <- sld_get_style(breaks = quantile_metrics,
#'                            colors = quantile_colors,
#'                            metric = "active_channel_width")
#' sld_style
#'
#' @importFrom glue glue
#'
#' @export
sld_get_style <- function(breaks, colors, metric) {
  sld_begin <- glue::glue('<?xml version="1.0" encoding="UTF-8"?>
    <StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:se="http://www.opengis.net/se" xmlns:ogc="http://www.opengis.net/ogc">
      <NamedLayer>
      <se:Name>network_metrics</se:Name>
        <UserStyle>
        <se:Name>network_metrics</se:Name>
        <se:FeatureTypeStyle>')

  sld_end <- '
          </se:FeatureTypeStyle>
        </UserStyle>
      </NamedLayer>
    </StyledLayerDescriptor>'

  sld_rules <- character(0)  # Initialize an empty list to store rules



  for (i in 1:(length(breaks))) {
    if (i < length(breaks)) {
      sld_rule <- glue::glue('
      <se:Rule>
          <se:Name>{breaks[i]} - {breaks[i+1]}</se:Name>
          <se:Description>
            <se:Title>{breaks[i]} - {breaks[i+1]}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>{metric}</ogc:PropertyName>
                <ogc:Literal>{breaks[i]}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>{metric}</ogc:PropertyName>
                <ogc:Literal>{breaks[i+1]}</ogc:Literal>
              </ogc:PropertyIsLessThan>
            </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors[i]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors[i]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')
    } else {
      sld_rule <- glue::glue('
      <se:Rule>
          <se:Name> >= {breaks[i]}</se:Name>
          <se:Description>
            <se:Title> >= {breaks[i]}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>{metric}</ogc:PropertyName>
                <ogc:Literal>{breaks[i]}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors[i]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors[i]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')
    }
    sld_rules <- c(sld_rules, sld_rule)
  }

  sld <- paste0(sld_begin, paste(sld_rules, collapse = "\n"), sld_end)
  return(sld)
}


#' Create sld style for fluvial styles layers
#'
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette
#'
#' @return Style Layer Descriptor XML code for the styling of the network wms-layer based on the fluvial styles classifications
#'
#' @usage sld_get_fluvialstyles()
#'
sld_get_fluvialstyles <- function() {

  sld_begin <- glue::glue('<?xml version="1.0" encoding="UTF-8"?>
    <StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:se="http://www.opengis.net/se" xmlns:ogc="http://www.opengis.net/ogc">
      <NamedLayer>
      <se:Name>network_metrics</se:Name>
      <UserStyle>
      <se:Name>network_metrics</se:Name>
      <se:FeatureTypeStyle>')

  sld_end <- '
  </se:FeatureTypeStyle>
        </UserStyle>
      </NamedLayer>
    </StyledLayerDescriptor>'


  # strahler ----------------------------------------------------------------

  colors_strahler <- c("#64b5f6", "#1e88e5", "#1976d2", "#1565c0", "#0d47a1", "#0a2472")
  strahler_sld_rules <- character(0)

  for (i in 1:6) {
    strahler_sld_rule <- glue::glue('
      <se:Rule>
          <se:Name>{i}</se:Name>
          <se:Description>
            <se:Title>{i}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsEqualTo>
                <ogc:PropertyName>strahler</ogc:PropertyName>
                <ogc:Literal>{i}</ogc:Literal>
              </ogc:PropertyIsEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_strahler[i]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_strahler[i]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')
    strahler_sld_rules <- c(strahler_sld_rules, strahler_sld_rule)
  }

  sld_strahler <- paste(strahler_sld_rules, collapse = "\n")

  # Topographie -------------------------------------------------------------

  colors_topo <- c( "#bb3e03", "#e9d8a6", "#a3b18a",
               "#780000","#ee9b00", "#3a5a40") %>%
    setNames(
      c("Plaines de montagne",
        "Plaines de moyenne altitude",
        "Plaines de basse altitude",
        "Pentes de montagne",
        "Pentes de moyenne altitude",
        "Pentes de basse altitude")
    )



  sld_topographie <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_topo[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_topo[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{1000}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>talweg_slope</ogc:PropertyName>
                <ogc:Literal>{0.05}</ogc:Literal>
              </ogc:PropertyIsLessThan>
            </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_topo[1]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_topo[1]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_topo[4])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_topo[4])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{1000}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_slope</ogc:PropertyName>
                <ogc:Literal>{0.05}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
            </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_topo[4]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_topo[4]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_topo[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_topo[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
            <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{300}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{1000}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              </ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>talweg_slope</ogc:PropertyName>
                <ogc:Literal>{0.05}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_topo[2]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_topo[2]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_topo[5])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_topo[5])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
              <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{300}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{1000}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_slope</ogc:PropertyName>
                <ogc:Literal>{0.05}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
            </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_topo[5]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_topo[5]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_topo[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_topo[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
              <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{-50}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{300}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              </ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>talweg_slope</ogc:PropertyName>
                <ogc:Literal>{0.05}</ogc:Literal>
              </ogc:PropertyIsLessThan>
            </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_topo[3]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_topo[3]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_topo[6])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_topo[6])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
            <ogc:And>
              <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{-50}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>talweg_elevation_min</ogc:PropertyName>
                <ogc:Literal>{300}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>talweg_slope</ogc:PropertyName>
                <ogc:Literal>{0.05}</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
            </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_topo[6]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_topo[6]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')


  # Dominant Land use -------------------------------------------------------

  colors_lu_dominante <- c("#31572c", "#90be6d", "#ffbe0b", "#ae2012") %>%
    setNames(
      c("Forêt", "Praire", "Cultures", "Urbain et infrastructure")
    )


  sld_lu_dominante  <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_lu_dominante[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_lu_dominante[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
          <ogc:And>
          <ogc:And>
          <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_lu_dominante[[1]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_lu_dominante[[1]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_lu_dominante[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_lu_dominante[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
          <ogc:And>
          <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_lu_dominante[[2]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_lu_dominante[[2]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_lu_dominante[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_lu_dominante[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
          <ogc:And>
          <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_lu_dominante[[3]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_lu_dominante[[3]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_lu_dominante[4])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_lu_dominante[4])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
          <ogc:And>
          <ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_lu_dominante[[4]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_lu_dominante[[4]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')


  # Urban landuse -----------------------------------------------------------

  colors_urban <- colors <- c("#6a040f", "#ba181b", "#ffdd00", "#74c69d") %>%
    setNames(
      c("Fortement urbanisé", "Urbanisé",
        "Modérément urbanisé", "Presque pas/pas urbanisé")
    )

  sld_urban  <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_urban[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_urban[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:Literal>70</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_urban[[1]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_urban[[1]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_urban[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_urban[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:Literal>{70}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:Literal>40</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_urban[[2]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_urban[[2]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_urban[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_urban[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:Literal>{40}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:Literal>10</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_urban[[3]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_urban[[3]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_urban[4])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_urban[4])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:Literal>{10}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>built_environment_pc</ogc:PropertyName>
                <ogc:Literal>0</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_urban[[4]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_urban[[4]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')


  # Agricultural landuse ----------------------------------------------------


  sld_agriculture  <-

    colors_agriculture <- colors <- c("#6a040f", "#ba181b", "#ffdd00", "#74c69d") %>%
    setNames(
      c("Forte impact agricole", "Impact agricole élevé",
        "Impact agricole modéré", "Presque pas/pas Impact agricole")
    )

  sld_agriculture  <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_agriculture[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_agriculture[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:Literal>70</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_agriculture[[1]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_agriculture[[1]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_agriculture[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_agriculture[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:Literal>{70}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:Literal>40</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_agriculture[[2]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_agriculture[[2]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_agriculture[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_agriculture[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:Literal>{40}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:Literal>10</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_agriculture[[3]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_agriculture[[3]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_agriculture[4])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_agriculture[4])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:Literal>{10}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>crops_pc</ogc:PropertyName>
                <ogc:Literal>0</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_agriculture[[4]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_agriculture[[4]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')

  # Natural landuse ---------------------------------------------------------

  colors_nature <- colors <- c("#081c15", "#2d6a4f", "#74c69d", "#d8f3dc") %>%
    setNames(
      c("Très forte utilisation naturelle", "Forte utilisation naturelle",
        "Utilisation naturelle modérée", "Presque pas/pas naturelle")
    )



  sld_nature  <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_nature[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_nature[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>natural_open_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>70</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_nature[[1]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_nature[[1]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_nature[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_nature[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:Add>
                <ogc:PropertyName>natural_open_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>{70}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>natural_open_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>40</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_nature[[2]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_nature[[2]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_nature[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_nature[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:Add>
                <ogc:PropertyName>natural_open_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>{40}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>natural_open_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>10</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_nature[[3]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_nature[[3]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_nature[4])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_nature[4])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:Add>
                <ogc:PropertyName>natural_open_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>{10}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>natural_open_pc</ogc:PropertyName>
                <ogc:PropertyName>forest_pc</ogc:PropertyName>
                <ogc:PropertyName>grassland_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>0</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_nature[[4]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_nature[[4]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')

  # Confinement -------------------------------------------------------------

  colors_confinement <- c("#d9ed92", "#99d98c", "#168aad", "#184e77") %>%
    setNames(
      c("espace abondant", "modérement espace", "confiné", "très confiné")
    )

  sld_confinement  <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_confinement[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_confinement[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>idx_confinement</ogc:PropertyName>
                <ogc:Literal>0.7</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_confinement[[1]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_confinement[[1]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_confinement[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_confinement[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>idx_confinement</ogc:PropertyName>
                <ogc:Literal>{0.7}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>idx_confinement</ogc:PropertyName>
                <ogc:Literal>0.4</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_confinement[[2]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_confinement[[2]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_confinement[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_confinement[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>idx_confinement</ogc:PropertyName>
                <ogc:Literal>{0.4}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>idx_confinement</ogc:PropertyName>
                <ogc:Literal>0.1</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_confinement[[3]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_confinement[[3]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_confinement[4])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_confinement[4])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:PropertyName>idx_confinement</ogc:PropertyName>
                <ogc:Literal>{0.1}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:PropertyName>idx_confinement</ogc:PropertyName>
                <ogc:Literal>0</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_confinement[[4]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_confinement[[4]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')


  # Habitat -----------------------------------------------------------------

  colors_habitat <- c("#2d6a4f", "#99d98c", "#fff3b0", "#ba181b") %>%
    setNames(
      c("très bien connecté", "bien connecté", "moyen connecté", "faible / absente")
    )

  sld_habitat  <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_habitat[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_habitat[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>riparian_corridor_pc</ogc:PropertyName>
                <ogc:PropertyName>semi_natural_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>70</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_habitat[[1]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_habitat[[1]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_habitat[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_habitat[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:Add>
                <ogc:PropertyName>riparian_corridor_pc</ogc:PropertyName>
                <ogc:PropertyName>semi_natural_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>{70}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>riparian_corridor_pc</ogc:PropertyName>
                <ogc:PropertyName>semi_natural_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>40</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_habitat[[2]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_habitat[[2]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_habitat[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_habitat[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:Add>
                <ogc:PropertyName>riparian_corridor_pc</ogc:PropertyName>
                <ogc:PropertyName>semi_natural_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>{40}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>riparian_corridor_pc</ogc:PropertyName>
                <ogc:PropertyName>semi_natural_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>10</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_habitat[[3]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_habitat[[3]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_habitat[4])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_habitat[4])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:And>
              <ogc:PropertyIsLessThan>
                <ogc:Add>
                <ogc:PropertyName>riparian_corridor_pc</ogc:PropertyName>
                <ogc:PropertyName>semi_natural_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>{10}</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Add>
                <ogc:PropertyName>riparian_corridor_pc</ogc:PropertyName>
                <ogc:PropertyName>semi_natural_pc</ogc:PropertyName>
                </ogc:Add>
                <ogc:Literal>0</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_habitat[[4]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_habitat[[4]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')


  # Gravel bars -------------------------------------------------------------


  colors_gravel <- c("#603808", "#e7bc91", "#0077b6") %>%
    setNames(
      c("abundant", "moyennement présente", "absent")
    )

  sld_gravel  <- glue::glue('
      <se:Rule>
          <se:Name>{names(colors_gravel[1])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_gravel[1])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsGreaterThanOrEqualTo>
                <ogc:Div>
                <ogc:PropertyName>gravel_bars</ogc:PropertyName>
                <ogc:Add>
                <ogc:PropertyName>water_channel</ogc:PropertyName>
                <ogc:Literal>0.00001</ogc:Literal>
                </ogc:Add>
                </ogc:Div>
                <ogc:Literal>0.5</ogc:Literal>
              </ogc:PropertyIsGreaterThanOrEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_gravel[[1]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_gravel[[1]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_gravel[2])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_gravel[2])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
          <ogc:And>
          <ogc:PropertyIsLessThan>
                <ogc:Div>
                <ogc:PropertyName>gravel_bars</ogc:PropertyName>
                <ogc:Add>
                <ogc:PropertyName>water_channel</ogc:PropertyName>
                <ogc:Literal>0.00001</ogc:Literal>
                </ogc:Add>
                </ogc:Div>
                <ogc:Literal>0.5</ogc:Literal>
              </ogc:PropertyIsLessThan>
              <ogc:PropertyIsGreaterThan>
                <ogc:Div>
                <ogc:PropertyName>gravel_bars</ogc:PropertyName>
                <ogc:Add>
                <ogc:PropertyName>water_channel</ogc:PropertyName>
                <ogc:Literal>0.00001</ogc:Literal>
                </ogc:Add>
                </ogc:Div>
                <ogc:Literal>0</ogc:Literal>
              </ogc:PropertyIsGreaterThan>
              </ogc:And>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_gravel[[2]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_gravel[[2]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        <se:Rule>
          <se:Name>{names(colors_gravel[3])}</se:Name>
          <se:Description>
            <se:Title>{names(colors_gravel[3])}</se:Title>
          </se:Description>
          <ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">
              <ogc:PropertyIsEqualTo>
                <ogc:Div>
                <ogc:PropertyName>gravel_bars</ogc:PropertyName>
                <ogc:Add>
                <ogc:PropertyName>water_channel</ogc:PropertyName>
                <ogc:Literal>0.00001</ogc:Literal>
                </ogc:Add>
                </ogc:Div>
                <ogc:Literal>0</ogc:Literal>
              </ogc:PropertyIsEqualTo>
          </ogc:Filter>
          <se:LineSymbolizer>
            <se:Stroke>
              <se:SvgParameter name="stroke">{colors_gravel[[3]]}</se:SvgParameter>
              <se:SvgParameter name="stroke-width">2</se:SvgParameter>
              <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
              <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
              <CssParameter name="stroke">{colors_gravel[[3]]}</CssParameter>
              <CssParameter name="stroke-width">2</CssParameter>
            </se:Stroke>
          </se:LineSymbolizer>
        </se:Rule>
        ')


  # Channel evolution -------------------------------------------------------


  sld_channelevolution  <- NULL


  # join all together -------------------------------------------------------

  sld_final <- tibble(
    class_name = c("class_strahler", "class_topographie", "class_lu_dominante", "class_urban", "class_agriculture",
                   "class_nature", "class_confinement", "class_habitat", "class_gravel", "class_channelevolution"),
    class_sld = c(paste0(sld_begin, sld_strahler, sld_end),
                  # "", "", "", ""
                  paste0(sld_begin, sld_topographie, sld_end),
                  paste0(sld_begin, sld_lu_dominante, sld_end),
                  paste0(sld_begin, sld_urban, sld_end),
                  paste0(sld_begin, sld_agriculture, sld_end),
                  paste0(sld_begin, sld_nature, sld_end),
                  paste0(sld_begin, sld_confinement, sld_end),
                  paste0(sld_begin, sld_habitat, sld_end),
                  paste0(sld_begin, sld_gravel, sld_end),
                  paste0(sld_begin, sld_channelevolution, sld_end))
  )

  return(sld_final)
}
