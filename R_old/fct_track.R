#' Track input user inputs.
#'
#' @param input reactivesValues Shiny input.
#'
#' @importFrom jsonlite toJSON
#'
#' @return message with the inputs name and value as json format.
#' @export
track_inputs <- function(input = input){
  inputs_tracked <- c("exploremap_shape_click", "strahler", "metricfilter", "metric_type",
                      "metric", "unit_area", "roe_profile", "profile_metric_type",
                      "profile_metric", "profile_unit_area", "roe_profile",
                      "remove_profile_axe")

  values <- list()
  for (name in names(input)) {
    if (grepl(paste(inputs_tracked, collapse = "|"), name)) {
      values[[name]] <- input[[name]]
    }
  }
  return(message(toJSON(list(values))))
}

#' Track input user session id and datetime.
#'
#' @param session reactivesValues Shiny input.
#'
#' @importFrom jsonlite toJSON
#'
#' @return message with the session info name and value as json format.
#' @export
track_session <- function(session = session){
  session_tracks <- list(
    session_id = session$token,
    session_time = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3%z")
  )
  message(toJSON(list(session_tracks)))
}
