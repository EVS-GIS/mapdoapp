#' Normalize a string by removing spaces, accents, and special characters.
#'
#' This function takes an input string and normalizes it by removing spaces, accents, diacritics,
#' and special characters. It then converts the string to lowercase.
#'
#' @param input_string The input string to be normalized.
#'
#' @return The normalized string with spaces, accents, and special characters removed, and in lowercase.
#'
#' @examples
#' original_string <- "Thïs is à sâmplè strîng with spèciál chàracters!"
#' normalized_string <- utils_normalize_string(original_string)
#' cat(normalized_string)  # "thisisasamplestringwithspecialcharacters"
#'
#' @export
utils_normalize_string <- function(input_string) {
  # Remove accents and diacritics
  input_string <- chartr("ÀÁÂÃÄÅàáâãäåÒÓÔÕÖØòóôõöøÈÉÊËèéêëÌÍÎÏìíîïÙÚÛÜùúûüÝýÑñÇç",
                         "AAAAAAaaaaaaOOOOOOooooooEEEEeeeeIIIIiiiiUUUUuuuuYyNnCc",
                         input_string)

  # Remove spaces and special characters
  input_string <- gsub("[^a-zA-Z0-9]", "", input_string)

  # Convert to lowercase
  normalized_string <- tolower(input_string)

  return(normalized_string)
}

#' Get a named vector of metric types from the params metric list.
#'
#' This function extracts metric types from a list of objects and returns a named
#' vector with metric type titles as names and metric type values as values.
#'
#' @param input_list A list of objects where each object has a 'metric_type_title' field.
#'
#' @importFrom stats setNames
#'
#' @return A named character vector with metric type titles as names and metric type values as values.
#'
#' @examples
#' metric_types <- utils_get_metric_type(params_metrics_choice())
#'
#' @export
utils_get_metric_type <- function(input_list) {
  metric_type <- setNames(names(sapply(input_list, function(x) x$metric_type_title)), sapply(input_list, function(x) x$metric_type_title))
  return(metric_type)
}

#' Get a named vector of all the metric from the metric type from the params metric list.
#'
#' This function extracts metric names and value with metric type stored in params metric list.
#'
#' @param metric_type A character with metric type.
#'
#' @return A named character vector with all the metric names and values.
#'
#' @examples
#' metrics <- utils_get_metric_name_value("largeur")
#'
#' @export
utils_get_metric_name_value <- function(metric_type){
  metric_name <- sapply(params_metrics_choice()[[metric_type]]$metric_type_value, function(x) x$metric_title)
  return(metric_name)
}

#' Create buttons with popover for metric labels.
#'
#' This function generates a list of buttons with popovers that display additional
#' information for each metric label.
#'
#' @param metric_type A character string specifying the metric type.
#'
#' @importFrom bslib popover
#' @importFrom bsicons bs_icon
#'
#' @return A list of buttons with popovers.
#'
#' @details This function uses the `params_metrics_choice` data structure to
#' extract metric labels and corresponding popover information for the specified
#' metric type.
#'
#' @examples
#' metric_buttons <- utils_button_label_with_popover("largeur")
#'
#' @export
utils_button_label_with_popover <- function(metric_type) {
  metrics <-
    names(params_metrics_choice()[[metric_type]]$metric_type_value)
  buttons <- list()
  for (var in metrics) {
    metric_title <-
      params_metrics_choice()[[metric_type]]$metric_type_value[[var]]$metric_title
    # popover to display
    metric_popover <-
      params_metrics_choice()[[metric_type]]$metric_type_value[[var]]$metric_info
    bttn <-
      list(
        div(
          # metric title to display next to the radioButton
          metric_title,
          style = "display: inline; align-items: center;",
          # info icon with popover label
          span(
            style = "display: inline; align-items: center",
            popover(
              trigger = bs_icon("info-circle"),
              metric_popover,
              placement = "right",
              id = "popover_metric"
            )
          )
        )
      )

    buttons <- append(buttons, bttn)
  }
  return(buttons)
}

#' get IGN remonterletemps url.
#'
#' This function take longitude and latitude to build and url to go IGN remonterletemps website on the same place.
#'
#' @param lng longitude.
#' @param lat latitude.
#' @param zoom zoom level.
#'
#' @importFrom glue glue
#'
#' @return a string with url link.
#' @export
#'
#' @examples
#' utils_url_remonterletemps(lng=6.869433, lat=45.923690, zoom = 12)
utils_url_remonterletemps <- function(lng=6.869433,
                                       lat=45.923690,
                                       zoom = 12){
  url <- glue::glue("https://remonterletemps.ign.fr/comparer/basic?x={lng}&y={lat}&z={zoom}&layer1=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&layer2=ORTHOIMAGERY.ORTHOPHOTOS&mode=vSlider")
  return(url)
}

#' Get a named vector of all the metric from the metric type from the params metric list.
#'
#' This function extracts metric names and value with metric type stored in params metric list.
#'
#' @param metric_type A character with metric type.
#'
#' @return A named character vector with all the metric names and values.
#'
#' @examples
#' metrics <- utils_get_metric_name_value_analysis("largeur")
#'
#' @export
utils_get_metric_name_value_analysis <- function(metric_type){
  metric_name <- sapply(params_metrics_choice_analysis()[[metric_type]]$metric_type_value, function(x) x$metric_title)
  return(metric_name)
}
