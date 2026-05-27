#' table_fluvialstyles
#'
#' @description A fct function
#'
#' @importFrom reactable reactable colDef
#' @importFrom htmltools div
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
create_table_fluvialstyles <- function() {
  data_classes=params_classes()
  data(metric_info)
  table <- reactable(data_classes,
                     columns = list(
                       class_title = colDef(name = "Classification", sortable = FALSE),
                       description = colDef(show = FALSE),  # Hide column
                       class_name = colDef(show = FALSE),  # Hide column
                       # class_sld = colDef(show = FALSE),  # Hide column
                       sld_style = colDef(show = FALSE)  # Hide column
                     ),
                     details = function(index) {
                       htmltools::div(
                         style = "padding: 10px; margin-left: 84px; white-space: pre-wrap;",  # Add text indentation
                         # htmltools::strong("Details: "),
                         metric_info$metric_description[index]  # Display the detail column for the specific row
                       )
                     }, selection = "single", defaultSelected = 1, onClick = "select",
                     highlight = TRUE)
}
