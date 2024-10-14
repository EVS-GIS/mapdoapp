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
create_table_fluvialstyles <- function(params_styles) {

  table <- reactable(params_styles,
                     columns = list(
                       class_title = colDef(name = "Style", sortable = FALSE),
                       description = colDef(show = FALSE),  # Hide column
                       class_name = colDef(show = FALSE),  # Hide column
                       class_sld = colDef(show = FALSE),  # Hide column
                       sld_style = colDef(show = FALSE)  # Hide column
                     ),
                     details = function(index) {
                       htmltools::div(
                         style = "padding: 10px; margin-left: 84px; white-space: pre-wrap;",  # Add text indentation
                         # htmltools::strong("Details: "),
                         params_styles$description[index]  # Display the detail column for the specific row
                       )
                     }, selection = "single", defaultSelected = 1, onClick = "select",
                     highlight = TRUE)
}
