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


#' Check if two vectors are identical in length and content
#'
#' This function compares two vectors and returns `TRUE` if they have the same length and the same elements (irrespective of order).
#'
#' @param x First vector to compare.
#' @param y Second vector to compare.
#'
#' @return A logical value (`TRUE` or `FALSE`). Returns `TRUE` if both vectors have the same length and the same elements, otherwise `FALSE`.
#' @export
#'
#' @examples
#' is_vector_identical(c(1, 2, 3), c(3, 2, 1))         # TRUE
#' is_vector_identical(c(1, 2), c(1, 2, 3))            # FALSE
#' is_vector_identical(c("a", "b"), c("a", "b"))       # TRUE
#' is_vector_identical(c("a", "b"), c("a", "b", "c"))  # FALSE
utils_is_vector_identical <- function(x, y) {
  length(x) == length(y) && setequal(x, y)
}

