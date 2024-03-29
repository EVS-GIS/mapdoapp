% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_utils.R
\name{utils_get_metric_type}
\alias{utils_get_metric_type}
\title{Get a named vector of metric types from the params metric list.}
\usage{
utils_get_metric_type(input_list)
}
\arguments{
\item{input_list}{A list of objects where each object has a 'metric_type_title' field.}
}
\value{
A named character vector with metric type titles as names and metric type values as values.
}
\description{
This function extracts metric types from a list of objects and returns a named
vector with metric type titles as names and metric type values as values.
}
\examples{
metric_types <- utils_get_metric_type(params_metrics_choice())

}
