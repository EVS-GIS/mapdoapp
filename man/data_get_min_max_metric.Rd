% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data.R
\name{data_get_min_max_metric}
\alias{data_get_min_max_metric}
\title{Get Minimum and Maximum Metric Values for a Selected Region}
\usage{
data_get_min_max_metric(selected_region_id, selected_metric)
}
\arguments{
\item{selected_region_id}{The ID of the selected region.}

\item{selected_metric}{The name of the selected metric.}
}
\value{
A data frame containing two columns: 'min' and 'max', representing the minimum and maximum values of the selected metric for the specified region.
}
\description{
This function retrieves the minimum and maximum values of a selected metric for a specified region
}
\examples{
data <- data_get_min_max_metric(selected_region_id = 11, selected_metric = "active_channel_width")

}
