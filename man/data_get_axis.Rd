% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data.R
\name{data_get_axis}
\alias{data_get_axis}
\title{Get Network Axis Data for a Region}
\usage{
data_get_axis(selected_region_id, con)
}
\arguments{
\item{selected_region_id}{The ID of the selected region.}

\item{con}{PqConnection to Postgresql database.}
}
\value{
A sf data frame containing information about the network axis within the specified region.
}
\description{
This function retrieves data about the network axis within a specified region based on its ID.
}
\examples{
con <- db_con()
axis_data <- data_get_axis(selected_region_id = 11, con = con)
DBI::dbDisconnect(con)

}
