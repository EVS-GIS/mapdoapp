% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data.R
\name{data_get_region}
\alias{data_get_region}
\title{Get hydrological region selected by user}
\usage{
data_get_region(region_click_id, con)
}
\arguments{
\item{region_click_id}{The ID of the selected region.}

\item{con}{PqConnection to Postgresql database.}
}
\value{
A sf data frame containing hydrographical data for the specified region.
}
\description{
This function retrieves hydrographical data for a specified region based on its ID.
}
\examples{
con <- db_con()
data <- data_get_region(region_click_id = 11, con = con)
DBI::dbDisconnect(con)

}
