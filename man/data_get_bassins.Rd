% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_data.R
\name{data_get_bassins}
\alias{data_get_bassins}
\title{Get Hydrographic Basins}
\usage{
data_get_bassins(opacity)
}
\arguments{
\item{opacity}{A list that contain numeric values clickable and not_clickable to inform the user the non available features.}
}
\value{
A sf data frame containing information about hydrographic basins.
}
\description{
This function retrieves hydrographic basins.
}
\examples{
opacity = list(clickable = 0.01,
               not_clickable = 0.10)

data <- data_get_bassins(opacity = opacity)

}
