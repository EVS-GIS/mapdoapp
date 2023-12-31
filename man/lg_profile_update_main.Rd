% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_lg_profile.R
\name{lg_profile_update_main}
\alias{lg_profile_update_main}
\title{Generate list to update main plot in plotlyProxy}
\usage{
lg_profile_update_main(data, y, y_label, y_label_category)
}
\arguments{
\item{data}{A data frame containing the data to be plotted.}

\item{y}{The name of the y-axis variable to plot.}

\item{y_label}{The label for the y-axis.}

\item{y_label_category}{The category label for the y-axis.}
}
\value{
A list containing trace and layout lists to plot with plotlyProxy.
}
\description{
This function generates a list to update the main axe in existing plotly graph with plotlyProxy.
}
\details{
This function generates a main profile update plot with the specified data and axis labels.
}
\examples{
\dontrun{
data <- data.frame(
measure = 1:10,
selected_metric = rnorm(10, mean = 1)
)
output$scatter_plot <- renderPlotly({
  plot_ly(data, x = ~measure, y = ~selected_metric, type = 'scatter', mode = 'line')
})
data_updated <- data.frame(
  measure = 1:10,
  selected_metric = rnorm(10, mean = 1)
)
selected_metric_name <-  "my metric"
select_metric_category <-  "my metric category"
update_main_axe <-
  lg_profile_update_main(
    data = data_updated,
    y = data_updated[[selected_metric]],
    y_label = selected_metric_name,
    y_label_category = r_val$select_metric_category
  )
plotlyProxy("scatter_plot") \%>\%
  plotlyProxyInvoke("deleteTraces", 0) \%>\%
  plotlyProxyInvoke("addTraces", update_main_axe$trace, 0) \%>\%
  plotlyProxyInvoke("relayout", update_main_axe$layout)
}

}
