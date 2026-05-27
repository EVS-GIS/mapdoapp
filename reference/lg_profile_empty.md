# Create an empty longitudinal profile plot.

This function generates an empty longitudinal profile plot using the
'plot_ly' function from the 'plotly' package.

## Usage

``` r
lg_profile_empty()
```

## Value

An empty longitudinal profile plot with a specified title.

## Examples

``` r
# Create an empty longitudinal profile plot
empty_plot <- lg_profile_empty()
empty_plot

{"x":{"visdat":{"1d0b7c1f25d7":["function () ","plotlyVisDat"]},"cur_data":"1d0b7c1f25d7","attrs":{"1d0b7c1f25d7":{"mode":"lines","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":{"text":"Sélectionnez un cours d'eau sur la carte pour afficher le graphique","y":0.80000000000000004,"x":0.29999999999999999,"font":{"size":15}},"xaxis":{"domain":[0,1],"automargin":true,"zeroline":false},"yaxis":{"domain":[0,1],"automargin":true,"zeroline":false},"modebar":{"remove":["select2d","lasso2d","autoscale","zoomIn2d","zoomOut2d"]},"hovermode":"closest","showlegend":false},"source":"L","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displaylogo":false},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"data":[{"mode":"lines","type":"scatter","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
