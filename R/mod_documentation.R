#' documentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_documentation_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
      fluidRow(
        column(
          width = 9,
          h1("Le projet"),
          HTML("<p style='text-align: justify;'>
          Le projet <b>Mapd'O</b> vise à développer une interface web permettant aux opérateurs et aux scientifiques d'analyser les données et les modèles hydromorphologiques du réseau hydrographique français. Le projet est porté par le laboratoire <i>CNRS UMR 5600 - Environnement Ville Société (EVS)</i> et est soutenu par <i>l'Office Français de la Biodiversité</i> depuis janvier 2023.<br> <br>

          L'application est basée sur les résultats de la <a href='https://tramebleue.github.io/fct-cli/' target='_blank' style='text-decoration: underline;'>Fluvial Corridor Toolbox (FCT)</a> appliquée au réseau hydrographique français. Également développée par le laboratoire EVS, la FCT est une boîte à outils de système d'information géographique pour l'hydromporphologie à grande échelle, fournissant des approches géomatiques à l'analyse de la topographie et de l'occupation des sols, et permettant la production de cartes de continuité latérale et d'autres caractéristiques fluviales. Les couches de fond de vallée et de corridor fluvial sont d'abord extraites d'un modèle numérique d'élévation. Diverses mesures morphologiques, telles que l'occupation et l'utilisation des sols, la largeur, la pente, la surface drainée, l'altitude, sont ensuite extraites du réseau à intervalles réguliers en fusionnant les données avec les cartes d'occupation des sols. <br> <br>

          Avec <b>Mapd'O</b>, ces données peuvent être interprétées et analysées pour faciliter les diagnostics hydromorphologiques des rivières françaises à différentes échelles, telles que l'ensemble du bassin versant des principales rivières françaises, mais aussi leur sous-bassin versant et l'échelle de l'axe individuel de la rivière. </p>
               ")
        ),
        column(
          width = 3,
          h2("Contact"),
          HTML("
            Lise Vaudor <br>
            <a href='mailto:lise.vaudor@ens-lyon.fr?subject=Application%20MAPDO'>lise.vaudor@ens-lyon.fr</a> <br>
            CNRS UMR 5600, Environnement Ville Soci&eacute;t&eacute; <br>
            <a href='https://umr5600.cnrs.fr/' target='_blank'>https://umr5600.cnrs.fr/</a></p>
         ")
        )
      ),


      tags$a(
        href = "https://evs-gis.github.io/mapdowebsite/",
        icon("book"),
        "Cliquez ici pour ouvrir la documentation et en savoir plus sur le projet et sa méthodologie.",
        target = "_blank"
      )
    )
  )
}

#' documentation Server Functions
#'
#' @noRd
mod_documentation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_documentation_ui("documentation_1")

## To be copied in the server
# mod_documentation_server("documentation_1")
