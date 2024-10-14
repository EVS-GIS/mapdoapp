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
      HTML("
            <p>MAPD&#39;O est un projet visant le d&eacute;veloppement d&#39;une interface web &agrave; l&#39;intention des op&eacute;rateurs autour des donn&eacute;es et mod&egrave;les en hydromorphologie. C&#39;est un projet r&eacute;alis&eacute; par le laboratoire Environnement Ville Soci&eacute;t&eacute; du CNRS et port&eacute; par l&#39;Office Fran&ccedil;ais de la Biodiversit&eacute; depuis janvier 2023.</p>

            <p>L&#39;application repose sur des approches g&eacute;omatiques d&#39;analyse de la topographie et de l&#39;occupation du sol permettant de produire une carte de continuit&eacute; lat&eacute;rale des cours d&#39;eau du r&eacute;seau hydrographique. Diff&eacute;rentes m&eacute;triques morphologiques sont ensuite extraites &agrave; intervalle r&eacute;gulier du r&eacute;seau (largeur, pente, surface drain&eacute;e, &eacute;l&eacute;vation). L&#39;application a pour ambition de fournir des outils d&#39;analyse et d&#39;interpr&eacute;tation de ces m&eacute;triques afin de faciliter les diagnostics hydromorphologiques des cours d&#39;eau fran&ccedil;ais &agrave; l&#39;&eacute;chelle du bassin versant.</p>

            <p>L&#39;application dispose aujourd&#39;hui d&#39;un module d&#39;exploration des donn&eacute;es permettant de visualiser les diff&eacute;rentes m&eacute;triques mises &agrave; disposition pour l&#39;analyse de bassin versant.</p>
           "),
      HTML("
            <p><strong>Contact</strong> : Lise Vaudor (CNRS UMR 5600, Environnement Ville Soci&eacute;t&eacute;) - <a href='mailto:louis.maniere@ens-lyon.fr?subject=Application%20MAPDO'>lise.vaudor@ens-lyon.fr</a></p>
         "),
      tags$a(
        href = "https://evs-gis.github.io/mapdowebsite/",
        icon("book"),
        "Toute la documentation est ici",
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
