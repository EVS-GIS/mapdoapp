#' help_guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom cicerone use_cicerone
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_guide_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("help_btn"), " Aide",
                 icon = icon("circle-question")),
    use_cicerone() # Load the cicerone dependencies
  )
}

#' help_guide Server Functions
#'
#' @importFrom cicerone Cicerone
#'
#' @noRd
mod_help_guide_server <- function(id, r_val){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    # Set up a dynamic observer based on the active tab
    observeEvent(input$help_btn, {

      # Reset tour steps for each new tab
      # tour$reset()

      # Define the cicerone tours for each tab
      tour <- Cicerone$new(
        done_btn_text = "Terminer",
        close_btn_text = "Fermer",
        next_btn_text = "Suivant",
        prev_btn_text = "Précédent",
      )

      # browser()

      #| notes:
        #| <p> for paragraph, <br/> for line break
        #| <strong> for bold, <em> for italics, and <ul> for bullet points
      #| <a href="https://www.google.com">Google</a> for links
      #| <img src="www/logos_mapdo_evs_ofb.png" width="100px"> for images
      #| <code> for code snippets
      #|



      ### Exploration-Tab tour ####
      if (r_val$tab_page == "Exploration & Classification") {
        tour$
          step("explore_1-map", "Carte",
               description = "Vue d'ensemble du réseau fluvial disponible, avec différentes fonctionnalités : <p>
                  <strong> Sélection de couches</strong> (en haut à droite) : <br/>
                    - <em>Les couches de base</em>, y compris l'imagerie satellitaire, les cartes géologiques et d'occupation des sols, et le plan IGN.<br/>
                    - <em>Couches fonctionnelles,</em> comprenant les entités géographiques des bassins et des régions de sous-bassins, l'emplacement des obstacles aux cours d'eau, les jauges de débit, ainsi que les ensembles de données sous-jacents de la caractérisation géomorphologique du réseau fluvial. Il s'agit notamment du MNT relatif, du fond de vallée et des cartes de continuité latérale. En outre, les cartes des risques d'inondation et des infrastructures de protection contre les inondations sont incluses. <p>
                  <strong> Fonctionnalités de la carte</strong> (en haut à gauche) :<br/>
                    - Zoom avant et arrière, mode plein écran <br/>
                    - Recherche de lieux/rivières et géolocalisation automatique  <br/>
                    - Lien vers le service <em>Remonter le temps</em> de l'IGN, où la même étendue de carte est ouverte pour être analysée avec des cartes et des images satellites historiques et actuelles.",
               position = "right")

        if (r_val$selection_text != "") {
          tour$step("explore_1-selection_textUI", "Sélection actuelle", "Noms du bassin, de la région et de l'axe hydrographique actuellement sélectionnés.",
                    position = "right")
        }

        if (r_val$tab_classes == "Classes proposées") {
          tour$step("expl_classes_proposed_1-table", "Classes proposées",
                    description = "Sélectionnez l'une des classifications proposées pour l'appliquer à la carte. Cela ajoute une variable de classe au réseau afin que ces classifications puissent être utilisées dans des analyses ultérieures, par exemple sous l'onglet « Analyse ». Cliquez sur le petit triangle situé à côté du nom de la classification pour obtenir plus d'informations sur la manière dont la classification est établie.",
                    position = "left")
        }

        if (r_val$tab_classes == "Classification manuelle") {
          tour$step("expl_classes_manual_1-manual_classificationUI", "Classification manuelle",
                    description = "Créez une classification basée sur une mesure spécifique. Choisissez d'abord une métrique, puis éditez les noms de classe, les seuils et les couleurs pour chaque classe. Les seuils initiaux des classes sont, pour exclure les valeurs aberrantes, basés sur le quantile à 95 % de la métrique correspondante pour l'ensemble du réseau français. En cliquant sur la flèche en haut à droite, vous pouvez ajuster l'étendue du quantile ou la base de l'échelle. En outre, vous pouvez définir le nombre de classes que vous souhaitez appliquer. Des informations sur la métrique sélectionnée peuvent être obtenues en cliquant sur le bouton info à côté de la sélection de la métrique.",
                    position = "left")
        }

        if (r_val$tab_plots == "Évolution longitudinale") {
          tour$step("expl_plot_long_1-longitudinal_profile", "Évolution longitudinale",
                    description = "En sélectionnant un axe fluvial, ce module permet de visualiser une ou deux métriques et leur évolution depuis la source jusqu'à l'exutoire ou la confluence. Sur le côté droit, vous pouvez sélectionner les métriques et ajouter la position des obstacles dans le cours d'eau (uniquement les barrages) à la figure. En outre, il est possible d'ajouter la classification appliquée en arrière-plan. Dans ce cas, une fonctionnalité expérimentale est incluse qui permet l'homogénéisation de ces classifications, envisageant la détection de sections homogènes de la rivière.",
                    position = "top")
        }

        if (r_val$tab_plots == "Profil transversal") {
          tour$step("expl_plot_crosssection_1-cross_section", "Profil transversal",
                    description = "En sélectionnant un tronçon de rivière sur la carte, ce module affiche son profil transversal médian. Les altitudes des deux rives sont affichées par rapport à l'altitude du talweg.",
                    position = "top")
        }
      }

      ### Analyse-Tab tour ####
      if (r_val$tab_page == "Analyse") {

        # current selection analysis tab
        if (r_val$tab_analysis == "Caractérisation de la sélection") {
          if (r_val$selection_text != "") {
            tour$step("analysis_1-selection_textUI", "Sélection actuelle", "Noms du bassin, de la région et de l'axe hydrographique actuellement sélectionnés.",
                      position = "bottom")
          }

          tour$step("analysis_1-selact_tableUI", "Comparaison de métriques multi-échelles",
                    description = "Ce tableau permet de comparer les valeurs moyennes et les distributions statistiques des indicateurs entre les entités hydrographiques sélectionnées. La valeur moyenne est imprimée pour chaque indicateur et en survolant les distributions statistiques, plus d'informations sur la médiane, les quantiles et les valeurs aberrantes sont imprimées. En cliquant sur les colonnes, il est possible de modifier l'ordre.",
                    position = "right")

          tour$step("analysis_1-selact_modifications", "Sélection de l'ordre de Strahler et de la métrique",
                    description = "Sélectionnez les métriques que vous souhaitez comparer dans le tableau. Sélectionnez les ordres de Strahler pour subdiviser les distributions statistiques pour chaque entité hydrographique. ",
                    position = "left")

          tour$step("analysis_1-selact_plotUI", "Visualisation de la distribution des classes",
                    description = "Visualisation de la distribution de la classification appliquée pour chaque entité hydrographique. Cliquez sur les classes individuelles dans la légende pour les activer/désactiver.",
                    position = "top")
        }

        # regions analysis tab
        if (r_val$tab_analysis == "Comparaison des Régions") {

          tour$step("analysis_1-regions_table", "Comparaison de métriques entre les régions hydrographiques françaises",
                    description = "Ce tableau permet de comparer les valeurs moyennes et les distributions statistiques des indicateurs entre les different régions hydrographiques de la France. La valeur moyenne est imprimée pour chaque indicateur et en survolant les distributions statistiques, plus d'informations sur la médiane, les quantiles et les valeurs aberrantes sont imprimées. En cliquant sur les colonnes, il est possible de modifier l'ordre.",
                    position = "right")

          tour$step("analysis_1-regions_modifications", "Sélection de l'ordre de Strahler et de la métrique",
                    description = "Sélectionnez les métriques que vous souhaitez comparer dans le tableau. Sélectionnez les ordres de Strahler pour subdiviser les distributions statistiques pour chaque région. ",
                    position = "left")

          tour$step("analysis_1-regions_plotUI", "Visualisation de la distribution des classes",
                    description = "Visualisation de la distribution de la classification appliquée pour chaque région. Cliquez sur les classes individuelles dans la légende pour les activer/désactiver.",
                    position = "top")
        }

        # regions analysis tab
        if (r_val$tab_analysis == "Analyse Bimétrique") {

          tour$step("analysis_bimetric_1-plot", "Graphique de la relation entre les métriques",
                    description = "Ce graphique permet à l'utilisateur de comparer les tronçons de l'axe sélectionné en fonction de deux métriques. En survolant les points de données, on obtient des informations sur chaque tronçon, notamment la distance par rapport à l'exutoire de la rivière (position) et les valeurs des métriques. ",
                    position = "right")

          tour$step("analysis_bimetric_1-analysis_settings", "Paramètrage d'analyse",
                    description = "Sélectionnez les métriques que vous souhaitez analyser. En outre, vous pouvez colorer les points de données selon la classification appliquée sélectionnée dans l'onglet « Exploration & Classification » et appliquer un modèle de relation linéaire entre les deux métriques, en l'ajoutant sous forme de ligne à la graphique. ",
                    position = "left")

        }
      }

      ### Télechargement-Tab tour ####
      if (r_val$tab_page == "Télechargement") {
        tour$step("download_1-download_selection", "Sélection des données à télécharger",
                  description = "Cette section vous permet de sélectionner le type de jeu de données à télécharger et l'échelle, notamment pour le bassin, la région ou l'axe sélectionné. ",
                  position = "right")

        tour$step("download_1-download_info", "Informations sur le jeu de données et bouton de téléchargement",
                  description = "Les informations sur l'ensemble des données sélectionnées sont préparées ici. En cliquant sur le bouton, il est possible de télécharger les données au format csv.",
                  position = "left")


        if (!is.null(r_val$dataset_input)) {
          tour$step("download_1-table_data", "Aperçu des données",
                    description = "Aperçu des 10 premières lignes de l'ensemble de données avec toutes les colonnes disponibles.",
                    position = "left")
        }


      }

      tour$init()$start() # Start the tour
    })
  })
}
