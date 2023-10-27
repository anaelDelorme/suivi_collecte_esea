#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import waiter
#' @import bs4Dash 
#' @import shinymanager
#' @noRd
app_ui <- function(request) {
  tagList(
    # TOUJOURS laisser cette fonction
    golem_add_external_resources(),
    # Création de la page
    bs4Dash::dashboardPage(
      # Titre général
      title = "Suivi collecte ESEA",
      # Ajouter le bouton en haut à droite permettant d'ouvrir le site en page complète
      fullscreen = TRUE,
      preloader = list(html = waiter::spin_flower(), color = "#6610f2"),
      # Création du header
      header = bs4Dash::dashboardHeader(
        # Titre du header qui se met en haut à gauche du site
        title = "Suivi collecte ESEA",
        # Couleur du header --> choix de couleurs dans le help de bs4Dash : ?bs4Dash::dashboardHeader
        status = "indigo",
        # logo à gauche du header qui permet d'ouvrir et fermer le menu latéral
        # possibilité d'utiliser les logos de bootstrap : https://fontawesome.com/icons ou https://getbootstrap.com/docs/3.3/components/#glyphicons
        sidebarIcon = shiny::icon("tractor")
      ),
      # Création du menu latéral
      sidebar = bs4Dash::dashboardSidebar(
        # Couleur du menu latéral
        status = "indigo",
        # id à laisser
        id = "sidebar",
        # ajout de la liste des liens dans le menu
        bs4Dash::sidebarMenu(
          id = "sidebarmenu",
          # pour chaque page à afficher mettre un menuItem avec le libellé affiché
          # le tabName qui fait le lien avec l'affichage dans le body
          # le logo
          bs4Dash::menuItem("Suivi Collecte", tabName = "suiviCollecte", icon = icon("globe")),
          bs4Dash::menuItem("Liste des dossiers", tabName = "listeDossiers", icon = icon("list"))

        ),
        img(src = app_sys("logo_tfc.png"))
      ),
      # Ajout du body
      body = bs4Dash::dashboardBody(
        # liste des pages à afficher
        bs4Dash::tabItems(
          # pour chaque page on met un tabItem avec le nom du tabName du menuItem
          # et la fonction d'affchage de la page (cette fonction est expliquée dans la suite : Ajouter des modules)
          bs4Dash::tabItem("suiviCollecte", mod_suivi_collecte_ui("suivi_collecte_1")),
          bs4Dash::tabItem("listeDossiers", mod_liste_dossier_ui("liste_dossier_1"))
        )
      ),
      # Ajout d'un footer avec un texte à gauche et à droite dans cet exemple.
      footer = bs4Dash::dashboardFooter(
        left = "Equipe ESEA",
        right = "2023-2024"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "suiviCollecte"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
