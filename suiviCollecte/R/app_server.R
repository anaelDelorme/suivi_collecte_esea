#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinymanager
#' @import arrow
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      data.frame(
        user = c(Sys.getenv("LOGIN_SITE_1")), # mandatory
        password = c(Sys.getenv("MDP_SITE_1")), # mandatory
        admin = c(FALSE),
        stringsAsFactors = FALSE
      )
    )
  )



  r <- reactiveValues()

  observe({
    r$data_suivi <- aws.s3::s3read_using(
      FUN = arrow::read_parquet,
      object = "ESEA/DON_SUIVI.parquet",
      bucket = "projet-suivi-collecte-masa",
      opts = list("region" = "")
    )

   # r$nb_dossier <- aws.s3::s3read_using(
    #  FUN = arrow::read_parquet,
     # object = "ESEA/NBDOSSIER.parquet",
     # bucket = "projet-suivi-collecte-masa",
     # opts = list("region" = "")
    #)

    regions <- geojsonio::topojson_read("https://raw.githubusercontent.com/neocarto/resources/master/geometries/France/regions.topojson")
    r$map_regions <- regions
    suppressWarnings({
      france_regions_centroid <- sf::st_centroid(regions)
    })
    regions$centroid_longitude <- sf::st_coordinates(france_regions_centroid)[, 1]
    regions$centroid_latitude <- sf::st_coordinates(france_regions_centroid)[, 2]

    r$map_regions_centroid <- regions

    departements <- geojsonio::topojson_read("https://raw.githubusercontent.com/neocarto/resources/master/geometries/France/departements.topojson")    
    suppressWarnings({
      france_departements_centroid <- sf::st_centroid(departements)
    })
    r$map_departements <- departements
    departements$centroid_longitude <- sf::st_coordinates(france_departements_centroid)[, 1]
    departements$centroid_latitude <- sf::st_coordinates(france_departements_centroid)[, 2]
    r$map_departements_centroid <- departements

  })
  # Your application server logic


  mod_suivi_collecte_server("suivi_collecte_1",r)
  mod_suivi_accept_server("suivi_accept_1",r)
  mod_suivi_etat_server("suivi_etat_1",r)
  mod_suivi_validation_srise_server("suivi_validation_srise_1",r)
}
