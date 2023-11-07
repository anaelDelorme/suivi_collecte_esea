#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom aws.s3 s3read_using
#' @importFrom arrow read_parquet
#' @import geojsonio
#' @import shinymanager
#' @noRd

app_server <- function(input, output, session) {
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      data.frame(
        user = c("id"), # mandatory
        password = c("123456"), # mandatory
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

    r$nb_dossier <- aws.s3::s3read_using(
       FUN = arrow::read_parquet,
        object = "ESEA/NBDOSSIER.parquet",
        bucket = "projet-suivi-collecte-masa",
        opts = list("region" = "")
        )  

    r$map_regions <- geojsonio::topojson_read("https://raw.githubusercontent.com/neocarto/resources/master/geometries/France/regions.topojson")

    r$map_departements <- geojsonio::topojson_read("https://raw.githubusercontent.com/neocarto/resources/master/geometries/France/departements.topojson")

  })
 # Your application server logic
  

  mod_suivi_collecte_server("suivi_collecte_1",r)
  mod_suivi_accept_server("suivi_accept_1",r)
  mod_suivi_etat_server("suivi_etat_1",r)
  mod_suivi_validation_srise_server("suivi_validation_srise_1",r)
}
