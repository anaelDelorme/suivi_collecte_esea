#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom aws.s3 s3read_using
#' @importFrom arrow read_parquet
#' @import geojsonio
#' @noRd

app_server <- function(input, output, session) {
  r <- reactiveValues()
  observe({
    r$data_suivi <- aws.s3::s3read_using(
       FUN = arrow::read_parquet,
        object = "ESEA/suivi.parquet",
        bucket = "projet-suivi-collecte-masa",
        opts = list("region" = "")
        )  

    r$map_regions <- geojsonio::topojson_read("https://raw.githubusercontent.com/neocarto/resources/master/geometries/France/regions.topojson")

    r$map_departements <- geojsonio::topojson_read("https://raw.githubusercontent.com/neocarto/resources/master/geometries/France/departements.topojson")

  })
 # Your application server logic
  mod_suivi_collecte_server("suivi_collecte_1",r)
}
