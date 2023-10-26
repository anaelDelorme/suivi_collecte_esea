#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom aws.s3 s3read_using
#' @importFrom arrow read_parquet
#' @noRd


data_suivi <- aws.s3::s3read_using(
  FUN = arrow::read_parquet,
  object = "ESEA/suivi.parquet",
  bucket = "projet-suivi-collecte-masa",
  opts = list("region" = "")
)


app_server <- function(input, output, session) {

  data_suivi <- reactive({
    return(data_suivi)
  })

  # Your application server logic
  mod_suivi_collecte_server("suivi_collecte_1")
}
