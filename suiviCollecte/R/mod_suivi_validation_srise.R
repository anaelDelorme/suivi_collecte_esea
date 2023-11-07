#' suivi_validation_srise UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_suivi_validation_srise_ui <- function(id){
  ns <- NS(id)
  tagList(
      h1("Suivi Validation Srise")
  )
}
    
#' suivi_validation_srise Server Functions
#'
#' @noRd 
mod_suivi_validation_srise_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_suivi_validation_srise_ui("suivi_validation_srise_1")
    
## To be copied in the server
# mod_suivi_validation_srise_server("suivi_validation_srise_1")
