#' suivi_etat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_suivi_etat_ui <- function(id){
  ns <- NS(id)
  tagList(
      h1("Suivi Etat")
  )
}
    
#' suivi_etat Server Functions
#'
#' @noRd 
mod_suivi_etat_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_suivi_etat_ui("suivi_etat_1")
    
## To be copied in the server
# mod_suivi_etat_server("suivi_etat_1")
