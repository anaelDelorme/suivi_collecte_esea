#' suivi_collecte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_suivi_collecte_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' suivi_collecte Server Functions
#'
#' @noRd 
mod_suivi_collecte_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_suivi_collecte_ui("suivi_collecte_1")
    
## To be copied in the server
# mod_suivi_collecte_server("suivi_collecte_1")
