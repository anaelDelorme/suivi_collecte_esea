#' liste_dossier UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import DT
#' @import dplyr
#' @import bs4Dash
#' @import shinyWidgets
#' @import shinymanager
mod_liste_dossier_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
          bs4Dash::box(title = "Liste des dossiers",
                   status = "orange",
                   DT::dataTableOutput(ns("table_dossiers")),
                   width = 12,
                   height = "1100px"
      )
      )
  )
}
    
#' liste_dossier Server Functions
#'
#' @noRd 
mod_liste_dossier_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
      ### Liste des dossiers
     output$table_dossiers <-
         DT::renderDataTable(
            dossier <- r$data_suivi %>%
                datatable(rownames = FALSE,
                 extensions = c("Scroller", "FixedColumns", "Buttons", "Select"),
                 selection = 'none',
                   filter = list(position = 'top',
                      clear = TRUE,
                      plain = FALSE
                   ),
                 options = list(
                   dom = "Bfrtip",
                   # scroll :
                   scrollY = 750, scrollX = 400, scroller = TRUE,
                   # fixer les colonnes :
                   fixedColumns = list(leftColumns = 1),
                   # selection :
                   select = list(style = 'os', items = 'row'),
                   buttons = c(
                     # enregistrements
                      'csv', 'excel', 'pdf',
                     # selection des elements
                     'selectAll', 'selectNone', 'selectRows'
                   )
                 )
                )
         )


  })
}
    
## To be copied in the UI
# mod_liste_dossier_ui("liste_dossier_1")
    
## To be copied in the server
# mod_liste_dossier_server("liste_dossier_1")
