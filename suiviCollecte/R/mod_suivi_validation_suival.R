#' suivi_validation_suival UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom lubridate dmy
#' @import dplyr
#' @import tidyverse
#' @import bs4Dash
#' @import shinyWidgets
#' @import echarts4r
#' @importFrom grDevices colours
mod_suivi_validation_suival_ui <- function(id){
  ns <- NS(id)
  tagList(fluidRow(
      bs4Dash::box(
          title = "Suivi validation SUIVAL",
          status = "indigo",
          width = 12,
          pickerInput(
            inputId = ns("region_picker"),
            label = NULL,
            choices = NULL,
            options = list(
              style = "btn-warning",
              size = 5,
              title = "Choix de la région"
            )
          ),
          echarts4rOutput(ns("graph_validation_suival"))
      ))
 
  )
}
    
#' suivi_validation_suival Server Functions
#'
#' @noRd 
mod_suivi_validation_suival_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
        nom_region <-  c("France",unique(r$tab_suival %>% arrange(REP_LIB_REG_C) %>%  pull(REP_LIB_REG_C)))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region)    
    })

  output$graph_validation_suival <- renderEcharts4r({
      
            dossier <- r$tab_suival
            #print(dossier)
            if(!is.null(input$region_picker) && input$region_picker != "" && input$region_picker != "France"){
              dossier <- dossier %>%
                    filter(REP_LIB_REG_C == stringr::str_to_upper(input$region_picker))
              
            }
            #print(dossier)

             ETAT_ANOMALIE_RECOD = c('0' = "Non traité",
                         '1' ="En cours",
                         '2' ="Corrigée dans l'enquête",
                         '3' ="Corrigée",
                         '4' ='Forcée')

            data_validation_suival <- dossier %>% 
              select(starts_with("SUIV")) %>%
              pivot_longer(cols=everything(),
                          names_to="DATE",
                          values_to ="ETAT_ANOMALIE" ) %>%
                group_by(DATE,ETAT_ANOMALIE) %>%
                summarise(nbdossier=n()) %>%
                ungroup() %>%
                mutate(ETAT_ANOMALIE=recode(ETAT_ANOMALIE,!!!ETAT_ANOMALIE_RECOD )) %>%
                pivot_wider(names_from = ETAT_ANOMALIE,
                            values_from=nbdossier) %>% 
                # mis au format date via lubridate et la fonction dmy
                mutate(DATE_SUIVAL=lubridate::dmy(substr(DATE,8,18)),.before = everything()) %>%
                select(-DATE)
            #print(data_validation_suival)
            p <-  data_validation_suival %>%
                e_charts(DATE_SUIVAL) 
                
              if ("Non traité" %in% names(data_validation_suival)){
                p <- p %>%
                  e_area_("Non traité", stack = "grp", color = "#E8465B")
              }   

              if ("En cours" %in% names(data_validation_suival)){
                p <- p %>%
                  e_area_("En cours", stack = "grp", color = "#FFF18A") 
              }

              if ("Corrigée dans l'enquête" %in% names(data_validation_suival)){
                p <- p %>%
                e_area_("Corrigée dans l'enquête", stack = "grp", color = "#725523")
              }

              if ("Corrigée" %in% names(data_validation_suival)){
                p <- p %>%
                e_area_("Corrigée", stack = "grp", color = "#5470c6") 
              }

              if ("Forcée" %in% names(data_validation_suival)){
                p <- p %>%
                e_area_("Forcée", stack = "grp", color = "#58C74D")
              }

              p <- p |>
                   e_tooltip(trigger = "axis", 
                   axis_pointer_type = "cross")
              p
           })
  })
}
    
## To be copied in the UI
# mod_suivi_validation_suival_ui("suivi_validation_suival_1")
    
## To be copied in the server
# mod_suivi_validation_suival_server("suivi_validation_suival_1")
