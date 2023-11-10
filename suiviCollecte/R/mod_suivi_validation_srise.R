#' suivi_validation_srise UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import echarts4r
#' @import shinyWidgets
#' @import bs4Dash
#' @import dplyr
#' @import stringr
#' @import tidyr

mod_suivi_validation_srise_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
      bs4Dash::box(
          title = "Suivi validation SRISE",
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
          ),pickerInput(
            inputId = ns("departement_picker"),
            label = NULL,
            choices = NULL,
            options = list(
              style = "btn-primary",
              size = 5,
              title = "Choix du département"
            )
          ),
          echarts4rOutput(ns("graph_validation_srise"))
      ))
  )
}
    
#' suivi_validation_srise Server Functions
#'
#' @noRd 
mod_suivi_validation_srise_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ### Liste des régions / départements
    observe({
        nom_region <-  c("France",unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>%  pull(REP_LIB_REG_1)))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region)    

        nom_departement <- c("Tous",unique(r$data_suivi %>% arrange(REP_LIB_DEPT_1) %>% pull(REP_LIB_DEPT_1)))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)
    })


    observeEvent(input$region_picker,
    {
        nom_region <- c("France", unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>% pull(REP_LIB_REG_1)))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region, selected = input$region_picker) 
          nom_departement <- c("Tous", unique(r$data_suivi %>% 
                                            filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
                                            arrange(REP_LIB_DEPT_1)%>% 
                                            pull(REP_LIB_DEPT_1)))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)
      })



    #### Nombre de questionnaires par accept
    output$graph_validation_srise <- renderEcharts4r({
      
            dossier <- r$data_suivi

            if(!is.null(input$region_picker) && input$region_picker != "" && input$region_picker != "France"){
               if(!is.null(input$departement_picker) && input$departement_picker != "" && input$departement_picker != "Tous"){
                  dossier <- dossier %>%
                    filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$departement_picker))
               }else{
                  dossier <- dossier %>%
                    filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker))
               }
            }

            data_validation_srise <- dossier %>% 
                select(NOM_DOSSIER,starts_with("SEM_")) %>% 
                pivot_longer(names_to = "semaine", values_to = "etat", -NOM_DOSSIER) %>% 
                mutate(nb = ifelse(is.na(etat), 0, 1)) %>% 
                arrange(etat) %>% 
                pivot_wider(names_from = etat, values_from = nb) %>% 
                group_by(semaine) %>% 
                summarise(across(is.numeric,~sum(., na.rm= TRUE))) %>% 
                ungroup() 

        new_names <- c(
                  "1 - Validé" = "1", "2 - Corrigé" = "2", "3 - Non validé" = "3", "4 - R4" = "4", "5 - R5" = "5", 
                  "6 - R6" = "6", "7 - R7" = "7", "8 - R8" = "8", "9 - Non vu" = "9"
                )

          data_validation_srise <- data_validation_srise %>%
                rename(any_of(new_names))

              if("NA" %in% names(data_validation_srise)){
                data_validation_srise <- data_validation_srise %>%
                  select(-`NA`)
              }

              df_colours <- data.frame(etat_srise = c("1 - Validé", "2 - Corrigé", "3 - Non validé", "4 - R4", "5 - R5", "6 - R6", "7 - R7", "8 - R8", "9 - Non vu" ),
                                      colours = c("#58C74D", "#5470c6", "#725523", "#FFF18A" ,"#F2B449","#daa140","#eb8525","#eb6f5c", "#E8465B"))

              colour <- df_colours %>%
                filter(etat_srise %in% names(data_validation_srise)) %>%
                select(colours) %>%
                unlist() %>% 
                unname()

              p <- data_validation_srise %>% 
                e_charts(semaine) 

              for(name in names(data_validation_srise %>% select(-semaine))){
                p <- p %>% 
                  e_area_(name, stack = "grp")
              }
              p  <- p |>
                echarts4r::e_tooltip(formatter = htmlwidgets::JS(
                  'function(params) {
                    let seriesName = params.seriesName || "";
                    let value0 = params.value[0] === null ? "" : params.value[0];
                    let value1 = params.value[1] === null ? "" : params.value[1];
                    return seriesName + " : " + value0 + " : " + value1;
                  }'
                ))  %>% 
                e_color(color = colour)
              p

    })
  })
}
    
## To be copied in the UI
# mod_suivi_validation_srise_ui("suivi_validation_srise_1")
    
## To be copied in the server
# mod_suivi_validation_srise_server("suivi_validation_srise_1")
