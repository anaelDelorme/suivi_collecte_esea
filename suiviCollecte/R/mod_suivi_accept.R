#' suivi_accept UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_suivi_accept_ui <- function(id){
  ns <- NS(id)
  tagList(
     fluidRow(
      bs4Dash::box(
          title = "Suivi accept",
          status = "orange",
          width = 12,
          echarts4rOutput(ns("pie_questionnaire_par_etat"))
      )),
    fluidRow(
  style = "background-color: #343a40;",
  column(
    width = 12,
    br(),
   # h4("Vue régionale", style = "color: white"),
    pickerInput(
      inputId = ns("region_picker"),
      label = NULL,
      choices = NULL,
      options = list(
        style = "btn",
        size = 5,
        title = "Choix de la région"
      )
    )
  )
    ),
    br(),
  fluidRow(
      bs4Dash::box(
          title = "Suivi accept régional",
          status = "indigo",
          echarts4rOutput(ns("pie_questionnaire_par_etat_par_region"))
      ),
      bs4Dash::box(
          title = "Nombre de questionnaires acceptés (dont cessation)",
          status = "indigo",
          echarts4rOutput(ns("map_nb_questionnaire_accept"))
      ))
  )
}
    
#' suivi_accept Server Functions
#'
#' @noRd 
mod_suivi_accept_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### Liste des régions / départements
    observe({
        nom_region <- c("France", unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>%  pull(REP_LIB_REG_1)))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region)    

        nom_departement <- unique(r$data_suivi %>% arrange(REP_LIB_DEPT_1) %>% pull(REP_LIB_DEPT_1))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)
    })


    observeEvent(input$region_picker,
    {
      nom_region <- c("France", unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>% pull(REP_LIB_REG_1)))
        nom_region <- stringr::str_to_title(nom_region)
      updatePickerInput(session, inputId = "region_picker", choices = nom_region, selected = input$region_picker) 
          nom_departement <- unique(r$data_suivi %>% 
                                            filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
                                            arrange(REP_LIB_DEPT_1)%>% 
                                            pull(REP_LIB_DEPT_1))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)
      })

    
    #### Nombre de questionnaires par accept
    output$pie_questionnaire_par_etat <- renderEcharts4r({
       dossier <- r$data_suivi
       dossier_accept <- dossier %>%
         filter(!is.na(ACCEPT)) %>% 
            mutate(etat_accept = 
                case_when(
                  ACCEPT == "1" & CESSATION == "1" ~ "Cessation",
                  ACCEPT == "1" ~ "Accept = 1, hors Cessation",
                  ACCEPT %in% c("2", "3")~ "Injoingnable ou impossibilité de repondre",
                  ACCEPT ==  "9"  ~ "Refus",
                )
        ) %>% 
        group_by(etat_accept) %>% 
        count() %>% 
        ungroup()


df_colours <- data.frame(etat_accept = c("Accept = 1, hors Cessation", "Cessation","Injoingnable ou impossibilité de repondre", "Refus" ),
                         colours = c("#5470c6", "#73c0de", "#fac858", "#E8465B"))
colour <- df_colours %>%
  filter(etat_accept %in% dossier_accept$etat_accept) %>%
  select(colours) %>%
  unlist() %>% 
  unname()

dossier_accept %>% 
  e_charts(etat_accept) %>% 
  e_pie(n, radius = c("40%", "70%")) |>
  e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.name + ': ' + params.value;}")) |>
  e_color(color = colour)

    })

    ### Pie par région
    #### Nombre de questionnaires par accept
    output$pie_questionnaire_par_etat_par_region <- renderEcharts4r({
      if (!is.null(input$region_picker) && input$region_picker !=""){
      
            dossier <- r$data_suivi
            dossier_accept <- dossier %>%
              filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
              filter(!is.na(ACCEPT)) %>% 
                  mutate(etat_accept = 
                      case_when(
                        ACCEPT == "1" & CESSATION == "1" ~ "Cessation",
                        ACCEPT == "1" ~ "Accept = 1, hors Cessation",
                        ACCEPT %in% c("2", "3")~ "Injoingnable ou impossibilité de repondre",
                        ACCEPT ==  "9"  ~ "Refus",
                      )
              ) %>% 
              group_by(etat_accept) %>% 
              count() %>% 
              ungroup()


      df_colours <- data.frame(etat_accept = c("Accept = 1, hors Cessation", "Cessation","Injoingnable ou impossibilité de repondre", "Refus" ),
                              colours = c("#5470c6", "#73c0de", "#fac858", "#E8465B"))
      colour <- df_colours %>%
        filter(etat_accept %in% dossier_accept$etat_accept) %>%
        select(colours) %>%
        unlist() %>% 
        unname()

      dossier_accept %>% 
        e_charts(etat_accept) %>% 
        e_pie(n, radius = c("40%", "70%")) |>
        e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.name + ': ' + params.value;}")) |>
        e_color(color = colour)
      }
    })

  })
}
    
## To be copied in the UI
# mod_suivi_accept_ui("suivi_accept_1")
    
## To be copied in the server
# mod_suivi_accept_server("suivi_accept_1")
