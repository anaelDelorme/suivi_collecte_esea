#' suivi_collecte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList verbatimTextOutput h1 renderPrint
#' @importFrom aws.s3 s3read_using
#' @importFrom arrow read_parquet
#' @import glue
#' @import dplyr
#' @import tidyr
#' @import bs4Dash
#' @import echarts4r
#' @importFrom grDevices col2rgb colors rgb
#' @import leaflet
#' @import DT
#' @import shinymanager


### Load data ###
#  data_suivi <- aws.s3::s3read_using(
#        FUN = arrow::read_parquet,
#        object = "ESEA/suivi.parquet",
#        bucket = "projet-suivi-collecte-masa",
#        opts = list("region" = "")
#  )  

mod_suivi_collecte_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
          infoBoxOutput(ns("nb_questionnaires")),
          valueBoxOutput(ns("taux_collecte")),
          valueBoxOutput(ns("taux_reponse"))
        ),
    fluidRow(
      bs4Dash::box(
          title = "Avancement de la collecte",
          status = "orange",
          echarts4rOutput(ns("pie_questionnaire_par_etat"))
      )),
    fluidRow(
      bs4Dash::box(
          title = "Avancement de la collecte",
          status = "indigo",
          shinyWidgets::awesomeRadio(
            inputId = ns("map_choice_region_departement"),
            label = NULL, 
            choices = c("Région", "Département"),
            selected = "Région",
            inline = TRUE,
            status = "success"
          ),
          leafletOutput(ns("map_taux_collecte"))
      )
    )    
  )
}
    
#' suivi_collecte Server Functions
#'
#' @noRd 
mod_suivi_collecte_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #### Indicateurs
    observe({
      questionnaires_totaux_esea <- r$data_suivi %>% nrow()
      questionnaires_collectes <-  r$data_suivi %>% filter(ETAT_CONTROLE  != 1) %>% nrow()
      questionnaires_valides <-  r$data_suivi %>% filter(ETAT_CONTROLE  == 5) %>% nrow()

      output$nb_questionnaires <- renderInfoBox({
        infoBox(
          title = "Nombre total de questionnaires",
          fill = TRUE,
          gradient = TRUE,
          color = "info",
          value = questionnaires_totaux_esea,
          icon = icon("paper-plane")
        )
      })

       output$taux_collecte <- renderValueBox({
        valueBox(
          value = glue(round(100 *(questionnaires_collectes / questionnaires_totaux_esea),3), ' %'),
          subtitle = "Taux de collecte (questionnaires qui ne sont plus en état initial)",
        color = "primary",
        icon = icon("circle-check")
        )
      })

      output$taux_reponse <- renderValueBox({
        valueBox(
          value = glue(round(100 *(questionnaires_valides / questionnaires_totaux_esea),3), ' %'),
          subtitle = "Taux de réponse (questionnaires validés)",
        color = "teal",
        icon = icon("thumbs-up")
        )
      })

    })

    #### Nombre de questionnaires par état
    output$pie_questionnaire_par_etat <- renderEcharts4r({
       dossier <- r$data_suivi
       questionnaire_par_etat <- dossier %>%
         count(ETAT_CONTROLE) %>%
         mutate(
          ETAT_CONTROLE = case_when(
            ETAT_CONTROLE == 1 ~ "Non collecté",
            ETAT_CONTROLE == 5 ~ "Validé",
            ETAT_CONTROLE == 4 ~ "A confirmer",
            ETAT_CONTROLE == 2 ~ "A compléter",
            ETAT_CONTROLE == 3 ~ "A corriger",
            TRUE ~ "Indéterminé"
          )
         )

       questionnaire_par_etat %>%
        e_charts(ETAT_CONTROLE) %>%
        e_pie(n, radius = c("40%", "70%")) |>
         e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.name + ': ' + params.value;}"))
    })

    
    ### Carte taux de collecte par département ou région
    data_map_taux_collecte <- reactive({
      if (input$map_choice_region_departement == "Département") {
        suivi_par_departement <- r$data_suivi %>%
          mutate(collecte = ifelse(ETAT_CONTROLE == 1 , "Non collecté", 'Collecté')) %>%
          group_by(collecte, REP_CODE_DEPT_1) %>%
          count() %>%
          pivot_wider(names_from = collecte, values_from = n,  values_fill = 0) %>%
          filter(!is.na(REP_CODE_DEPT_1)) %>%
          mutate(total = Collecté + `Non collecté`) %>%
          mutate(taux_collecte = Collecté/total) 

        data_map_taux_collecte <- r$map_departements %>%
          left_join(suivi_par_departement, by = c("DEP" = "REP_CODE_DEPT_1")) %>%
          rename(Name = Nom)
      }
      else{
        suivi_par_region <- r$data_suivi %>%
          mutate(collecte = ifelse(ETAT_CONTROLE == 1 , "Non collecté", 'Collecté')) %>%
          group_by(collecte, REP_CODE_REG_1) %>%
          count() %>%
          pivot_wider(names_from = collecte, values_from = n,  values_fill = 0) %>%
          filter(!is.na(REP_CODE_REG_1)) %>%
          mutate(total = Collecté + `Non collecté`) %>%
          mutate(taux_collecte = Collecté/total) 

        data_map_taux_collecte <- r$map_regions %>%
          left_join(suivi_par_region, by = c("REG" = "REP_CODE_REG_1"))
      }
      list(df = data_map_taux_collecte)
    })
    
    pal <- colorNumeric("RdYlGn", NULL)

    output$map_taux_collecte <- renderLeaflet({
        data_taux_collecte <- data_map_taux_collecte()$df
        leaflet::leaflet(data_taux_collecte) %>%
          addPolygons(stroke = FALSE, 
              smoothFactor = 0.3, 
              fillOpacity = 1,
              fillColor = ~pal(taux_collecte),
              label = ~paste0(Name, ": ",
                              round(100 * taux_collecte,0),
                              " % / ",Collecté," collectés pour ", total," au total.")) %>%
              addLegend(pal = pal, values = ~round(100*taux_collecte,0), title = "Taux de collecte",
                    opacity = 1, position = "bottomright", na.label= "?",labFormat = labelFormat(suffix=" %"))
    })

  })
}
    
## To be copied in the UI
# mod_suivi_collecte_ui("suivi_collecte_1")
    
## To be copied in the server
# mod_suivi_collecte_server("suivi_collecte_1")
