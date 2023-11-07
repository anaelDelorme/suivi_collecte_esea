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
          infoBoxOutput(ns("nb_questionnaires")),
          valueBoxOutput(ns("taux_collecte")),
          valueBoxOutput(ns("taux_reponse"))
        ),
    fluidRow(
      bs4Dash::box(
          title = "Avancement de la collecte",
          status = "orange",
          echarts4rOutput(ns("pie_questionnaire_par_etat"))
      ),
      bs4Dash::box(
          title = "Questionnaires collectés",
          status = "orange",
          pickerInput(
            inputId = ns("departement_picker"),
            label = NULL, 
            choices = NULL,
            options = list(
              style = "btn-warning"
            )
          ),
          echarts4rOutput(ns("suivi_remontee_temps"))
      )),
    fluidRow(
  style = "background-color: #343a40;",
  column(
    width = 12,
    h4("Vue régionale", style = "color: white"),
    pickerInput(
      inputId = ns("region_picker"),
      label = NULL,
      choices = NULL,
      options = list(
        style = "btn"
      )
    )
  )
)
,
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
    
#' suivi_accept Server Functions
#'
#' @noRd 
mod_suivi_accept_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### Liste des régions / départements
    observe({
        nom_region <- c("France", unique(r$data_suivi %>% pull(REP_LIB_REG_1)))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region)    

        nom_departement <- c("Tous", unique(r$data_suivi %>% pull(REP_LIB_DEPT_1)))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)
    })


    observeEvent(input$region_picker,
    {
      nom_region <- c("France", unique(r$data_suivi %>% pull(REP_LIB_REG_1)))
        nom_region <- stringr::str_to_title(nom_region)
      updatePickerInput(session, inputId = "region_picker", choices = nom_region, selected = input$region_picker) 
          nom_departement <- c("Tous", unique(r$data_suivi %>% 
                                            filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
                                            pull(REP_LIB_DEPT_1)))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)
      })

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

    #### Suivi dans le temps des questionnaires remontés
      output$suivi_remontee_temps <- renderEcharts4r({
        
        if(is.null(input$region_picker) || input$region_picker == "France"){
            suivi_init <- r$data_suivi
        }else if (input$departement_picker== "Tous"  ) {
           suivi_init <- r$data_suivi %>%
                      filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker))
        }else{
          suivi_init <- r$data_suivi %>%
                      filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$departement_picker))
        }

      questionnaire_par_jour <- suivi_init   %>%
          select(NOM_DOSSIER, DATE_REMONTEE) %>%
          mutate(jour_remontee = as.Date(as.POSIXct(DATE_REMONTEE, format = "%Y-%m-%d %H:%M:%OS"))) %>%
          filter(!is.na(jour_remontee)) %>%
          count(jour_remontee) %>%
          arrange(jour_remontee) %>%
          mutate(cum_nb_questionnaire = cumsum(n)) 

      nb_jour_collecte = as.Date(as.POSIXct("2024-03-01", format = "%Y-%m-%d")) - as.Date(as.POSIXct("2023-10-03" , format = "%Y-%m-%d"))
      questionnaire_a_collecter_chaque_jour <- nrow(suivi_init) /as.numeric(nb_jour_collecte)

      questionnaire_par_jour <- questionnaire_par_jour %>% 
        mutate(nb_jours = as.numeric(as.Date(as.POSIXct(jour_remontee, format = "%Y-%m-%d")) - as.Date(as.POSIXct("2023-10-03" , format = "%Y-%m-%d")))) %>% 
        mutate(estimation = nb_jours * questionnaire_a_collecter_chaque_jour) 

    questionnaire_par_jour %>%
      e_charts(jour_remontee, height = "900px") %>%
      e_line(cum_nb_questionnaire, name = "Cumulé") %>%
      e_line(estimation, name = "Trajectoire estimée") %>%
      e_bar(n, name = "Par jour") %>%
      e_x_axis(type = "time", boundary_gap = 0) %>%
      e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.value[0] + ' : ' + params.value[1];}")) %>%
      e_title("") %>%
      e_y_axis(name = "Questionnaires remontés")

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
    
    pal <- leaflet::colorNumeric("Oranges", NULL)

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
# mod_suivi_accept_ui("suivi_accept_1")
    
## To be copied in the server
# mod_suivi_accept_server("suivi_accept_1")
