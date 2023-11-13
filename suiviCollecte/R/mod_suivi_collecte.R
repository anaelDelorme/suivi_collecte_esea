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
#' @import stringr
#' @import shinyWidgets
#' @import janitor


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
          shinyWidgets::awesomeRadio(
            inputId = ns("map_choice_region_departement"),
            label = NULL, 
            choices = c("Région", "Département"),
            selected = "Région",
            inline = TRUE,
            status = "success"
          ),
          leafletOutput(ns("map_taux_collecte"))
      ),
      bs4Dash::box(
          title = "Questionnaires collectés",
          status = "orange",
          shinyWidgets::awesomeRadio(
            inputId = ns("map_choice_absolu_relatif"),
            label = "Choix de l'affichage", 
            choices = c("En nombre de questionnaires", "Selon le taux d'avancement"),
            selected = "En nombre de questionnaires",
            inline = TRUE,
            status = "success"
          ),
           shinyWidgets::pickerInput(
            inputId = ns("region_picker"),
            label = NULL,
            choices = NULL,
            options = list(
              style = "btn-warning",
              size = 5,
              title = "Choix de la région"
            )
          ),
          echarts4rOutput(ns("suivi_remontee_temps"))
      )),
    fluidRow(
      bs4Dash::box(
          title = "Suivi par enquêteur",
          width = 12,
          status = "indigo",
          pickerInput(
            inputId = ns("region_picker_enqueteur"),
            label = NULL,
            choices = NULL,
            options = list(
              style = "btn-warning",
              size = 5,
              title = "Choix de la région"
            )
          ),
          pickerInput(
            inputId = ns("dept_picker_enqueteur"),
            label = NULL,
            choices = NULL,
            options = list(
              style = "btn-primary",
              size = 5,
              title = "Choix du département"
            )
          ),
          echarts4rOutput(ns("histo_enqueteur")),
          br(),
          p("Choisir un département pour afficher le graphique."))
    )    
  )
}
    
#' suivi_collecte Server Functions
#'
#' @noRd 
mod_suivi_collecte_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### Liste des régions / départements
   observe({
        nom_region <- c("France", unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>% pull(REP_LIB_REG_1)))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region)    
        updatePickerInput(session, inputId = "region_picker_enqueteur", choices = nom_region)    

        nom_departement <-  unique(r$data_suivi %>% arrange(REP_LIB_DEPT_1) %>% 
                                            filter(!is.na(REP_LIB_DEPT_1)) %>%  pull(REP_LIB_DEPT_1))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "dept_picker_enqueteur", choices = nom_departement)
    })

        observeEvent(input$region_picker,
    {
        nom_region <- c("France", unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>% pull(REP_LIB_REG_1)))
      nom_region <- stringr::str_to_title(nom_region)
      updatePickerInput(session, inputId = "region_picker_enqueteur", choices = nom_region, selected = input$region_picker) 
      updatePickerInput(session, inputId = "region_picker", choices = nom_region, selected = input$region_picker) 

      nom_departement <- unique(r$data_suivi %>% arrange(REP_LIB_DEPT_1) %>% 
                                            filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
                                            filter(!is.na(REP_LIB_DEPT_1)) %>% 
                                            pull(REP_LIB_DEPT_1))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "dept_picker_enqueteur", choices = nom_departement)
      })


    observeEvent(input$region_picker_enqueteur,
    {
        nom_region <- c("France", unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>% pull(REP_LIB_REG_1)))
      nom_region <- stringr::str_to_title(nom_region)
      updatePickerInput(session, inputId = "region_picker_enqueteur", choices = nom_region, selected = input$region_picker_enqueteur) 
      updatePickerInput(session, inputId = "region_picker", choices = nom_region, selected = input$region_picker_enqueteur) 

      nom_departement <- unique(r$data_suivi %>% arrange(REP_LIB_DEPT_1) %>%  
                                            filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker_enqueteur))%>% 
                                            filter(!is.na(REP_LIB_DEPT_1)) %>% 
                                            pull(REP_LIB_DEPT_1))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "dept_picker_enqueteur", choices = nom_departement)
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
          value = glue(round(100 *(questionnaires_collectes / questionnaires_totaux_esea),1), ' %'),
          subtitle = "Taux de collecte (questionnaires qui ne sont plus en état initial)",
        color = "primary",
        icon = icon("circle-check")
        )
      })

      output$taux_reponse <- renderValueBox({
        valueBox(
          value = glue(round(100 *(questionnaires_valides / questionnaires_totaux_esea),1), ' %'),
          subtitle = "Taux de réponse (questionnaires validés)",
        color = "teal",
        icon = icon("thumbs-up")
        )
      })

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
          mutate(DEP = as.character(DEP)) %>%
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

          left_join(suivi_par_region %>% mutate(REP_CODE_REG_1 = as.numeric(REP_CODE_REG_1)) , by = c("REG" = "REP_CODE_REG_1"))
      }
      list(df = data_map_taux_collecte)
    })
    
    pal <- leaflet::colorNumeric("Oranges", NULL)
    pal_inverse <- leaflet::colorNumeric("Oranges",  NULL, reverse = TRUE)

    output$map_taux_collecte <- renderLeaflet({
        data_taux_collecte <- data_map_taux_collecte()$df
        leaflet::leaflet(data_taux_collecte) %>%
          addPolygons(color = "#343a40", 
                weight = 0.5, 
                smoothFactor = 0.5,
                opacity = 0.1, 
                fillOpacity = 1.0,
              fillColor = ~pal(taux_collecte),
              label = ~paste0(Name, ": ",
                              round(100 * taux_collecte,0),
                              " % / ",Collecté," collectés pour ", total," au total.")) %>%
              addLegend(pal = pal_inverse, values = ~round(100*taux_collecte,0), title = "Taux de collecte",
                    opacity = 1, position = "bottomright", na.label= "?",labFormat = labelFormat(suffix=" %", transform = function(x)  sort(x, decreasing = TRUE)))

    })


    #### Suivi dans le temps des questionnaires remontés
      output$suivi_remontee_temps <- renderEcharts4r({
      questionnaire_par_jour <- r$data_suivi   %>%
        select(NOM_DOSSIER, DATE_REMONTEE, REP_LIB_REG_1, REP_LIB_DEPT_1) %>%
        mutate(REP_LIB_REG_1 = stringr::str_to_title(REP_LIB_REG_1), REP_LIB_DEPT_1 = stringr::str_to_title(REP_LIB_DEPT_1)) %>% 
        mutate(jour_remontee = as.Date(as.POSIXct(DATE_REMONTEE, format = "%Y-%m-%d %H:%M:%OS"))) %>%
        filter(!is.na(jour_remontee)) 
      


if (is.null(input$region_picker) || input$region_picker == "" || input$region_picker == "France"){
  questionnaire_par_jour_grp <- questionnaire_par_jour %>% 
    group_by(REP_LIB_REG_1) %>% 
    count(jour_remontee) 
    
    if(input$map_choice_absolu_relatif == "En nombre de questionnaires"){
  questionnaire_par_jour_grp <- questionnaire_par_jour %>% 
    pivot_wider(names_from = REP_LIB_REG_1, values_from = n, values_fill =0) %>% 
    arrange(jour_remontee) %>%
    mutate(across(where(is.numeric), ~ cumsum(.))) %>% 
    janitor::adorn_totals("col", name="France")
  
    }else{
    nb_france <-  r$data_suivi %>% 
        group_by(REP_LIB_REG_1) %>% 
        count() %>%
        ungroup() %>%
        janitor::adorn_totals("col", name="France")
    questionnaire_par_jour_grp <- questionnaire_par_jour_grp %>%




  }



  nb_jour_collecte = as.Date(as.POSIXct("2024-03-01", format = "%Y-%m-%d")) - as.Date(as.POSIXct("2023-10-03" , format = "%Y-%m-%d"))
  questionnaire_a_collecter_chaque_jour <- nrow(r$data_suivi) /as.numeric(nb_jour_collecte)
  
  questionnaire_par_jour_grp <- questionnaire_par_jour_grp %>% 
    mutate(nb_jours = as.numeric(as.Date(as.POSIXct(jour_remontee, format = "%Y-%m-%d")) - as.Date(as.POSIXct("2023-10-03" , format = "%Y-%m-%d")))) %>% 
    mutate(estimation = nb_jours * questionnaire_a_collecter_chaque_jour) %>% 
    select(-nb_jours) %>% 
    rename("Trajectoire estimée" = estimation)

    p <- questionnaire_par_jour_grp %>%
    echarts4r::e_charts(jour_remontee, height = "900px") %>% 
      e_line(France, name ="Total France") %>% 
      e_line(`Trajectoire estimée`, name ="Trajectoire estimée France")
 
    for (col_name in colnames(questionnaire_par_jour_grp %>%  select(-jour_remontee, - France, -`Trajectoire estimée`))) {
      p <- p %>% e_line_(col_name, name = col_name)
    }
}else{
  questionnaire_par_jour_grp <- questionnaire_par_jour %>% 
    filter(REP_LIB_REG_1 == input$region_picker) %>% 
    group_by(REP_LIB_DEPT_1) %>% 
    count(jour_remontee) %>%
    pivot_wider(names_from = REP_LIB_DEPT_1, values_from = n, values_fill =0) %>% 
    arrange(jour_remontee) %>%
    mutate(across(where(is.numeric), ~ cumsum(.))) %>% 
    janitor::adorn_totals("col", name="France") %>% 
    select(-France)

    p <- questionnaire_par_jour_grp %>%
    echarts4r::e_charts(jour_remontee, height = "900px")
     for (col_name in colnames(questionnaire_par_jour_grp %>%  select(-jour_remontee))) {
      p <- p %>% e_line_(col_name, name = col_name)
    }
}

p <- p %>%
  echarts4r::e_x_axis(type = "time",
                      axisLabel = list(
                        type = "time",
                        formatter =  htmlwidgets::JS(
                          'function(value){
              // convert to date format
              let date = new Date(value);

              label = date.getDate() + "-" + (parseInt(date.getMonth()) + 1);
              return label;
            }'
                        )
                      )) %>%
  echarts4r::e_tooltip(formatter = htmlwidgets::JS(
    'function(params) {
      let seriesName = params.seriesName || "";
      let value0 = params.value[0] === null ? "" : params.value[0];
      let value1 = params.value[1] === null ? "" : params.value[1];
      return seriesName + " : " + value0 + " : " + value1;
    }'
  )) %>%
  echarts4r::e_title("") %>%
  echarts4r::e_y_axis(name = "Questionnaires remontés") %>%
  echarts4r::e_legend(type = "scroll")

  p

  })


 #### Suivi des enquêteurs
      output$histo_enqueteur <- renderEcharts4r({
        if (!is.null(input$region_picker) && input$region_picker != ""){
          if(!is.null(input$dept_picker_enqueteur) && input$dept_picker_enqueteur != ""){
              
            liste_enqueteur <- r$data_suivi %>% 
              filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$dept_picker_enqueteur)) %>% 
              filter(!is.na(CODE_ENQUETEUR)) %>% 
              pull(CODE_ENQUETEUR) %>%
              unique()
            nbdossier_enqueteur <- r$data_suivi %>% 
              mutate(remonte = case_when(
                  ETAT_CONTROLE!=1 ~ "Remontés",
                  !is.na(ETAT_CONTROLE) ~ "A collecter"
              ))%>%
              filter(CODE_ENQUETEUR %in% liste_enqueteur) %>%
              filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
              filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$dept_picker_enqueteur)) %>% 
              group_by(CODE_ENQUETEUR,CODE_GEOGRAPHIQUE,NOM_ENQ,PRENOM_ENQ) %>% 
              count(remonte) %>% 
              ungroup() %>% 
              mutate() %>% 
              pivot_wider(names_from = remonte, values_from = n) %>%
              mutate(Enquêteur = paste(NOM_ENQ, PRENOM_ENQ, sep = " ")) 

            nbdossier_enqueteur %>%
              echarts4r::e_charts(Enquêteur) %>% 
              echarts4r::e_bar(Remontés, stack = "grp") %>% 
              echarts4r::e_bar_("A collecter", stack = "grp")%>%
              e_tooltip(trigger = "item")%>% 
              e_x_axis(
                axisLabel = list(
                rotate = 20, # Angle d'inclinaison en degrés
                interval = 0,
                fontSize = 9
                ))
          }
            
        }
          

      })
  })
}
    
## To be copied in the UI
# mod_suivi_collecte_ui("suivi_collecte_1")
    
## To be copied in the server
# mod_suivi_collecte_server("suivi_collecte_1")
