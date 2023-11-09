#' suivi_etat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import leaflet
#' @import echarts4r
#' @import shinyWidgets
#' @import bs4Dash
#' @import dplyr
#' @import stringr
#' @import tidyr
mod_suivi_etat_ui <- function(id){
  ns <- NS(id)
  tagList(
        fluidRow(
  style = "background-color: #343a40;",
  column(
    width = 12,
    br(),
    h4("Vue nationale", style = "color: white")
    )
    ),
    br(),
     fluidRow(
      bs4Dash::box(
          title = "Suivi état",
          status = "orange",
          width = 12,
          echarts4rOutput(ns("pie_questionnaire_par_etat"))
      )),
      
    fluidRow(
  style = "background-color: #343a40;",
  column(
    width = 12,
    br(),
    h4("Vue régionale", style = "color: white"),
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
          title = "Suivi état régional",
          status = "indigo",
          echarts4rOutput(ns("pie_questionnaire_par_etat_par_region"))
      ),
      bs4Dash::box(
          title = "Nombre de questionnaires injoignables, dans l'impossibilité de repondre ou en refus",
          status = "indigo",
          shinyWidgets::awesomeRadio(
            inputId = ns("map_choice_region_departement"),
            label = NULL, 
            choices = c("Région", "Département"),
            selected = "Région",
            inline = TRUE,
            status = "success"
          ),
          leaflet::leafletOutput(ns("map_nb_questionnaire_non_etat"))
      )),
    fluidRow(
  style = "background-color: #343a40;",
  column(
    width = 12,
    br(),
    h4("Vue départementale", style = "color: white"),
    pickerInput(
      inputId = ns("departement_picker"),
      label = NULL,
      choices = NULL,
      options = list(
        style = "btn",
        size = 5,
        title = "Choix du département"
      )
    )
  )
    ),
    br(),
  fluidRow(
      bs4Dash::box(
          title = "Suivi état départemental",
          status = "lightblue",
          width = 12,
          echarts4rOutput(ns("pie_questionnaire_par_etat_par_departement"))
      )),
  fluidRow(
      bs4Dash::box(
          title = "Suivi Accept par département intervenant sur le département",
          status = "lightblue",
          width = 12,
          echarts4rOutput(ns("graph_enqueteur_departement"))
      ))
  )
}
    
#' suivi_etat Server Functions
#'
#' @noRd 
mod_suivi_etat_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ### Liste des régions / départements
    observe({
        nom_region <-  unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>%  pull(REP_LIB_REG_1))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region)    

        nom_departement <- unique(r$data_suivi %>% arrange(REP_LIB_DEPT_1) %>% pull(REP_LIB_DEPT_1))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)
    })


    observeEvent(input$region_picker,
    {
        nom_region <- unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>% pull(REP_LIB_REG_1))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region, selected = input$region_picker) 
          nom_departement <- unique(r$data_suivi %>% 
                                            filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
                                            arrange(REP_LIB_DEPT_1)%>% 
                                            pull(REP_LIB_DEPT_1))
          nom_departement <- stringr::str_to_title(nom_departement)
          updatePickerInput(session, inputId = "departement_picker", choices = nom_departement)

        ### mise à jour de la carte
        code_region_select <- unique(r$data_suivi %>% 
                                      filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
                                      pull(REP_CODE_REG_1))
        if (is.character(code_region_select) && length(code_region_select) > 0) {
            zoom_lat <- r$map_regions_centroid %>% filter(REG == as.numeric(code_region_select)) %>% pull(centroid_latitude)
            zoom_lon <- r$map_regions_centroid %>% filter(REG == as.numeric(code_region_select)) %>% pull(centroid_longitude)
            leaflet::leafletProxy(ns("map_nb_questionnaire_non_etat")) %>%
                  leaflet::setView(lng = zoom_lon, lat = zoom_lat, zoom = 7)
        }


      })


    observeEvent(input$departement_picker,
    {
        ### mise à jour de la carte
        code_dpt_select <- unique(r$data_suivi %>% 
                                      filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$departement_picker)) %>% 
                                      pull(REP_CODE_DEPT_1))
        if (is.character(code_dpt_select) && length(code_dpt_select) > 0 && input$map_choice_region_departement == "Département") {
            zoom_lat <- r$map_departements_centroid %>% filter(DEP == as.numeric(code_dpt_select)) %>% pull(centroid_latitude)
            zoom_lon <- r$map_departements_centroid %>% filter(DEP == as.numeric(code_dpt_select)) %>% pull(centroid_longitude)
            leaflet::leafletProxy(ns("map_nb_questionnaire_non_etat")) %>%
                  leaflet::setView(lng = zoom_lon, lat = zoom_lat, zoom = 8)
        }


      })

    
    #### Nombre de questionnaires par état
    output$pie_questionnaire_par_etat <- renderEcharts4r({
       dossier <- r$data_suivi
       dossier_etat <- dossier %>%
         filter(!is.na(ETAT_CONTROLE)) %>% 
            mutate(etat_etat = 
                case_when(
                  ETAT_CONTROLE == "5" ~ "Validé",
                  ETAT_CONTROLE == "4" ~ "A confirmer",
                  ETAT_CONTROLE == "2"~ "A compléter",
                  ETAT_CONTROLE ==  "3"  ~ "A corriger",
                )
        ) %>% 
        group_by(etat_etat) %>% 
        count() %>% 
        ungroup()


df_colours <- data.frame(etat_etat = c("A compléter", "A confirmer", "A corriger","Validé" ),
                         colours = c( "#fac858","#EE81A2", "#E8465B","#31a859"))
colour <- df_colours %>%
  filter(etat_etat %in% dossier_etat$etat_etat) %>%
  select(colours) %>%
  unlist() %>% 
  unname()

dossier_etat %>% 
  filter(!is.na(etat_etat))%>%
  e_charts(etat_etat) %>% 
  e_pie(n, radius = c("40%", "70%")) |>
  e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.name + ': ' + params.value;}")) |>
  e_color(color = colour)

    })

    ### Pie par région
    #### Nombre de questionnaires par état
    output$pie_questionnaire_par_etat_par_region <- renderEcharts4r({
      if (!is.null(input$region_picker) && input$region_picker !=""){
      
            dossier <- r$data_suivi
            dossier_etat <- dossier %>%
              filter(REP_LIB_REG_1 == stringr::str_to_upper(input$region_picker)) %>% 
              filter(!is.na(ETAT_CONTROLE)) %>% 
                   mutate(etat_etat = 
                case_when(
                  ETAT_CONTROLE == "5" ~ "Validé",
                  ETAT_CONTROLE == "4" ~ "A confirmer",
                  ETAT_CONTROLE == "2"~ "A compléter",
                  ETAT_CONTROLE ==  "3"  ~ "A corriger",
                )
        ) %>% 
        group_by(etat_etat) %>% 
        count() %>% 
        ungroup()


df_colours <- data.frame(etat_etat = c("A compléter", "A confirmer", "A corriger","Validé" ),
                         colours = c( "#fac858","#EE81A2", "#E8465B","#31a859"))
      colour <- df_colours %>%
        filter(etat_etat %in% dossier_etat$etat_etat) %>%
        select(colours) %>%
        unlist() %>% 
        unname()

      dossier_etat %>% 
        filter(!is.na(etat_etat)) %>%
        e_charts(etat_etat) %>% 
        e_pie(n, radius = c("40%", "70%")) |>
        e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.name + ': ' + params.value;}")) |>
        e_color(color = colour)
      }
    })
### Carte régionale 
 output$map_nb_questionnaire_non_etat <- renderLeaflet({
    dossiers <- r$data_suivi %>%
          filter(ETAT_CONTROLE != "1") %>% 
          count(REP_CODE_REG_1, REP_LIB_REG_1)%>% 
          mutate(REP_LIB_REG_1 = stringr::str_to_title(REP_LIB_REG_1))%>% 
          mutate(REP_CODE_REG_1 = as.numeric(REP_CODE_REG_1)) 
          
        data_carte_rond_proportionnel <-  r$map_regions_centroid  %>% 
          left_join(dossiers, by = c("REG" = "REP_CODE_REG_1")) 
          
      leaflet(data_carte_rond_proportionnel) %>% 
            leaflet::addPolygons(color = "#343a40", weight = 0.3, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0.5,fillColor = "white") %>%
            leaflet::addCircleMarkers(
              lng = ~centroid_longitude,
              lat = ~centroid_latitude,
              radius = ~ n,
              fillOpacity = ~ifelse(is.na(n), 0, 0.8),
              color = "#e91f1fb6",
              stroke = FALSE,
                label = ~paste0(Name, ": ",n)
            )
  })
  observeEvent(input$map_choice_region_departement,
    {
    if(input$map_choice_region_departement == "Région"){
        dossiers <- r$data_suivi %>%
          filter(ETAT_CONTROLE != "1") %>% 
          count(REP_CODE_REG_1, REP_LIB_REG_1)%>% 
          mutate(REP_LIB_REG_1 = stringr::str_to_title(REP_LIB_REG_1))%>% 
          mutate(REP_CODE_REG_1 = as.numeric(REP_CODE_REG_1)) 
          
        data_carte_rond_proportionnel <-  r$map_regions_centroid  %>% 
          left_join(dossiers, by = c("REG" = "REP_CODE_REG_1")) 
    }else{
        dossiers <- r$data_suivi %>%
          filter(ETAT_CONTROLE != "1") %>% 
           count(REP_CODE_DEPT_1, REP_LIB_DEPT_1)%>% 
          mutate(REP_LIB_DEPT_1 = stringr::str_to_title(REP_LIB_DEPT_1))%>% 
          mutate(REP_CODE_DEPT_1 = as.numeric(REP_CODE_DEPT_1)) 
          
        data_carte_rond_proportionnel <-  r$map_departements_centroid  %>% 
          left_join(dossiers, by = c("REG" = "REP_CODE_DEPT_1")) %>% 
          rename(Name = Nom)

    }
 
         leaflet::leafletProxy(ns("map_nb_questionnaire_non_etat"), data = data_carte_rond_proportionnel) %>%
            leaflet::clearShapes() %>%
            leaflet::clearMarkers()%>%
            leaflet::addPolygons(color = "#343a40", weight = 0.3, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0.5,fillColor = "white") %>%
            leaflet::addCircleMarkers(
              lng = ~centroid_longitude,
              lat = ~centroid_latitude,
              radius = ~ n,
              fillOpacity = ~ifelse(is.na(n), 0, 0.8),
              color = "#e91f1fb6",
              stroke = FALSE,
                label = ~paste0(Name, ": ",n)
            )
    })


### Pie par département
    #### Nombre de questionnaires par état
    output$pie_questionnaire_par_etat_par_departement <- renderEcharts4r({
      if (!is.null(input$departement_picker) && input$departement_picker !=""){
      
            dossier <- r$data_suivi
            dossier_etat <- dossier %>%
              filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$departement_picker)) %>% 
              filter(!is.na(ETAT_CONTROLE)) %>% 
                  mutate(etat_etat = 
                case_when(
                  ETAT_CONTROLE == "5" ~ "Validé",
                  ETAT_CONTROLE == "4" ~ "A confirmer",
                  ETAT_CONTROLE == "2"~ "A compléter",
                  ETAT_CONTROLE ==  "3"  ~ "A corriger",
                )
        ) %>% 
              group_by(etat_etat) %>% 
              count() %>% 
              ungroup()

      if (nrow(dossier_etat) > 0){
df_colours <- data.frame(etat_etat = c("A compléter", "A confirmer", "A corriger","Validé" ),
                         colours = c( "#fac858","#EE81A2", "#E8465B","#31a859"))
      colour <- df_colours %>%
        filter(etat_etat %in% dossier_etat$etat_etat) %>%
        select(colours) %>%
        unlist() %>% 
        unname()

      dossier_etat %>% 
        filter(!is.na(etat_etat)) %>%
        e_charts(etat_etat) %>% 
        e_pie(n, radius = c("40%", "70%")) |>
        e_tooltip(formatter = htmlwidgets::JS("function(params) {return params.name + ': ' + params.value;}")) |>
        e_color(color = colour)
      }
      }else{return(NULL)}
    })

  ### Graph empilé par enquêteur
  output$graph_enqueteur_departement <- renderEcharts4r({
      if (!is.null(input$region_picker) && input$region_picker != ""){
          if(!is.null(input$departement_picker) && input$departement_picker != ""){
            liste_enqueteur <- r$data_suivi %>% 
              filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$departement_picker)) %>% 
              filter(!is.na(CODE_ENQUETEUR)) %>% 
              pull(CODE_ENQUETEUR)

            nbdossier_enqueteur <- r$data_suivi %>% 
              filter(CODE_ENQUETEUR %in% liste_enqueteur) %>% 
            mutate(etat_etat = 
                case_when(
                  ETAT_CONTROLE == "1" ~ "Accept = 1, hors Cessation",
                  ETAT_CONTROLE %in% c("2", "3")~ "Injoignable ou impossibilité de repondre",
                  ETAT_CONTROLE ==  "9"  ~ "Refus",
                )
        ) %>%  
              group_by(CODE_ENQUETEUR,etat_etat,NOM_ENQ,PRENOM_ENQ) %>% 
              count() %>% 
              ungroup() %>% 
              mutate(Enquêteur = paste(NOM_ENQ, PRENOM_ENQ, sep = " ")) %>%
              tidyr::pivot_wider(names_from = état_etat, values_from= n)
              
            #print(nbdossier_enqueteur) 

            chart_enq <- nbdossier_enqueteur %>%
              echarts4r::e_charts(Enquêteur) 

            if ("Accept = 1, hors Cessation" %in% names(nbdossier_enqueteur)) {
                chart_enq <- chart_enq %>% echarts4r::e_bar_("Accept = 1, hors Cessation", stack = "grp", color="#5470c6") 
            } 

            if ("Injoignable ou impossibilité de repondre" %in% names(nbdossier_enqueteur)) {
                chart_enq <- chart_enq %>% echarts4r::e_bar_("Injoignable ou impossibilité de repondre", stack = "grp", color="#fac858")
            } 
            if ("Refus" %in% names(nbdossier_enqueteur)) {
                chart_enq <- chart_enq %>% echarts4r::e_bar_("Refus", stack = "grp", color = "#E8465B") 
            }  
            chart_enq <- chart_enq %>%
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
# mod_suivi_etat_ui("suivi_etat_1")
    
## To be copied in the server
# mod_suivi_etat_server("suivi_etat_1")
