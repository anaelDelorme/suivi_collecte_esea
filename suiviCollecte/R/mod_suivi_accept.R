#' suivi_accept UI Function
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
mod_suivi_accept_ui <- function(id){
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
          title = "Suivi accept régional",
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
          leaflet::leafletOutput(ns("map_nb_questionnaire_non_accept"))
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
          title = "Suivi accept départemental",
          status = "lightblue",
          width = 12,
          echarts4rOutput(ns("pie_questionnaire_par_etat_par_departement"))
      )),
  fluidRow(
      bs4Dash::box(
          title = "Suivi accept par enquêteur intervenant sur le département",
          status = "lightblue",
          width = 12,
          echarts4rOutput(ns("graph_enqueteur_departement"))
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
        nom_region <-  unique(r$data_suivi %>% arrange(REP_LIB_REG_1) %>%  pull(REP_LIB_REG_1))
        nom_region <- stringr::str_to_title(nom_region)
        updatePickerInput(session, inputId = "region_picker", choices = nom_region)    

        nom_departement <- unique(r$data_suivi %>% 
                                            filter(!is.na(REP_LIB_DEPT_1))%>% arrange(REP_LIB_DEPT_1) %>% pull(REP_LIB_DEPT_1))
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
                                            filter(!is.na(REP_LIB_DEPT_1))%>% 
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
            leaflet::leafletProxy(ns("map_nb_questionnaire_non_accept")) %>%
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
            leaflet::leafletProxy(ns("map_nb_questionnaire_non_accept")) %>%
                  leaflet::setView(lng = zoom_lon, lat = zoom_lat, zoom = 8)
        }


      })

    
    #### Nombre de questionnaires par accept
    output$pie_questionnaire_par_etat <- renderEcharts4r({
       dossier <- r$data_suivi
       dossier_accept <- dossier %>%
          filter(!is.na(ACCEPT)) %>% 
            mutate(etat_accept = 
                case_when(
                  ACCEPT == "1" & CESSATION == "1" ~ "Cessation",
                  ACCEPT == "1" ~ "Répondu (hors cessation)",
                  ACCEPT %in% c("2", "3")~ "Injoignable ou impossibilité de repondre",
                  ACCEPT ==  "9"  ~ "Refus",
                )
        ) %>% 
        group_by(etat_accept) %>% 
        count() %>% 
        ungroup()

factor_level_accept <- c("Répondu (hors cessation)", "Cessation","Injoignable ou impossibilité de repondre", "Refus")

dossier_accept$etat_accept = factor(dossier_accept$etat_accept, levels = factor_level_accept, ordered = TRUE)
dossier_accept <- dossier_accept %>%
  arrange(match(etat_accept, factor_level_accept))

df_colours <- data.frame(etat_accept = factor_level_accept,
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
                        ACCEPT == "1" ~ "Répondu (hors cessation)",
                        ACCEPT %in% c("2", "3")~ "Injoignable ou impossibilité de repondre",
                        ACCEPT ==  "9"  ~ "Refus",
                      )
              ) %>% 
              group_by(etat_accept) %>% 
              count() %>% 
              ungroup()

      
      factor_level_accept <- c("Répondu (hors cessation)", "Cessation","Injoignable ou impossibilité de repondre", "Refus")

      dossier_accept$etat_accept = factor(dossier_accept$etat_accept, levels = factor_level_accept, ordered = TRUE)
      dossier_accept <- dossier_accept %>%
        arrange(match(etat_accept, factor_level_accept))


      df_colours <- data.frame(etat_accept = c("Répondu (hors cessation)", "Cessation","Injoignable ou impossibilité de repondre", "Refus" ),
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
### Carte régionale 
 output$map_nb_questionnaire_non_accept <- renderLeaflet({
    dossiers <- r$data_suivi %>%
          filter(ACCEPT != "1") %>% 
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
          filter(ACCEPT != "1") %>% 
          count(REP_CODE_REG_1, REP_LIB_REG_1)%>% 
          mutate(REP_LIB_REG_1 = stringr::str_to_title(REP_LIB_REG_1))%>% 
          mutate(REP_CODE_REG_1 = as.numeric(REP_CODE_REG_1)) 
          
        data_carte_rond_proportionnel <-  r$map_regions_centroid  %>% 
          left_join(dossiers, by = c("REG" = "REP_CODE_REG_1")) 
        adaptation_rond = 1

    }else{
        dossiers <- r$data_suivi %>%
          filter(ACCEPT != "1") %>% 
           count(REP_CODE_DEPT_1, REP_LIB_DEPT_1)%>% 
          mutate(REP_LIB_DEPT_1 = stringr::str_to_title(REP_LIB_DEPT_1))%>% 
          filter(!is.na(REP_CODE_DEPT_1))%>%
          mutate(REP_CODE_DEPT_1 = as.character(REP_CODE_DEPT_1)) 
          
        data_carte_rond_proportionnel <-  r$map_departements_centroid  %>% 
          left_join(dossiers, by = c("DEP" = "REP_CODE_DEPT_1")) %>% 
          rename(Name = Nom)
         adaptation_rond = 2
    }
 
         leaflet::leafletProxy(ns("map_nb_questionnaire_non_accept"), data = data_carte_rond_proportionnel) %>%
            leaflet::clearShapes() %>%
            leaflet::clearMarkers()%>%
            leaflet::addPolygons(color = "#343a40", weight = 0.3, smoothFactor = 0.5,
                       opacity = 1.0, fillOpacity = 0.5,fillColor = "white") %>%
            leaflet::addCircleMarkers(
              lng = ~centroid_longitude,
              lat = ~centroid_latitude,
              radius = ~ adaptation_rond * n,
              fillOpacity = ~ifelse(is.na(n), 0, 0.8),
              color = "#e91f1fb6",
              stroke = FALSE,
                label = ~paste0(Name, ": ",n)
            )
    })


### Pie par département
    #### Nombre de questionnaires par accept
    output$pie_questionnaire_par_etat_par_departement <- renderEcharts4r({
      if (!is.null(input$departement_picker) && input$departement_picker !=""){
      
            dossier <- r$data_suivi
            dossier_accept <- dossier %>%
              filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$departement_picker)) %>% 
              filter(!is.na(ACCEPT)) %>% 
                  mutate(etat_accept = 
                      case_when(
                        ACCEPT == "1" & CESSATION == "1" ~ "Cessation",
                        ACCEPT == "1" ~ "Répondu (hors cessation)",
                        ACCEPT %in% c("2", "3")~ "Injoignable ou impossibilité de repondre",
                        ACCEPT ==  "9"  ~ "Refus",
                      )
              ) %>% 
              group_by(etat_accept) %>% 
              count() %>% 
              ungroup()
      
      factor_level_accept <- c("Répondu (hors cessation)", "Cessation","Injoignable ou impossibilité de repondre", "Refus")

      dossier_accept$etat_accept = factor(dossier_accept$etat_accept, levels = factor_level_accept, ordered = TRUE)
      dossier_accept <- dossier_accept %>%
        arrange(match(etat_accept, factor_level_accept))

      if (nrow(dossier_accept) > 0){
      df_colours <- data.frame(etat_accept = c("Répondu (hors cessation)", "Cessation","Injoignable ou impossibilité de repondre", "Refus" ),
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
      }else{return(NULL)}
    })

  ### Graph empilé par enquêteur
  output$graph_enqueteur_departement <- renderEcharts4r({
      if (!is.null(input$region_picker) && input$region_picker != ""){
          if(!is.null(input$departement_picker) && input$departement_picker != ""){
            liste_enqueteur <- r$data_suivi %>% 
              filter(REP_LIB_DEPT_1 == stringr::str_to_upper(input$departement_picker)) %>% 
              filter(!is.na(CODE_ENQUETEUR)) %>% 
              filter(ACCEPT %in% c("1", "2", "3", "9")) %>% 
              pull(CODE_ENQUETEUR)

            nbdossier_enqueteur <- r$data_suivi %>% 
              filter(CODE_ENQUETEUR %in% liste_enqueteur) %>% 
            mutate(etat_accept = 
                case_when(
                  ACCEPT == "1" ~ "Répondu (tous)",
                  ACCEPT %in% c("2", "3")~ "Injoignable ou impossibilité de repondre",
                  ACCEPT ==  "9"  ~ "Refus",
                )
        ) %>%  
              group_by(CODE_ENQUETEUR,etat_accept,NOM_ENQ,PRENOM_ENQ) %>% 
              count() %>% 
              ungroup() %>% 
              mutate(Enquêteur = paste(NOM_ENQ, PRENOM_ENQ, sep = " ")) %>%
              tidyr::pivot_wider(names_from = etat_accept, values_from= n)
              
            #print(nbdossier_enqueteur) 

            chart_enq <- nbdossier_enqueteur %>%
              echarts4r::e_charts(Enquêteur) 

            if ("Répondu (tous)" %in% names(nbdossier_enqueteur)) {
                chart_enq <- chart_enq %>% echarts4r::e_bar_("Répondu (tous)", stack = "grp", color="#5470c6") 
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
# mod_suivi_accept_ui("suivi_accept_1")
    
## To be copied in the server
# mod_suivi_accept_server("suivi_accept_1")
