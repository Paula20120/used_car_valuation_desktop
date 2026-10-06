library (tidyverse)
library(shiny)
library(DT)
library(shinydashboard)
library(shinythemes)
library(VIM)

dane <- read_csv2("auta_pulpit.csv", locale = locale(decimal_mark = ","))
str(dane)
ui <- dashboardPage(
  skin = "blue",
  
  # Nagłówek
  dashboardHeader(
    title = tagList(
      icon("car"),
      "Wycena samochodu"
    ),
    titleWidth = 300,
    
    tags$li(class = "dropdown",
            style = "display: flex; justify-content: center; align-items: 
                      center; width: 100%; padding-top: 0px; margin-top: -10px; position: absolute; 
                      left: 50%; transform: translateX(-50%);",
            tagList(
              tags$span(
                style = "color: white; font-size: 22px; margin-right: 10px; padding-top: 5px;",
                "Wybierz rok produkcji:"
              ),
              selectInput("rok_produkcji", "",
                          choices = sort(unique(dane$rok_produkcji)),
                          selected = max(dane$rok_produkcji),
                          width = "220px")  
            )
            
    )
  ),
  
  # Panel boczny z filtrami
  dashboardSidebar(
    tags$style(HTML("
    /* Pasek boczny */
    .main-sidebar {
      background-color: #003153 !important; /* Kolor tła paska bocznego */
      color: white !important; /* Kolor tekstu w pasku bocznym */
      text-align: left !important;
    }

    /* Linki w pasku bocznym */
    .main-sidebar a {
      color: white !important; /* Kolor linków w pasku bocznym */
    }

    /* Zmniejszenie odstępu przed wszystkimi inputami w sidebar */
    .shiny-input-container {
      margin-top: 1px !important;
    }

    /* Zmniejszenie odstępu przed selectInput i jego kontenerem */
    .selectize-input {
      margin-top: 1px !important;
      margin-bottom: 1px !important;
    }

    /* Dodatkowy CSS do zmiany marginesów w form-group, jeśli to konieczne */
    .form-group {
      margin-top: 1px !important;
      margin-bottom: 1px !important;
    }
  ")),
    sidebarMenu(
      menuItem("Wycena", tabName = "wycena", icon = icon("dollar-sign")),
      menuItem("Szczegóły", tabName = "szczegóły", icon = icon("search")),
      menuItem("Podsumowanie", tabName = "podsumowanie", icon = icon("chart-line")),
      menuItem("Baza danych", tabName = "tabela", icon = icon("table"))
    ),
    tags$div(
      style = "padding: 10px;",
      h4(
        tagList(
          icon("sliders"),
          "Filtry")
        ),
      selectInput ("marka_model", "Wybierz markę i model:",
                   choices = unique(dane$marka_model)),
      checkboxInput("filtruj_marke", "Czy filtrować po marce i modelu?", value = FALSE),
      selectInput("zmienna_filtru", "Wybierz zmienną do filtrowania:",
                  choices = c("typ", "uszkodzony", "skrzynia_biegow", "naped"),
                  selected = "typ"),
      uiOutput("dynamiczny_select_input"),
      sliderInput("przebieg_km", "Przebieg (km):", 
                  min = min(dane$przebieg_km, na.rm = TRUE), 
                  max = max(dane$przebieg_km, na.rm = TRUE),
                  value = min(dane$przebieg_km, na.rm = TRUE)),
      sliderInput("pojemnosc_skokowa", "Pojemność (cm³):", 
                  min = min(dane$pojemnosc_skokowa_cm3, na.rm = TRUE),
                  max = max(dane$pojemnosc_skokowa_cm3, na.rm = TRUE),
                  value = min(dane$pojemnosc_skokowa_cm3, na.rm = TRUE)),
      sliderInput("moc_km", "Moc (KM):", 
                  min = min(dane$moc_KM, na.rm = TRUE),
                  max = max(dane$moc_KM, na.rm = TRUE),
                  value = min(dane$moc_KM, na.rm = TRUE)),
      sliderInput("okres_gwarancji", "Okres gwarancji (dni):", 
                  min = min(dane$okres_gwarancji_prod_dni, na.rm = TRUE),
                  max = max(dane$okres_gwarancji_prod_dni, na.rm = TRUE),
                  value = min(dane$okres_gwarancji_prod_dni, na.rm = TRUE)),
      numericInput("liczba_elementow", "Liczba elementów dodatkowych:",
                   min = 0,
                   value = max(dane$licz_dod_element, na.rm = TRUE)),
      textOutput("liczba_elementow_warning")
    )
  ),
  
  # Główna zawartość
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .main-sidebar, .main-header {
        background-color: white !important; /* Ustawienie białego tła */
        text-align: center !important;
      }
        #wycena_finalna{color: black;
                      font-size: 22px;
                      font-style: bold;
        }
    "))
    ),
    tabItems(
      tabItem(tabName = "podsumowanie",
              fluidRow(
                style = "margin-bottom: 30px;",
                plotOutput("statyczny_wykres", height = 500)
              ),
              fluidRow(
                column(6,
                       plotOutput("wykres_elementy", height = 500)
                ),
                column(6,
                       plotOutput("dynamiczny_wykres", height = 500)
                ),
                tags$div(style = "height: 100px;")  # Przestrzeń między elementami
              )
      ),
      tabItem(tabName = "szczegóły",
              fluidRow(
                column(6,
                       style = "margin-bottom: 30px;",
                       plotOutput("wykres_1", height = 500)  
                ),
                column(6,
                       style = "margin-bottom: 30px;",
                       plotOutput("wykres_2", height = 500)  
                )
              ),
              fluidRow(
                column(6,
                       plotOutput("wykres_3", height = 500)  
                ),
                column(6,
                       plotOutput("wykres_4", height = 500)  
                )
              )
      ),
      tabItem(tabName = "tabela",
              box(title = "Baza danych", status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("tabela_danych"))
      ),
      tabItem(tabName = "wycena", 
              fluidRow(
                box(title = "Finalna wycena pojazdu", status = "success", solidHeader = TRUE,
                    textOutput("wycena_finalna", container = span), width = 12)
              ),
              fluidRow(
                box(
                  title = "Wybrane parametry", status = "info", solidHeader = TRUE, width = 12,
                  dataTableOutput("wybrane_parametry"), 
                  tags$style(HTML("
                              .box-header .box-title {
                              font-size: 22px !important;
                                                      }
                                                    ")),
                )
              )
      )
    )
  )
)

  

# Serwer aplikacji
server <- function(input, output, session) {
#zmiana zakresu dla elementów dod
  observe({
    dane_filtr_rok <- dane %>%
      filter(rok_produkcji == input$rok_produkcji)
    
    min_value <- min(dane_filtr_rok$licz_dod_element, na.rm = TRUE)
    max_value <- max(dane_filtr_rok$licz_dod_element, na.rm = TRUE)
 
    updateNumericInput(session, "liczba_elementow", 
                       min = min_value, 
                       max = max_value, 
                       value = max_value) 
  })
  
  # Aktualizacja slider 'przebieg_km' na podstawie wybranego roku
  filtered_data <- reactive({
    dane %>% filter(rok_produkcji == input$rok_produkcji)
  })
  
  observe({
    data_year <- filtered_data()
    updateSliderInput(session, "przebieg_km",
                      min = min(data_year$przebieg_km, na.rm = TRUE),
                      max = max(data_year$przebieg_km, na.rm = TRUE),
                      value = min(data_year$przebieg_km, na.rm = TRUE))
  })
  
  # Aktualizacja slider 'pojemnosc_skokowa' na podstawie wybranego roku
  observe({
    data_year <- filtered_data()
    updateSliderInput(session, "pojemnosc_skokowa",
                      min = min(data_year$pojemnosc_skokowa_cm3, na.rm = TRUE),
                      max = max(data_year$pojemnosc_skokowa_cm3, na.rm = TRUE),
                      value = min(data_year$pojemnosc_skokowa_cm3, na.rm = TRUE))
  })
  
  # Aktualizacja slider 'moc_km' na podstawie wybranego roku
  observe({
    data_year <- filtered_data()
    updateSliderInput(session, "moc_km",
                      min = min(data_year$moc_KM, na.rm = TRUE),
                      max = max(data_year$moc_KM, na.rm = TRUE),
                      value = min(data_year$moc_KM, na.rm = TRUE))
  })
  
  # Aktualizacja slider 'okres_gwarancji' na podstawie wybranego roku
  observe({
    data_year <- filtered_data()
    updateSliderInput(session, "okres_gwarancji",
                      min = min(data_year$okres_gwarancji_prod_dni, na.rm = TRUE),
                      max = max(data_year$okres_gwarancji_prod_dni, na.rm = TRUE),
                      value = min(data_year$okres_gwarancji_prod_dni, na.rm = TRUE))
  })
  
  

  # Dynamiczne zmiany filtrów w zależności od wyboru
  output$dynamiczny_select_input <- renderUI({
   if (input$zmienna_filtru == "typ") {
      selectInput("filtr_typ", "Wybierz typ pojazdu:", choices = sort(unique(dane$typ)))
    } else if(input$zmienna_filtru == "uszkodzony") {
      selectInput("filtr_uszkodzony", "Wybierz opcję uszkodzony:", choices = c("Tak", "Nie"))
    } else if(input$zmienna_filtru == "skrzynia_biegow") {
      selectInput("filtr_skrzynia", "Wybierz skrzynię biegów:", choices = sort(unique(dane$skrzynia_biegow)))
    } else if(input$zmienna_filtru == "naped") {
      selectInput("filtr_naped", "Wybierz napęd:", choices = sort(unique(dane$naped[!is.na(dane$naped)])))
    } 
  })
  
  # Ustawienia dla liczby elementów (maksymalna wartość)
  output$liczba_elementow_warning <- renderText({
    dane_filtr_rok <- dane %>%
      filter(rok_produkcji == input$rok_produkcji)
    
    max_value <- max(dane_filtr_rok$licz_dod_element, na.rm = TRUE)
    
    if(input$liczba_elementow > max_value) {
      paste("Można wpisać maksymalnie", max_value, "elementów.")
    } else {
      ""
    }
  })
  
  output$wycena_finalna <- renderText({
    # Sprawdzenie, czy wszystkie wymagane pola są uzupełnione
    if (is.null(input$rok_produkcji) | is.null(input$marka_model) | 
        is.null(input$filtr_typ) | is.null(input$filtr_uszkodzony) | 
        is.null(input$filtr_skrzynia) | is.null(input$filtr_naped) | 
        is.null(input$przebieg_km) | is.null(input$pojemnosc_skokowa) | 
        is.null(input$moc_km) | is.null(input$okres_gwarancji) | 
        is.null(input$liczba_elementow)) {
      return("Proszę wypełnić wszystkie pola!")
    } else {
      # Inicjalizacja listy do przechowywania średnich cen
      ceny_parametrów <- list()
      
      # Obliczanie średniej ceny dla każdego parametru
      ceny_parametrów$rok_produkcji <- mean(dane %>%
                                              filter(rok_produkcji == input$rok_produkcji) %>%
                                              pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$marka_model <- mean(dane %>%
                                            filter(marka_model == input$marka_model) %>%
                                            pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$filtr_typ <- mean(dane %>%
                                          filter(typ == input$filtr_typ) %>%
                                          pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$filtr_uszkodzony <- mean(dane %>%
                                                 filter(uszkodzony == input$filtr_uszkodzony) %>%
                                                 pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$filtr_skrzynia <- mean(dane %>%
                                               filter(skrzynia_biegow == input$filtr_skrzynia) %>%
                                               pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$filtr_naped <- mean(dane %>%
                                            filter(naped == input$filtr_naped) %>%
                                            pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$przebieg_km <- mean(dane %>%
                                            filter(przebieg_km <= input$przebieg_km) %>%
                                            pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$pojemnosc_skokowa <- mean(dane %>%
                                                  filter(pojemnosc_skokowa_cm3 <= input$pojemnosc_skokowa) %>%
                                                  pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$moc_km <- mean(dane %>%
                                       filter(moc_KM <= input$moc_km) %>%
                                       pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$okres_gwarancji <- mean(dane %>%
                                                filter(okres_gwarancji_prod_dni <= input$okres_gwarancji) %>%
                                                pull(ofertaCena_num), na.rm = TRUE)
      
      ceny_parametrów$liczba_elementow <- mean(dane %>%
                                                 filter(licz_dod_element <= input$liczba_elementow) %>%
                                                 pull(ofertaCena_num), na.rm = TRUE)
      
      # Obliczenie średniej ceny na podstawie średnich cen dla wszystkich parametrów
      srednia_cena_finalna <- sum(unlist(ceny_parametrów), na.rm = TRUE) / length(ceny_parametrów)
      
      return(paste("Proponowana cena pojazdu na podstawie średniej ceny dla poszczególnych parametrów wynosi:",
                   round(srednia_cena_finalna, 0), "zł"))
    }
  })
  
  
  #Wykres_1 
  output$wykres_1 <- renderPlot({
    dane %>%
      filter(
        rok_produkcji == input$rok_produkcji,
        przebieg_km >= input$przebieg_km  
      ) %>%
      ggplot(aes(x = przebieg_km, y = ofertaCena_num)) +
      geom_point(color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
      labs(
        title = "Cena a przebieg",
        x = "Przebieg (km)",
        y = "Cena (zł)"
      ) +
      theme_minimal() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)
      )+
      scale_x_continuous(
        labels = scales::label_number(big.mark = "") 
      ) +
      scale_y_continuous(
        labels = scales::label_number(big.mark = "") 
      )
    
  })
  output$wykres_2 <- renderPlot({
    dane %>%
      filter(
        rok_produkcji == input$rok_produkcji,
        pojemnosc_skokowa_cm3 >= input$pojemnosc_skokowa  
      ) %>%
      ggplot(aes(x = pojemnosc_skokowa_cm3, y = ofertaCena_num)) +
      geom_point(color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
      labs(
        title = "Cena a pojemność silnika",
        x = "Pojemność skokowa (cm3)",
        y = "Cena (zł)"
      ) +
      theme_minimal() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)
      )+
      scale_x_continuous(
        breaks = seq(from = min(dane$pojemnosc_skokowa_cm3), 
                     to = max(dane$pojemnosc_skokowa_cm3), 
                     by = 300),
        labels = scales::label_number(big.mark = "") 
      ) +
      scale_y_continuous(
        labels = scales::label_number(big.mark = "") 
      )
  })
  output$wykres_3 <- renderPlot({
    dane %>%
      filter(
        rok_produkcji == input$rok_produkcji,
        moc_KM >= input$moc_km  
      ) %>%
      ggplot(aes(x = moc_KM, y = ofertaCena_num)) +
      geom_point(color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") + 
      labs(
        title = "Cena a moc silnika",
        x = "Moc (KM)",
        y = "Cena (zł)"
      ) +
      theme_minimal() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)
      )+
      scale_x_continuous(
        breaks = seq(from = min(dane$moc_KM), 
                     to = max(dane$moc_KM), 
                     by = 25),
        labels = scales::label_number(big.mark = "") 
      ) +
      scale_y_continuous(
        labels = scales::label_number(big.mark = "") 
      )
  })
  output$wykres_4 <- renderPlot({
    dane_filtr <- dane %>%
      filter(
        rok_produkcji == input$rok_produkcji,
        okres_gwarancji_prod_dni >= input$okres_gwarancji 
      )
    
    # Wykres punktowy z linią trendu
    ggplot(dane_filtr, aes(x = okres_gwarancji_prod_dni, y = ofertaCena_num)) +
      geom_point(color = "blue", alpha = 0.7) +  # Punkty
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +  
      labs(
        title = "Cena a okres gwarancji producenta",
        x = "Gwarancja (dni)",
        y = "Cena (zł)"
      ) +
      theme_minimal() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)
      )+
      scale_x_continuous(
        breaks = seq(from = min(dane$okres_gwarancji_prod_dni), 
                     to = max(dane$okres_gwarancji_prod_dni), 
                     by = 300),
        labels = scales::label_number(big.mark = "") 
      ) +
      scale_y_continuous(
        labels = scales::label_number(big.mark = "") 
      )
  })
  # Statyczny wykres: Liczba zakupów pojazdów
  output$statyczny_wykres <- renderPlot({
    dane %>%
      count(ofertaData) %>%
      ggplot(aes(x = ofertaData, y = n)) +
      geom_line(color = "green", size = 1.2) +
      labs(title = "Trendy w zakupach pojazdów", x = "Data zakupu", y = "Liczba pojazdów") +
      scale_x_date(labels = scales::date_format("%Y-%m-%d"), breaks = scales::date_breaks("5 days")) + 
      theme_minimal()+
      theme(
        panel.border = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 22),  
        axis.title = element_text(size = 20),  
        axis.text = element_text(size = 16)    
      )
  })
 
  output$dynamiczny_wykres <- renderPlot({
    dane_filtr <- dane %>%
      filter(rok_produkcji == input$rok_produkcji)
    if(input$filtruj_marke) {
      dane_filtr <- dane_filtr %>%
        filter(marka_model == input$marka_model)
    }
    
    #  Średnia cena dla każdej kategorii zmiennej
    srednia_cena_per_kategoria <- dane_filtr %>%
      group_by_at(input$zmienna_filtru) %>%
      summarise(srednia_cena = mean(ofertaCena_num, na.rm = TRUE))
    
    srednia_cena_per_kategoria <- srednia_cena_per_kategoria %>%
      filter(!is.na(!!sym(input$zmienna_filtru)))
    
    text_size <- ifelse(input$zmienna_filtru == "naped", 10, 14)
    

    ggplot(srednia_cena_per_kategoria, aes_string(x = input$zmienna_filtru, y = "srednia_cena")) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
      geom_text(aes(label = round(srednia_cena, 0)), vjust = -0.5, color = "black", size = 4) +
      labs(title = paste("Średnia cena a", input$zmienna_filtru),
           x = input$zmienna_filtru, y = "Średnia cena (zł)") +
      theme_minimal() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = text_size)
      )
  })
  
  output$wybrane_parametry <- renderDataTable({
    datatable(
      data.frame(
        Parametr = c("Rok produkcji", "Marka i model", "Typ", "Uszkodzony", "Skrzynia biegów", "Napęd", 
                     "Przebieg (km)", "Pojemność (cm³)", "Moc (KM)", "Okres gwarancji (dni)", 
                     "Liczba elementów dodatkowych"),
        Wartość = c(
          input$rok_produkcji,
          input$marka_model,
          ifelse(is.null(input$filtr_typ), "Brak wyboru", input$filtr_typ),  
          ifelse(is.null(input$filtr_uszkodzony), "Brak wyboru", input$filtr_uszkodzony),  
          ifelse(is.null(input$filtr_skrzynia), "Brak wyboru", input$filtr_skrzynia),
          ifelse(is.null(input$filtr_naped), "Brak wyboru", input$filtr_naped),
          input$przebieg_km,
          input$pojemnosc_skokowa,
          input$moc_km,
          input$okres_gwarancji,
          input$liczba_elementow
        )
      ),
      options = list(pageLength = 11, 
                     columnDefs = list(
                       list(targets = 0, className = 'dt-left'),
                       list(targets = 1, className = 'dt-left'),  
                       list(targets = 2, className = 'dt-left')   
                     ),
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().container()).css({'font-size': '21px'});",  
                       "}"
                     )
                     
              )
    )
  })
  
  
  output$wykres_elementy <- renderPlot({
    dane_filtr_2 <- dane %>%
      filter(licz_dod_element <= input$liczba_elementow, rok_produkcji == input$rok_produkcji)
    
    if(input$filtruj_marke) {
      dane_filtr_2 <- dane_filtr_2 %>%
        filter(marka_model == input$marka_model)
    }
    
    # Średnią cena dla każdej liczby elementów
    srednia_cena_per_liczba_elementow <- dane_filtr_2 %>%
      group_by(licz_dod_element) %>%
      summarise(srednia_cena = mean(ofertaCena_num, na.rm = TRUE))
    
    # Wykres słupkowy z dodaną linią trendu
    ggplot(srednia_cena_per_liczba_elementow, aes(x = as.factor(licz_dod_element), y = srednia_cena)) +
      geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +  # Słupki
      geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "red", linetype = "dashed") +  
      geom_text(aes(label = round(srednia_cena, 0)), vjust = -0.5, color = "black", size = 3.5) +  
      labs(title = paste("Zależność średniej ceny od liczby elementów dodatkowych wyposażenia"),
           x = "Liczba elementów wyposażenia", y = "Średnia cena (zł)") +
      theme_minimal() +
      theme(
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 21),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16)
      )
  })
  

  # Tabela danych
  output$tabela_danych <- renderDT({
    datatable(
      dane %>% filter(rok_produkcji == input$rok_produkcji),
      options = list(
        scrollX = TRUE,  
        pageLength = 10  
      ),
      rownames = FALSE  
    )
  })
}

shinyApp(ui = ui, server = server)


