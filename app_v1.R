library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(RColorBrewer)
library(cachem)
library(shinythemes)


rm(list = ls())
gc(T)

# Leia os bancos de dados
dengue_geocoded_ARCGIS <- read.csv("./DENGUE_ARCGIS.csv", sep = ",") %>%
  filter(!is.na(lat)) %>%
  mutate(SG_UF_NOT = as.numeric(SG_UF_NOT))

dengue_geocoded_OSM <- read.csv("./DENGUE_OSM.csv", sep = ",") %>%
  filter(!is.na(lat)) %>%
  mutate(SG_UF_NOT = as.numeric(SG_UF_NOT))

dengue_geocoded_GOOGLE <- read.csv("./DENGUE_GOOGLE.csv", sep = ",") %>%
  filter(!is.na(lat)) %>%
  mutate(SG_UF_NOT = as.numeric(SG_UF_NOT))

dengue_geocoded_CNEFE <- read.csv("./DENGUE_CNEFE.csv", sep = ",") %>%
  filter(!is.na(lat)) %>%
  mutate(SG_UF_NOT = as.numeric(SG_UF_NOT))

# UI
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  titlePanel("Painel de Geoencoding de Agravos de Notificação Compulsória"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "apis_comparacao",
        label = "Selecione as APIs para comparar:",
        choices = c("Google", "ArcGIS", "OSM"),
        selected = c("Google")
      ),
      checkboxInput(
        inputId = "exibir_cnefe",
        label = "CNEFE (Padrão de comparação)",
        value = FALSE
      ),
      sliderInput(
        inputId = "threshold",
        label = "Limiar de Distância (em metros):",
        min = 0,
        max = 1000,
        value = 100,
        step = 10
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Informações sobre Acurácia",
          tableOutput("tabelaDistancias"),
          plotOutput("graficoObservacoes")
        ),
        tabPanel("Visualização Espacial dos Dados", leafletOutput("mapaAcuracia"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  api_data <- reactive({
    list(
      "Google" = dengue_geocoded_GOOGLE,
      "ArcGIS" = dengue_geocoded_ARCGIS,
      "OSM" = dengue_geocoded_OSM,
      "CNEFE" = dengue_geocoded_CNEFE
    )
  })
  
  # Calcular distâncias e acurácia
  output$tabelaDistancias <- renderTable({
    selected_apis <- input$apis_comparacao
    threshold <- input$threshold
    
    if (length(selected_apis) < 1) {
      return(data.frame(Mensagem = "Selecione pelo menos uma API para calcular distâncias e acurácia."))
    }
    
    results <- lapply(selected_apis, function(api) {
      api_df <- api_data()[[api]]
      cnefe_df <- api_data()[["CNEFE"]]
      
      combined <- inner_join(
        api_df %>% select(ID_UNICO, lat_api = lat, long_api = long),
        cnefe_df %>% select(ID_UNICO, lat_cnefe = lat, long_cnefe = long),
        by = "ID_UNICO"
      )
      
      if (nrow(combined) == 0) {
        return(data.frame(API = api, Acuracia = NA, `Distância (m)` = NA, `Distância (km)` = NA))
      }
      
      api_sf <- st_as_sf(combined, coords = c("long_api", "lat_api"), crs = 4326, remove = FALSE)
      cnefe_sf <- st_as_sf(combined, coords = c("long_cnefe", "lat_cnefe"), crs = 4326, remove = FALSE)
      
      distancias <- st_distance(api_sf, cnefe_sf, by_element = TRUE)
      distancias <- as.numeric(distancias)
      
      combined$distancia <- distancias
      
      acuracia <- mean(distancias <= threshold)
      distancia_media <- mean(distancias)
      distancia_media_km <- distancia_media / 1000  # Conversão para km
      
      data.frame(
        API = api,
        Acuracia = round(acuracia * 100, 2),
        `Distância (m)` = round(distancia_media, 2),
        `Distância (km)` = round(distancia_media_km, 2)
      )
    })
    
    # Combina os resultados e renomeia as colunas explicitamente
    tabela <- do.call(rbind, results)
    colnames(tabela) <- c("API", "Acurácia (%)", "Distância (m)", "Distância (km)")
    return(tabela)
  })
  
  
  # Gráfico de observações por API
  output$graficoObservacoes <- renderPlot({
    selected_apis <- input$apis_comparacao
    total_observacoes <- lapply(selected_apis, function(api) {
      nrow(api_data()[[api]])
    })
    
    if (input$exibir_cnefe) {
      total_observacoes <- c(total_observacoes, list(CNEFE = nrow(api_data()[["CNEFE"]])))
      selected_apis <- c(selected_apis, "CNEFE")
    }
    
    total_observacoes_df <- data.frame(
      API = selected_apis,
      Observacoes = unlist(total_observacoes),
      Percentual = unlist(total_observacoes) / 20433 * 100
    )
    
    ggplot(total_observacoes_df, aes(x = API, y = Observacoes, fill = API)) +
      geom_bar(stat = "identity", color = "black", width = 0.7) +
      geom_text(
        aes(label = paste0(Observacoes, " (", round(Percentual, 2), "%)")),
        vjust = -0.5,
        size = 5
      ) +
      theme_classic() +
      labs(
        title = "Número Total de Observações por API",
        x = "API",
        y = "Número de Observações"
      ) +
      scale_y_continuous(
        limits = c(0, max(total_observacoes_df$Observacoes) * 1.1),
        expand = c(0, 0)
      ) +
      theme(
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  # Mapa interativo
  output$mapaAcuracia <- renderLeaflet({
    selected_apis <- input$apis_comparacao
    exibir_cnefe <- input$exibir_cnefe
    threshold <- input$threshold
    
    colors <- list("Google" = "green", "ArcGIS" = "red", "OSM" = "purple", "CNEFE" = "blue")
    
    map <- leaflet() %>% addTiles()
    
    for (api in selected_apis) {
      # Filtrar os dados da API selecionada
      api_df <- api_data()[[api]]
      cnefe_df <- api_data()[["CNEFE"]]
      
      # Calcular distâncias
      combined <- inner_join(
        api_df %>% select(ID_UNICO, lat_api = lat, long_api = long),
        cnefe_df %>% select(ID_UNICO, lat_cnefe = lat, long_cnefe = long),
        by = "ID_UNICO"
      )
      
      if (nrow(combined) > 0) {
        # Criar objetos sf para calcular distâncias
        api_sf <- st_as_sf(combined, coords = c("long_api", "lat_api"), crs = 4326, remove = FALSE)
        cnefe_sf <- st_as_sf(combined, coords = c("long_cnefe", "lat_cnefe"), crs = 4326, remove = FALSE)
        distancias <- st_distance(api_sf, cnefe_sf, by_element = TRUE)
        
        # Adicionar distâncias como coluna
        combined$distancia <- as.numeric(distancias)
        
        # Adicionar marcadores ao mapa
        map <- map %>%
          addCircleMarkers(
            data = combined,
            lng = ~long_api,
            lat = ~lat_api,
            radius = 5,
            color = colors[[api]],
            fillOpacity = 0.7,
            popup = ~paste0("API: ", api, "<br>Distância: ", round(distancia, 2), "m")
          )
      }
    }
    
    if (exibir_cnefe) {
      map <- map %>%
        addCircleMarkers(
          data = dengue_geocoded_CNEFE,
          lng = ~long,
          lat = ~lat,
          radius = 3,
          color = colors[["CNEFE"]],
          fillOpacity = 0.7,
          popup = ~paste0("Padrão Ouro - ID: ", ID_UNICO)
        )
    }
    
    map
  })
  
}

onStop(function() {
  # Fechar conexões de banco de dados (se houver)
  # dbDisconnect(con)  # Remova ou comente esta linha se não estiver usando conexões
  
  # Liberar qualquer outro recurso necessário
  print("Encerrando o aplicativo Shiny. Recursos liberados com sucesso.")
})

# Executar a aplicação
shinyApp(ui, server)
