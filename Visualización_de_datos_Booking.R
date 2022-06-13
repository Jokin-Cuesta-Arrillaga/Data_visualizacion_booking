#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
df = read.csv('C:/Users/Jokin Cuesta/Documents/DATA_VIS_PEC4/Hotel_Reviews.csv')
#devtools::install_github('rstudio/leaflet')
#devtools::install_github('rstudio/DT')
library(leaflet)
library(dplyr)
library(tidyr)
library(leaflet.extras)
library(scales)
library(DT)

# Definimos la UI
ui <- fluidPage(
  navbarPage("Reviews", id="rev",
             tabPanel("Mapa Interactivo",
 
                          leafletOutput("mymap"),
                          

                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        sidebarPanel(
                                          sliderInput("puntuacion",
                                                      "Puntuación mínima :",
                                                      width = 330,
                                                      min = 1,
                                                      max = 10,
                                                      value = 6),
                                        width = 330
                                        ),
                                        
                                       plotOutput("histplot", height = 200),
                                       # plotOutput("scatterCollegeIncome", height = 250)
                          ),
                          
                          tags$div(id="cite",
                                   'Data recogida para la asignatura M2.859 - Visualización de datos desde ', tags$em(' Kaggle: 515K Hotel Reviews Data in Europe'), ' Por Jokin Cuesta (Junio de 2022).'
                          )
                      ),
             
             tabPanel("Explorador de datos",
                      fluidRow(
                       column(1,
                              numericInput("minScore", "Puntuación mínima", min=0, max=10, value=0)
                        ),
                        column(1,
                               numericInput("maxScore", "Puntuación máxima", min=0, max=10, value=10)
                        )
                      ),
                      hr(),
                      DT::dataTableOutput("reviewtable")
             )
  )
)
# Definimos el server logic
server <- function(input, output,session) {
  output$mymap <- renderLeaflet({
    
    hotel.names = df %>% 
      select(lat,lng,Hotel_Name, Hotel_Address, Average_Score, Total_Number_of_Reviews,
             Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
      #Quitamos 17 entradas sin valores geoespaciales
      filter(lat != 0 & lng != 0 & Average_Score > input$puntuacion) %>%
      group_by(Hotel_Name, Hotel_Address, lng, lat, Average_Score, Total_Number_of_Reviews) %>%
      summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
                Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
                Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
                Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
                Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words))
    
    leaflet() %>% 
      addProviderTiles('OpenStreetMap.Mapnik',
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = hotel.names,
                 popup = paste0("<strong>Hotel: </strong>",
                                hotel.names$Hotel_Name,                 
                                "<br><strong>Dirección: </strong>", 
                                hotel.names$Hotel_Address, 
                                "<br><strong>Puntuación Media: </strong>", 
                                hotel.names$Average_Score, 
                                "<br><strong>Nº de reviews: </strong>", 
                                hotel.names$Total_Number_of_Reviews,
                                "<br><strong>Porcentaje positivo de reviews: </strong>",
                                hotel.names$Pos_Word_Rate),
                 clusterOptions = markerClusterOptions())
    
  })
  
  output$histplot<- renderPlot({
    hotel.names = df %>% 
      select(Hotel_Name,Average_Score,lat,lng) %>%
      #Quitamos 17 entradas sin valores geoespaciales
      filter(lat != 0 & lng != 0 & Average_Score > input$puntuacion) %>%
      group_by(Hotel_Name)
    
    x <- hotel.names$Average_Score
    
    # draw the histogram with the specified number of bins
    hist(x, col = 'black', border = 'white')
  })
  
  output$reviewtable <- DT::renderDataTable({
    df <- df %>%
      filter(
        Average_Score >= input$minScore,
        Average_Score <= input$maxScore,
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', lng))
    action <- DT::dataTableAjax(session, df, outputId = "reviewtable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
