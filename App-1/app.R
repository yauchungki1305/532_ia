library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)

parks_df <- read_delim("../data/parks.csv", delim = ";")
ui <- fluidPage(
  titlePanel("Vancouver Park Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Search input
      textInput("search", "Search Parks", placeholder = "Enter keywords..."),
      
      # Neighbourhood selection
      selectizeInput(
        "neighbourhood", "Neighbourhood",
        choices = sort(unique(na.omit(parks_df$NeighbourhoodName))),
        multiple = TRUE
      ),
      
      # Slider for park size
      sliderInput(
        "size", "Hectare",
        min = min(parks_df$Hectare, na.rm = TRUE),
        max = max(parks_df$Hectare, na.rm = TRUE),
        value = c(min(parks_df$Hectare, na.rm = TRUE), max(parks_df$Hectare, na.rm = TRUE))
      ),
      
      # Checkbox group for facilities
      checkboxGroupInput(
        "facilities", "Select Facilities",
        choices = c("Washrooms" = "Washrooms", 
                    "Facilities" = "Facilities", 
                    "Special Features" = "SpecialFeatures")
      ),
      
      hr(),
      helpText("Adjust filters to update the charts.")
    ),
    
    mainPanel(
      # Top Row: Table and Pie Chart
      fluidRow(
        column(6, 
               wellPanel(
                 h4("Table of data"),
                 tableOutput("table_out")
               )
        ),
        column(6, 
               wellPanel(
                 h4("Washroom availability"),
                 plotlyOutput("washroom_chart", height = "300px")
               )
        )
      ),
      
      # Bottom Row: Map
      fluidRow(
        column(12,
               wellPanel(
                 h4("Map"),
                 leafletOutput("park_map", height = "500px")
               )
        )
      )
    )
  )
)
server <- function(input, output, session) {
  
  # Reactive calculation for filtered data
  filtered <- reactive({
    df <- parks_df
    
    # Filter by search text
    if (input$search != "") {
      df <- df %>% filter(grepl(input$search, Name, ignore.case = TRUE))
    }
    
    # Filter by neighbourhood
    if (!is.null(input$neighbourhood)) {
      df <- df %>% filter(NeighbourhoodName %in% input$neighbourhood)
    }
    
    # Filter by Hectare slider
    df <- df %>% filter(Hectare >= input$size[1] & Hectare <= input$size[2])
    
    # Filter by facilities checkbox
    if (!is.null(input$facilities)) {
      for (f in input$facilities) {
        df <- df %>% filter(!!sym(f) == "Y")
      }
    }
    
    df
  })
  # Render Table
  output$table_out <- renderTable({
    df <- filtered()
    df %>%
      mutate(Address = paste(StreetNumber, StreetName)) %>%
      select(Name, Address, Neighbourhood = NeighbourhoodName, URL = NeighbourhoodURL)
  })
  
  # Render Leaflet Map
  output$park_map <- renderLeaflet({
    df <- filtered()
    
    # Logic for parsing GoogleMapDest (e.g., "49.28,-123.12")
    # R handles coordinates slightly differently than Python's Marker loop
    coords <- tidyr::separate(df, GoogleMapDest, into = c("lat", "lng"), sep = ",", convert = TRUE)
    
    m <- leaflet(coords) %>%
      addTiles() %>%
      setView(lng = -123.1207, lat = 49.2827, zoom = 12) %>%
      addMarkers(
        lng = ~lng, lat = ~lat,
        popup = ~paste0("<b>", Name, "</b><br>",
                        "Neighbourhood: ", NeighbourhoodName, "<br>",
                        "Size: ", Hectare, " ha")
      ) %>%
      # Add the custom Count Box
      addControl(
        html = tags$div(
          style = "background: rgba(255, 255, 255, 0.8); backdrop-filter: blur(4px); 
                   padding: 10px; border-radius: 8px; border: 1px solid rgba(0,0,0,0.1); 
                   text-align: center; min-width: 100px; box-shadow: 0 4px 15px rgba(0,0,0,0.15);",
          tags$div(style = "font-size: 10px; color: #444; font-weight: bold; letter-spacing: 1px;", "COUNT"),
          tags$div(style = "font-size: 24px; color: #2e7d32; font-weight: 800; line-height: 1;", nrow(df))
        ),
        position = "topright"
      )
    
    m
  })
  
  # Render Plotly Pie Chart
  output$washroom_chart <- renderPlotly({
    df <- filtered()
    
    counts <- df %>%
      group_by(Washrooms) %>%
      summarise(Count = n()) %>%
      mutate(Washrooms = ifelse(Washrooms == "Y", "Yes", "No"))
    
    plot_ly(counts, labels = ~Washrooms, values = ~Count, type = 'pie',
            marker = list(colors = c('No' = 'lightgreen', 'Yes' = 'darkgreen'))) %>%
      layout(showlegend = TRUE)
  })
}


shinyApp(ui = ui, server = server)