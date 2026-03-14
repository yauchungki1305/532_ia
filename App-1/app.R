library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)
library(DT)      # For the scrollable table
library(bslib)   # For the modern card layout (matches Python)

# Load data
parks_df <- read_delim("../data/parks.csv", delim = ";")

ui <- page_sidebar(
  title = "Vancouver Park Dashboard",
  
  sidebar = sidebar(
    title = "Filters",
    textInput("search", "Search Parks", placeholder = "Enter keywords..."),
    selectizeInput("neighbourhood", "Neighbourhood",
                   choices = sort(unique(na.omit(parks_df$NeighbourhoodName))),
                   multiple = TRUE),
    sliderInput("size", "Hectare",
                min = min(parks_df$Hectare, na.rm = TRUE),
                max = max(parks_df$Hectare, na.rm = TRUE),
                value = c(min(parks_df$Hectare, na.rm = TRUE), max(parks_df$Hectare, na.rm = TRUE))),
    checkboxGroupInput("facilities", "Select Facilities",
                       choices = c("Washrooms" = "Washrooms", 
                                   "Facilities" = "Facilities", 
                                   "Special Features" = "SpecialFeatures")),
    hr(),
    helpText("Adjust filters to update the charts.")
  ),
  
  # THE MAIN CONTENT AREA
  # layout_column_wrap ensures the top two cards stay side-by-side
  layout_column_wrap(
    width = 1/2,
    height = 350, # Fixes the height of the top row
    
    card(
      card_header("Table of data"),
      DTOutput("table_out"),
      full_screen = TRUE # Allows user to expand the table to full screen
    ),
    
    card(
      card_header("Washroom availability"),
      plotlyOutput("washroom_chart")
    )
  ),
  
  # Bottom Map Card
  card(
    card_header("Map"),
    leafletOutput("park_map"),
    full_screen = TRUE
  )
)

server <- function(input, output, session) {
  
  filtered <- reactive({
    df <- parks_df
    if (input$search != "") {
      df <- df %>% filter(grepl(input$search, Name, ignore.case = TRUE))
    }
    if (!is.null(input$neighbourhood)) {
      df <- df %>% filter(NeighbourhoodName %in% input$neighbourhood)
    }
    df <- df %>% filter(Hectare >= input$size[1] & Hectare <= input$size[2])
    if (!is.null(input$facilities)) {
      for (f in input$facilities) {
        df <- df %>% filter(!!sym(f) == "Y")
      }
    }
    df
  })
  
  # Render DT (DataTable) with fixed scrolling
  output$table_out <- renderDT({
    df <- filtered() %>%
      mutate(Address = paste(StreetNumber, StreetName)) %>%
      select(Name, Address, Neighbourhood = NeighbourhoodName)
    
    datatable(df, 
              options = list(
                scrollY = "180px",  # Forces the table to scroll WITHIN the card
                paging = FALSE,     # Removes pagination buttons to save space
                dom = 't',          # Shows ONLY the table (hides search/length)
                scrollX = TRUE      # Handles wide columns
              ),
              rownames = FALSE)
  })
  
  output$park_map <- renderLeaflet({
    df <- filtered()
    coords <- tidyr::separate(df, GoogleMapDest, into = c("lat", "lng"), sep = ",", convert = TRUE)
    
    leaflet(coords) %>%
      addTiles() %>%
      setView(lng = -123.1207, lat = 49.2827, zoom = 12) %>%
      addMarkers(lng = ~lng, lat = ~lat, popup = ~Name) %>%
      addControl(
        html = tags$div(
          style = "background: white; padding: 10px; border-radius: 8px; text-align: center; border: 1px solid grey;",
          tags$b("COUNT"), br(),
          tags$span(style = "font-size: 20px; color: #2e7d32;", nrow(df))
        ),
        position = "topright"
      )
  })
  
  output$washroom_chart <- renderPlotly({
    df <- filtered()
    counts <- df %>% group_by(Washrooms) %>% summarise(Count = n())
    plot_ly(counts, labels = ~Washrooms, values = ~Count, type = 'pie') %>%
      layout(margin = list(l=10, r=10, b=10, t=10))
  })
}

shinyApp(ui, server)