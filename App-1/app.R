library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)
library(DT)
library(bslib) # Essential for card-based UI

# 1. Load Data
parks_df <- read_delim("../data/parks.csv", delim = ";")

# 2. UI Definition using page_sidebar and cards
ui <- page_sidebar(
  title = "Vancouver Park Dashboard",
  
  sidebar = sidebar(
    title = "Filters",
    textInput("search", "Search Park by Name", placeholder = "Enter park name..."),
    selectizeInput(
      "neighbourhood", "Neighbourhood",
      choices = sort(unique(na.omit(parks_df$NeighbourhoodName))),
      selected = "Downtown",
      multiple = TRUE
    ),
    sliderInput(
      "size", "Hectare",
      min = min(parks_df$Hectare, na.rm = TRUE),
      max = max(parks_df$Hectare, na.rm = TRUE),
      value = c(min(parks_df$Hectare, na.rm = TRUE), max(parks_df$Hectare, na.rm = TRUE))
    ),
    checkboxGroupInput(
      "facilities", "Select Facilities",
      choices = c("Washrooms" = "Washrooms", 
                  "Facilities" = "Facilities", 
                  "Special Features" = "SpecialFeatures")
    ),
    actionButton("reset_all", "Reset all filters", class = "btn-outline-secondary btn-sm")
  ),
  
  # MAIN CONTENT AREA
  layout_column_wrap(
    width = 1/2,
    heights_equal = "row",
    
    # Card 1: Table
    card(
      card_header("Table of Data"),
      DTOutput("table_out"),
      full_screen = TRUE
    ),
    
    # Card 2: Bar Chart (The logic you requested)
    card(
      card_header("Washroom Availability"),
      plotlyOutput("washroom_chart"),
      full_screen = TRUE
    )
  ),
  
  # Card 3: Map
  card(
    card_header("Map"),
    leafletOutput("park_map"),
    full_screen = TRUE
  ),
  
  # Styling to match your Python theme colors
  theme = bs_theme(
    version = 5,
    primary = "#285F2A",
    "card-cap-bg" = "#f8f9fa"
  )
)

# 3. Server Logic
server <- function(input, output, session) {
  
  # Reactive filtering
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
  
  # Table Output
  output$table_out <- renderDT({
    datatable(
      filtered() %>% select(Name, NeighbourhoodName, Hectare, Washrooms),
      options = list(pageLength = 5, scrollY = "200px", dom = 'tp'),
      rownames = FALSE
    )
  })
  
  # THE ADAPTED BAR CHART LOGIC
  output$washroom_chart <- renderPlotly({
    # Calculate counts based on ALL parks that have Washrooms='Y'
    # Then highlight based on user selection
    all_counts <- parks_df %>%
      filter(Washrooms == "Y") %>%
      group_by(NeighbourhoodName) %>%
      summarise(Count = n(), .groups = 'drop')
    
    selected <- input$neighbourhood
    
    # Define colors: Dark green for selected/none, grey otherwise
    all_counts <- all_counts %>%
      mutate(Color = ifelse(
        is.null(selected) | NeighbourhoodName %in% selected,
        "#285F2A", 
        "#bdbdbd"
      ))
    
    avg_val <- mean(all_counts$Count)
    
    plot_ly(all_counts, x = ~NeighbourhoodName, y = ~Count, type = 'bar',
            marker = list(color = ~Color)) %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Total Washrooms"),
        # Horizontal average line
        shapes = list(
          list(type = "line", x0 = 0, x1 = 1, xref = "paper",
               y0 = avg_val, y1 = avg_val, yref = "y",
               line = list(color = "#ef9a9a", dash = "dot"))
        ),
        margin = list(b = 80)
      )
  })
  
  # Map Output
  output$park_map <- renderLeaflet({
    df <- filtered()
    coords <- tidyr::separate(df, GoogleMapDest, into = c("lat", "lng"), sep = ",", convert = TRUE)
    
    leaflet(coords) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        color = "#285F2A", radius = 6, fillOpacity = 0.8,
        popup = ~paste0("<b>", Name, "</b><br>Size: ", Hectare, " ha")
      ) %>%
      # Native Count Control
      addControl(
        html = tags$div(
          style = "background: rgba(255,255,255,0.8); padding: 8px; border-radius: 5px;",
          tags$b("Park Count: "), nrow(df)
        ),
        position = "topright"
      )
  })
  
  # Reset Button logic
  observeEvent(input$reset_all, {
    updateTextInput(session, "search", value = "")
    updateSelectizeInput(session, "neighbourhood", selected = "Downtown")
    updateSliderInput(session, "size", value = c(min(parks_df$Hectare), max(parks_df$Hectare)))
    updateCheckboxGroupInput(session, "facilities", selected = character(0))
  })
}

shinyApp(ui, server)