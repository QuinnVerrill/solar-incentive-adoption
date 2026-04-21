#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ui <- fluidPage(
  
  # Title
  titlePanel("My Location-Based Date Explorer"),
  
  # Layout
  sidebarLayout(
    
    sidebarPanel(
      # Date range selector
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = min(data$incident_start_date, na.rm = TRUE),
        end = max(data$incident_start_date, na.rm = TRUE),
        min = min(data$incident_start_date, na.rm = TRUE),
        max = max(data$incident_start_date, na.rm = TRUE)
      ),
   
      # 2. indiv Resident name dropdown
      selectInput(
        "resident",
        "Select Resident:",
        choices = c("All", unique(data$resident_name)),
        selected = "All"
      ),
      
      # checkboxes for harm type
      checkboxGroupInput(
        "harms",
        "Select Harms:",
        choices = c("airborne_sand", "personal_health", "noise"),
        selected = c("airborne_sand", "personal_health", "noise")  #default: show all
      )
      
    ),
    
    
    
    mainPanel(
      # Plot output
      leafletOutput("map_plot", height = 600)    
      )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  
 data$incident_start_date <- as.Date(data$incident_start_date)
  
  # Reactive filtering 
  filtered_data <- reactive({
    req(input$date_range)
    
    df <- data %>%
      filter(incident_start_date >= input$date_range[1],
             incident_start_date <= input$date_range[2])
    
    
    # Filter by resident
    if (input$resident != "All") {
      df <- df %>% filter(resident_name == input$resident)
    }
    
    # Filter by selected harms
    if (length(input$harms) > 0) {
      df <- df %>%
        filter(
          if_any(all_of(input$harms), ~ . == TRUE)
        )
    } else {
      # If no harms selected → show nothing
      df <- df[0, ]
    }
    
    df
  })
    

  
  #leaflet map
  output$map_plot <- renderLeaflet({
      leaflet() %>%
      addTiles() %>% 
      addProviderTiles(providers$Esri.WorldImagery)
      
  })
      
    # Update map reactively
    observe({
      df <- filtered_data()
      
      leafletProxy("map_plot", data = df) %>%
      clearMarkers() %>%
      addCircleMarkers(data = df,
                       lng = ~long2, 
                       lat =  ~lat2, 
                       radius = 8,
                       color = "#FFAC1C",
                       #fillColor = "yellow",
                       fillOpacity = 1,
                       popup = ~paste("<strong>Start Date:</strong> ", incident_start_date, "<br>",
                                      "<strong>Harms Reported:</strong> ", harms_list, "<br>",
                                      "<strong>Comment:</strong> ", comment2),
                       clusterOptions = markerClusterOptions()
                       )
  })
  

}

# ---- Run app ----
shinyApp(ui = ui, server = server)

