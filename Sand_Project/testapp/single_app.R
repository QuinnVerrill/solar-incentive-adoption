ui <- fluidPage(
  
  # # Set CSS theme
  # theme = bs_theme(bootswatch = "darkly",
  #                  bg = "white",
  #                  fg = "#86C7ED",
  #                  success ="#86C7ED"),
  
  
  # Title
  titlePanel("Sand Mines and Incidents in SE Massachussets"),
  
  
  
  
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
        choices = c("All", unique(data$name2)),
        selected = "All"
      ),
      
      # checkboxes for harm type
      
      checkboxGroupInput(
        "harms",
        "Select harms to highlight affected locations",
        choices = c("airborne_sand", "personal_health", "noise", "safety_concern", "sedimentation", "truck_traffic", "harm_to_wildlife"),
        selected = c("airborne_sand", "personal_health", "noise", "safety_concern", "sedimentation", "truck_traffic", "harm_to_wildlife")  #default: show all
      )
      
    ), #end of sidebar panel
    
    
    
    mainPanel(
      
      div(
        style = "display: flex; gap: 30px; margin-bottom: 15px;",
        
        div(
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 6px;",
          strong("Total Reports"),
          br(),
          textOutput("total_reports")
        ),
        
        div(
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 6px;",
          strong("Most Recent Report"),
          br(),
          textOutput("latest_report")
        ),
        
        div(
          style = "padding: 10px; border: 1px solid #ddd; border-radius: 6px;",
          strong("Mapped Sand Mines"),
          br(),
          textOutput("total_mines")
        )
      ),
      
      # Plot output
      leafletOutput("comment_map", height = 600)    
    )
  )
)



server <- function(input, output) {
  
  incident_color <- "red"
  sandmine_color <- "#FF8B28"
  aq_color <- "cyan"
  
  data$incident_start_date <- as.Date(data$incident_start_date)
  
  # Reactive filtering 
  filtered_data <- reactive({
    req(input$date_range)
    
    df <- data %>%
      filter(incident_start_date >= input$date_range[1],
             incident_start_date <= input$date_range[2])
    
    if (input$resident != "All") {
      df <- df %>% filter(name2 == input$resident)
    }
    
    valid_harms <- intersect(input$harms, names(df))
    
    df <- df %>%
      mutate(
        matches_harm = if (length(valid_harms) > 0) {
          if_any(all_of(valid_harms), ~ . == TRUE)
        } else {
          TRUE
        }
      )
    
    df
  })
  
  # Summary outputs
  output$total_reports <- renderText({ nrow(filtered_data()) })
  
  output$total_mines <- renderText({ nrow(geocoded_2) })
  
  output$latest_report <- renderText({
    df <- filtered_data()
    df <- df[!is.na(df$incident_start_date), ]
    if (nrow(df) == 0) return("No reports")
    format(max(as.Date(df$incident_start_date), na.rm = TRUE), "%B %d, %Y")
  })
  
  # Base leaflet map (static layers only) ----
  output$comment_map <- renderLeaflet({
    
    m <- leaflet() %>% addTiles()
    message("Base map OK")
    
   # m <- addProviderTiles("Esri.WorldImagery") 
    
    m <- tryCatch({
      m %>% addCircleMarkers(data = geocoded_2,
                             lng = ~lon, lat = ~lat,
                             radius = 3,
                             color = sandmine_color,
                             opacity = 1,
                             popup = ~paste("<strong>Location:</strong> ", location, "<br>",
                                            "<strong>Owner:</strong> ", owner, "<br>",
                                            "<strong>Size (acres):</strong> ", size, "<br>",
                                            "<strong>Volume Extracted:</strong> ", volume),
                             group = "Sand Mines")
    }, error = function(e) { message("FAILED at Sand Mines: ", e$message); m })
    message("Sand Mines OK")
    
    m <- tryCatch({
      m %>% addCircleMarkers(data = AQ_loc,
                             lng = ~Long, lat = ~Lat,
                             radius = 3,
                             color = aq_color,
                             opacity = 1,
                             group = "Air Quality Sensors")
    }, error = function(e) { message("FAILED at AQ sensors: ", e$message); m })
    message("AQ Sensors OK")
    
    m <- tryCatch({
      m %>% addLegend(position = "bottomright",
                      colors = c(incident_color, sandmine_color, aq_color),
                      labels = c("Incident Reports", "Sand Mines", "Air Quality Sensors"),
                      title = "Legend",
                      opacity = 1)
    }, error = function(e) { message("FAILED at Legend: ", e$message); m })
    message("Legend OK")
    
    m
  })
  
  # Reactive layer: incident reports ----
  observe({
    df <- filtered_data()
    
    leafletProxy("comment_map", data = df) %>%
      clearGroup("Incident Reports") %>%
      addCircleMarkers(lng = ~long2, lat = ~lat2,
                       radius = 8,
                       color = ~ifelse(matches_harm, incident_color, "gray"),
                       fillColor = ~ifelse(matches_harm, incident_color, "gray"),
                       fillOpacity = 1,
                       popup = ~paste("<strong>Start Date:</strong> ", incident_start_date, "<br>",
                                      "<strong>Harms Reported:</strong> ", harms_list, "<br>",
                                      "<strong>Comment:</strong> ", comment2),
                       clusterOptions = markerClusterOptions(maxClusterRadius = 50,
                                                             disableClusteringAtZoom = 16,
                                                             spiderfyOnMaxZoom = TRUE,
                                                             spiderfyDistanceMultiplier = 2),
                       group = "Incident Reports")
  })
  
} # end server

shinyApp(ui = ui, server = server)