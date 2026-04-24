#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)

#filter harms data so plot shows up
all_harms <- c("airborne_sand", "personal_health", "noise", "safety_concern", "sedimentation", "truck_traffic", "harm_to_wildlife")
available_harms <- intersect(all_harms, names(data)) # Which columns actually exist in your dataset?
disabled_harms <- setdiff(all_harms, available_harms) # Disable the ones that don’t exist


ui <- fluidPage(
  
  # Title
 # titlePanel("Sand Mines and Incidents in SE Massachussets"),
  
  div(
    img(src = "clwc_logo.png", height = "45px", style = "margin-right: 15px;"),
    style = "
    color: #1f4e79;
    background-color: white;
    padding: 20px;
    font-size: 28px;
    font-weight: bold;
  ",
    "Sand Mines and Incidents in SE Massachussets"
  ),
  
  tags$head(
    tags$style(HTML("
    
    /* ---- Overall background ---- */
    body {
      background-color: #f2f2f2;
      font-family: 'Arial Black', 'Helvetica', sans-serif;
    }
    
    /* ---- Header styling ---- */
    .navbar, .titlePanel {
      background-color: #1f4e79;
      color: white;
      padding: 15px;
      font-weight: bold;
      font-size: 24px;
    }
    
    /* ---- Sidebar ---- */
    .well {
      background-color: #e6e6e6;
      border-radius: 0px;
      border: none;
    }
    
    /* ---- Main panel ---- */
    .panel {
      border-radius: 0px;
    }
    
    /* ---- Inputs (blocky feel) ---- */
    .form-control, .selectize-input {
      border-radius: 0px;
      border: 2px solid #1f4e79;
    }
    
    /* ---- Checkbox labels ---- */
    .checkbox label {
      font-weight: bold;
    }
    
    /* ---- Leaflet map container ---- */
    #comment_map {
      border: 3px solid #1f4e79;
    }
    
  "))
  ),

 
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
          strong("Sand Mine Count"),
          br(),
          textOutput("total_mines")
        )
      ),
      
      # Plot output
      leafletOutput("comment_map", height = 600)    
      )
  )
)

# ---- Server ----
server <- function(input, output) {
  
incident_color <- "cyan"
sandmine_color <- "#FF8B28"
  
 data$incident_start_date <- as.Date(data$incident_start_date)
  
  # Reactive filtering 
  filtered_data <- reactive({
    req(input$date_range)
    
    df <- data %>%
      filter(incident_start_date >= input$date_range[1],
             incident_start_date <= input$date_range[2])
    
    
    # Filter by resident
    if (input$resident != "All") {
      df <- df %>% filter(name2 == input$resident)
    }
    
    # code by selected harms (WIP)
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
  
  # total reports
  output$total_reports <- renderText({
    df <- filtered_data()
    nrow(df)
  })
  
  output$total_mines <- renderText({
    df <- geocoded_2()
    nrow(df)
  })
 
  #recent reports
  output$latest_report <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("No reports")
    max(df$comment_date, na.rm = TRUE)
  })
 
  
  #leaflet map
  output$comment_map <- renderLeaflet({
      leaflet() %>%
      addTiles() %>% 
      addProviderTiles(providers$Esri.WorldImagery)%>%
      
      #plot sand mine locations
      addCircleMarkers(data = geocoded_2,
                       lng = ~lon, 
                       lat =  ~lat, 
                       radius = 3,
                       #radius = ~volume_norm * 10,
                       color = sandmine_color,
                       #fill = "#FF8B28", 
                       opacity = 1,
                       popup = ~paste("<strong>Location:</strong> ", location, "<br>",
                                      "<strong>Owner:</strong> ", owner, "<br>",
                                      "<strong>Size (acres):</strong> ", size, "<br>",
                                      "<strong>Volume Extracted:</strong> ", volume, "<br>"),
                       group = "Sand Mines") 
    #These currently break the app "object '.xts_chob' not found"
    # %>%
    #   addLayersControl(
    #     overlayGroups = c("Sand Mines", "Incident Reports"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   ) %>%
    #   
    #     addLegend(
    #       position = "bottomright",
    #       colors = c("#FF8B28", "yellow"),
    #       labels = c("Sand Mines", "Incident Reports"),
    #       opacity = 1
    #     )
      
 }) 
  

    # Update map reactively
    observe({
      df <- filtered_data()
      
      #plot resident comments
      leafletProxy("comment_map", data = df) %>%
      clearGroup("Incident Reports") %>%
      addCircleMarkers(data = df,
                       lng = ~long2, 
                       lat =  ~lat2, 
                       radius = 8,
                       #color = incident_color,
                       color = ~ifelse(matches_harm, incident_color, "gray"),
                       fillColor = ~ifelse(matches_harm, incident_color, "gray"),
                       #fill = "yellow",
                       fillOpacity = 1,
                       popup = ~paste("<strong>Start Date:</strong> ", incident_start_date, "<br>",
                                      "<strong>Harms Reported:</strong> ", harms_list, "<br>",
                                      "<strong>Comment:</strong> ", comment2),
                       #clusterOptions = markerClusterOptions(),
                       group = "Incident Reports"
                       )

})

}

# ---- Run app ----
shinyApp(ui = ui, server = server)

