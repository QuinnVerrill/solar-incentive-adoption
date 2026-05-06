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
library(thematic)

#filter harms data so plot shows up
all_harms <- c("airborne_sand", "personal_health", "noise", "safety_concern", "sedimentation", "truck_traffic", "harm_to_wildlife")
available_harms <- intersect(all_harms, names(data)) # Which columns actually exist in your dataset?
disabled_harms <- setdiff(all_harms, available_harms) # Disable the ones that don’t exist
data$comment_date <- as.Date(data$comment_date, origin = "1899-12-30")

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()

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

# ---- Server ----
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
    df <- geocoded_2
    nrow(df)
  })
 

  #recent reports 2
  output$latest_report <- renderText({
    df <- filtered_data()
    df <- df[!is.na(df$incident_start_date), ]
    
    if (nrow(df) == 0) return("No reports")
    
    #format(max(df$comment_date), "%Y-%m-%d")
    format(max(as.Date(df$incident_start_date), na.rm = TRUE), "%B %d, %Y")
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
                       group = "Sand Mines") %>%
  #plot AQ sensor
      addCircleMarkers(data = AQ_loc,
                     lng = ~Long, 
                     lat =  ~Lat, 
                     radius = 3,
                     #radius = ~volume_norm * 10,
                     color = aq_color,
                     #fill = "#FF8B28", 
                     opacity = 1,
                    
                      #this is not live.. ? 
                     # popup = ~paste("<strong>Location of sensor:</strong> ", Location, "<br>",
                     #                "<strong>Link to Live Air Quality Results:</strong> ", Link
                     #                #"<a href='", Link, "' target='_blank'>", Link, "</a>"), #this makes the link live
                     # ),
                     #
                     
                     #this shows all
                     # popup = ~htmltools::HTML(paste(
                     #   "<strong>Location of sensor:</strong> ", Location, "<br>",
                     #   "<strong>Link:</strong> ",
                     #   "<a href='", Link, "' target='_blank'>View results</a>"
                     # ),
                    
                     group = "Air Quality Sensors") %>%
      addLegend(
        position = "bottomright",
        colors = c(incident_color, sandmine_color, aq_color),
        labels = c("Incident Reports", "Sand Mines", "Air Quality Sensors"),
        title = "Legend",
        opacity = 1
      )
  
    
    #These currently break the app "object '.xts_chob' not found"
    # %>%
    #   addLayersControl(
    #     overlayGroups = c("Sand Mines", "Incident Reports"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   ) %>%
    #   
 

  

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
                       clusterOptions = markerClusterOptions(maxClusterRadius = 50,
                                                             disableClusteringAtZoom = 16,
                                                             spiderfyOnMaxZoom = TRUE,
                                                             #showCoverageOnHover = TRUE,
                                                             #zoomToBoundsOnClick = FALSE,
                                                             spiderfyDistanceMultiplier = 2),
                       group = "Incident Reports"
                       )

})

} #ends the server part of code


# ---- Run app ----
shinyApp(ui = ui, server = server)

