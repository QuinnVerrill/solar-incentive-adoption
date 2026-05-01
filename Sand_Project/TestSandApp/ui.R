#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
  #titlePanel("Sand Mines and Incidents in SE Massachussets"),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f2f2f2;
        font-family: 'Oswald', sans-serif;
      }
    "))
  ),
  
  div(
    style = "
      background-color: #1f4e79;
      color: white;
      padding: 20px;
      font-size: 28px;
      font-weight: bold;
    ",
    "Community Land & Water Coalition Map"
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
      # Plot output
      leafletOutput("comment_map", height = 600)    
    )
  )
)
