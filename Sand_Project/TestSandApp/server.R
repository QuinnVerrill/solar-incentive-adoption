#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
  
  