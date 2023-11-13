#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(ggplot2)

# Import Data
data3 <- read.csv("YOUR PATH HERE")

# Set Color

### MAP FUNCTIONS ###


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("PM 2.5 Mapper"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", 
                  h3("Select a Year"), 
                  min = 2016, 
                  max = 2019, 
                  value = 2016),
      
      selectInput("week",
                  h3("Select a week:"),
                  choices = c(unique(data3$week)),
                  selected = 1)
    ),
    
    mainPanel(
      plotOutput("choropleth")
    )
  )

  
)
 

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Main Choropleth
  output$choropleth <- renderPlot({
    # Define Inputs
    iyear <- input$year
    iweek <- input$week
    # Make Plot
    ggplot(data3 %>% filter(year == iyear & week == iweek), 
           aes(long, lat, group = group, fill = PM25_pop_pred)) + 
      geom_polygon() + 
      scale_fill_gradient(low = "blue", high = "red", na.value = "grey") + 
      coord_quickmap() + 
      theme_void()
    # Produce Plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
