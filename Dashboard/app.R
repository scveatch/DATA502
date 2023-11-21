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
#data3 <- read.csv("../Project Data/joinedData2.csv")
#power_plants <- read_csv('../Project Data/PowerPlants.csv')
#coal_plants <- power_plants%>%
#  filter(PrimSource == 'coal')%>%
#  filter(!State %in% c('Alaska', 'Hawaii', 'Puerto Rico'))

data3 <- readRDS('../Project Data/data3.rds')
coal_plants <- readRDS('../Project Data/coal_plants.rds')

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
           aes(long, lat)) + 
      geom_polygon(aes(group = group, fill = PM25_pop_pred)) + 
      scale_fill_gradient(low = "blue", high = "red", na.value = "grey") +
      geom_point(data=coal_plants, aes(Longitude, Latitude)) +
      coord_quickmap() + 
      theme_void()
    # Produce Plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
