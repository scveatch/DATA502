#
# Notes: We need to improve the runtime of this significantly, probably by rewriting some functions or 
# making use of different data structures. Either way, we have a functioning first page drawn up right now,
# With cohesive interactivity. Here are the main issues we need to solve:
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(plotly)
library(sf)
library(maps)

# Import Data
#data3 <- read.csv("../Project Data/joinedData2.csv")
#power_plants <- read_csv('../Project Data/PowerPlants.csv')
#coal_plants <- power_plants%>%
#  filter(PrimSource == 'coal')%>%
#  filter(!State %in% c('Alaska', 'Hawaii', 'Puerto Rico'))

data3 <- readRDS("../Project Data/data3.rds")
coal_plants <- readRDS("../Project Data/coal_plants.rds")
#data3_sf <- readRDS("C:\\Users\\bveat\\OneDrive\\Documents\\data_sf.rds")
lung <- read.csv("../Project Data/USCSOverviewMap.csv")
states <- map_data("state")


# Add Column to data3:
data3$PM2.5_pop_cut <- cut(data3$PM25_pop_pred, breaks = c(-Inf, 5, 10, 20, 40, 60, 100), 
                              labels = c( "0 - 5", "5 - 10", "10 - 20", "20 - 40", "40 - 60", ">100"))

# Set Color

### MAP FUNCTIONS ###
plot_map <- function(input_year, input_week, EPA){
  # Plot Main Choropleth
  if(EPA == TRUE){
    ggplot(data3 %>% filter(year == input_year & date == input_week),
           aes(long, lat)) + 
      geom_polygon(aes(group = group, fill = ifelse(PM25_pop_pred > 35, "red", "blue"))) + 
      scale_fill_identity() + 
      geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "grey30", fill = NA) +
      geom_point(data=coal_plants, aes(Longitude, Latitude)) +
      coord_quickmap() + 
      theme_void() + 
      labs(title = "PM 2.5 Exposure", fill = "PM 2.5 Exposure")
  }
  else{
    # Plot main map -- first page. 
    ggplot(data3 %>% filter(year == input_year & date == input_week),
           aes(long, lat)) + 
      geom_polygon(aes(group = group, fill = PM2.5_pop_cut)) + 
      scale_fill_manual(values = brewer.pal(n = 6, "YlOrRd")) +
      geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "grey30", fill = NA) +
      geom_point(data=coal_plants, aes(Longitude, Latitude)) +
      coord_quickmap() + 
      theme_void() + 
      labs(title = "PM 2.5 Exposure", fill = "PM 2.5 Exposure")
  }
}

plot_bars <- function(state){
  # Plot Cumulative Exposure Bars
  ggplot(data3 %>% filter(state_name == state), aes(x = year, y = PM25_pop_pred)) + 
    geom_col(color = "#355070") + 
    theme_minimal() + 
    labs(title = str_glue("Cumulative exposure to PM2.5 in ", str_to_title(state)), 
         x = "Year", 
         y = "Cumulative Exposure \n(Micrograms per Cubic Meter)")
}

compare_bars <- function(state, year){
  # Compares Selected State with Others
  ggplot(na.omit(data3) %>% filter(year == year) 
         %>% group_by(state_name) %>% summarise(total = sum(PM25_pop_pred)), aes(y = fct_reorder(state_name,total), x =  total)) + 
    geom_col(fill = "#355070") + 
    geom_col(data = na.omit(data3) %>% filter(year == year & state_name == state) 
             %>% group_by(state_name) %>% summarise(total = sum(PM25_pop_pred)),
             aes(y = fct_reorder(state_name, total), x = total), fill = "red") + 
    theme_minimal() + 
    labs(x = str_glue("PM2.5 Exposure in ", year), 
         y = "States")
}

plot_lung <- function(){
  # Plot Lung Cancer Prevalence
  states$region <- str_to_title(states$region)
  lung1 <- left_join(lung, states, by = c("Area" = "region"))
  lung1$Age.Adjusted.Rate <- as.numeric(lung1$Age.Adjusted.Rate)
  lung1$Age.Adjusted.Rate <- cut(lung1$Age.Adjusted.Rate, breaks = c(-Inf, 15, 49, 56, 60, 84.4))
  colors = c("#C1DBFF", "#9DB6DB", "#7992B6", "#355070")
  
  ggplot(lung1, aes(x = long, y = lat)) + 
    geom_polygon(aes(fill = Age.Adjusted.Rate, group = group),color="black") + 
    scale_fill_manual(values = colors, labels = c("15 - 49", "49 - 56", "56 - 60", "60 - 84.4", "No Data")) + 
    geom_point(data=coal_plants, aes(Longitude, Latitude), color = "red") + 
    theme_void() + 
    coord_quickmap() + 
    labs(title = "New Lung Cancer Rates: 2016 - 2020", 
         subtitle = "Cancers of the Lungs and Bronchus, adjusted by age. \nRate per 100,000 people", 
         fill = "Rate per 100,000 People")
}

### Secondary Functions ###
find_closest_index <- function(n1, n2, data) {
  # n1 correlates to x dimension -- longitude
  # n2 correlates to y diemsions -- latitude
  diff1 <- abs(data$long - n1)
  diff2 <- abs(data$lat - n2)
  
  total_diff <- diff1 + diff2
  
  return(which.min(total_diff))
}


# Define UI for application
ui <- dashboardPage(
  #titlePanel("PM 2.5 Mapper"),
  dashboardHeader(title = "The Health Effects of PM 2.5", titleWidth = 400),

  dashboardSidebar(
    # Year Drop-Down
    awesomeRadio(
      inputId = "year",
      label = h3("Select a Year:"),
      choices = c(2016, 2017, 2018, 2019),
      selected = 2016),
    
    # Week Drop-down
    selectInput("week",
                h3("Select a week:"),
                choices = c("01JAN2016"),
                selected="01JAN2016"),
    
    # EPA switch / blurb
    div(
      h5("The EPA classifies exposures to PM2.5 at levels greater than 35 micrograms per
         cubic meter to be dangerously high."),
      switchInput(
        inputId = "showEPA",
        label = "Above EPA Safe Levels", 
        labelWidth = "80px"))
    ),
  
  dashboardBody(
    # Produce Main Plot -- Take up full page
    div(h5("PM 2.5 is fine particulate matter in the atmosphere with a diameter of 2.5 microns or less. It is largely produced when things are burned, whether that be from wildfires or car exhaust. Exposure to large quantities of PM 2.5 is understood to contribute to a variety of health problems.")),
    
    div(h3("Click on a state to view detailed information")),
    
    fluidRow(
      box(plotOutput("choropleth", click = "clickPoint"), width = 12, 
          title = "PM 2.5 Exposure by County, With Coal Power Plant Locations")
      ),
    
    # Produce Comparison / Exposure Bars
    fluidRow(
      box(plotOutput("expbars"), 
          title = "Cumulative Statewide Exposure"), 
      box(plotOutput("compare_bars"), 
          title = "Total Exposures")
    ), 
    
    fluidRow(
      box(plotOutput("lung"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Update Week Dropdown to Only Contain Weeks for Selected Year
  observeEvent(input$year, {
    temp<- data3%>%filter(year == input$year)
    updateSelectInput(session, "week", choices = c(unique(temp$date)), selected=temp$date[1])
  })
  
  # Main Choropleth
  output$choropleth <- renderPlot({
    plot_map(input$year, input$week, input$showEPA)
  })
  
  # Produce Exposure Bar Plot / Comparison Bars for Clicked State
  observeEvent(input$clickPoint, {
    index = find_closest_index(input$clickPoint$x, input$clickPoint$y, states)
    state = str_to_title(states$region[index])
    output$expbars <- renderPlot({plot_bars(state)})
    output$compare_bars <- renderPlot({compare_bars(state, input$year)})
  })
  
  output$lung <- renderPlot({plot_lung()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
