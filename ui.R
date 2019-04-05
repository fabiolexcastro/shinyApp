
suppressMessages(if(!require(tidyverse)){install.packages('tidyverse'); library(tidyverse)} else {library(tidyverse)})
suppressMessages(if(!require(grid)){install.packages('grid'); library(grid)} else {library(grid)})
suppressMessages(if(!require(gtable)){install.packages('gtable'); library(gtable)} else {library(gtable)})
suppressMessages(if(!require(rsconnect)){install.packages('rsconnect'); library(rsconnect)} else {library(rsconnect)})
suppressMessages(if(!require(shiny)){install.packages('shiny'); library(shiny)} else {library(shiny)})
suppressMessages(if(!require(leaflet)){install.packages('leaflet'); library(leaflet)} else {library(leaflet)})# Create a numeric input controll
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(rgeos)){install.packages('rgeos'); library(rgeos)} else {library(rgeos)})

# source('www/dual_axis.txt')

# Define UI for data upload app ----

fluidPage(
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel('Select the coordinates into de map',
             numericInput(inputId =  'Lon', label = 'Longitude:', value = uiOutput("lon.value"), min = -Inf, max = Inf),
             numericInput(inputId =  'Lat', label = 'Latitude:', value = uiOutput("lat.value"), min = -Inf, max = Inf),
             fluidRow(titlePanel("Click on the map") ,column(12,
                             wellPanel(
                               leafletOutput("mymap") 
                             )
                             )   
                      )
    ),
    tabPanel('Plot two axis - Precipitations & Temperature',  mainPanel(tittle = paste0('Climate graph for'), plotOutput("plot1") )
             
    )
    
  ))