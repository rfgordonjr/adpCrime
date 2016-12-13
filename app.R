#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(leaflet)
library(Hmisc)
library(lubridate)
library(maptools)
library(foreign)

apdCrimeData <- read_excel("/Users/robertgordon/Documents/apdCrimeData/apdFirstProj/apdCrime1/COBRA110416.xlsx"
                           , sheet="Query")
names(apdCrimeData) <- gsub(" ", "_", names(apdCrimeData))
apdCrimeDataTidy <- apdCrimeData %>% 
  mutate(MI_PRINX = as.numeric(MI_PRINX),
         offense_id = as.numeric(offense_id),
         rpt_date = lubridate::as_date(rpt_date, format = "%m/%d/%Y"),
         occur_date = lubridate::as_date(occur_date, format = "%m/%d/%Y"),
         poss_date = lubridate::as_date(poss_date, format = "%m/%d/%Y"),
         x = as.numeric(x),
         y = as.numeric(y))
errors_horiz_offset <- c(91350923, 91420511, 91471067, 91521689, 101540909, 
                         101701138, 111971638, 112090917, 112411694, 113130827, 
                         113221244, 113270554, 113531411, 113590628, 120230979, 
                         122561142, 130101490, 141621526, 142570818, 151362710)
errors_strange_date <- c(141260924)
errors_all <- c(errors_horiz_offset, errors_strange_date)
apdCrimeDataClean <- apdCrimeDataTidy %>% 
  filter(!(offense_id %in% errors_all)) 
apdCrimeDataErrors <- apdCrimeDataTidy %>% 
  filter(offense_id %in% errors_all) 
beats <- readShapeSpatial("/Users/robertgordon/Documents/apdCrimeData/apdFirstProj/apdCrime1/1909FAB1-9E7F-4A34-8CDD-142D9DC83E7C/APD-Beats-070116_region.shp")
zones <- readShapeSpatial("/Users/robertgordon/Documents/apdCrimeData/apdFirstProj/apdCrime1/1909FAB1-9E7F-4A34-8CDD-142D9DC83E7C/APD-Zones-070116_region.shp")
beats_dbf <- read.dbf("/Users/robertgordon/Documents/apdCrimeData/apdFirstProj/apdCrime1/1909FAB1-9E7F-4A34-8CDD-142D9DC83E7C/APD-Beats-070116_region.dbf")
zones_dbf <- read.dbf("/Users/robertgordon/Documents/apdCrimeData/apdFirstProj/apdCrime1/1909FAB1-9E7F-4A34-8CDD-142D9DC83E7C/APD-Zones-070116_region.dbf")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("ATL Police Department Crime Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("crime",
                     "Select a crime:",
                     unique(apdCrimeDataClean$UC2_Literal))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("myMap", height = 800)
         # tableOutput("table")
      )
   )
))

server <- shinyServer(function(input, output) {
  
  byBeat <- reactive({
    apdCrimeDataClean %>% 
      # filter(UC2_Literal=="HOMICIDE") %>% 
      filter(UC2_Literal %in% input$crime) %>% 
      group_by(beat) %>% 
      summarise(freq = n()) %>% 
      ungroup()
  })
  
  output$table <- renderTable({
    byBeat()
  })
   
   output$myMap <- renderLeaflet({
     mypal <- colorNumeric(
       palette = "YlOrRd",
       domain = byBeat()$freq
     )
     apdCrimeDataClean %>% 
       # filter(UC2_Literal=="HOMICIDE") %>% 
       leaflet() %>% 
       addTiles() %>% 
       addPolylines(data=beats, stroke=TRUE, fillOpacity = 0.5, smoothFactor=0.5,
                    color="black") %>%
       addPolygons(data=beats, stroke=FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
                   color = ~mypal(byBeat()$freq)) %>% 
       addLegend("bottomright", pal = mypal, values = byBeat()$freq,
                 title = "Legend", opacity = 1)
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

