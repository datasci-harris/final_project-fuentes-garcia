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
library(lubridate)
library(sf)
library(shinythemes)
library(rsconnect)
library(plotly)
library(scales)

# Fernando
wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)


rsconnect::setAccountInfo(name='nanojgarcia', 
                          token='3D27515E9473E538F46606D5C46EA075', 
                          secret='C9pgrbt7XyTYjYnS3rhH1W89XhFro9VxZMgpDBZ/')

#rsconnect::deployApp('path/to/your/app')

rm(list = ls())

spatial_data <- readRDS("panel_elections.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Electoral Results by State in the U.S."),
                
                # Year input
                selectInput(inputId = "date", 
                            label = "Choose a year",
                            choices = seq(1976, 2020, by = 4)),
                
                    
                    # Show a map of us elections
                    mainPanel(
                        plotlyOutput("elections"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df <- filter(spatial_data, candidate_win == 1)
    party_colors <- c("#2E74C0", "#CB454A")
    

    output$elections <- renderPlotly({
        p = ggplot(data = filter(df, year == input$date)) +
            geom_sf(aes(fill = party_simplified)) +
            geom_sf_text(aes(label = state_abbv), size = 2)
            labs(title = "Election Results input$date", fill = "") +
            theme_minimal()
        
        p1  = p + scale_color_manual(values = party_colors)  
        ggplotly(p1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#https://nanojgarcia.shinyapps.io/Covid19/