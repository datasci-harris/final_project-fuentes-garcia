rm(list=ls())
options(scipen = 10^6, digits=2)

library(shiny)
library(tidyverse)
library(lubridate)
library(sf)
library(shinythemes)
library(rsconnect)
library(plotly)
library(scales)

# Victor
#wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)


setAccountInfo(name='nanojgarcia', 
               token='3D27515E9473E538F46606D5C46EA075', 
               secret='C9pgrbt7XyTYjYnS3rhH1W89XhFro9VxZMgpDBZ/')

spatial_df <- readRDS("panel_elections.RDS")

# Define UI 
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Electoral Results by State in the U.S."),
                
                fluidRow(
                    column(4, selectInput(inputId = "date", 
                            label = "Choose an election year",
                            choices = seq(1976, 2020, by = 4))
                           ),
                
                column(6, tableOutput("candidates")
                       ),
                    
                # Show a map of us elections
                mainPanel(width = 11, plotlyOutput("elections")
                          )
                )
)

# Define server 
server <- function(input, output) {
    df <- filter(spatial_df, candidate_win == 1)
    party_colors <- c("#2E74C0", "#CB454A")
    
    
    output$elections <- renderPlotly({
        p = ggplot(data = filter(df, year == input$date)) +
            geom_sf(aes(fill = party_simplified)) +
            geom_sf_text(aes(label = paste(state_abbv,"\n",electoral_votes)), 
                         size =2
                             ) +
            labs(title = "Visualization of Election Results by State", 
                 x = NULL, y = NULL, fill = "") +
            theme(panel.grid.major = element_line(colour = "transparent")) +
            theme_void()
        
        p1  = p + scale_fill_manual(values = party_colors)  
        ggplotly(p1)
    })
    spatial_df$geometry <- NULL 

    output$candidates <- renderTable(spatial_df %>%
                                        rename(Party = party_simplified, 
                                               Candidate = candidate) %>%
                                        mutate(electoral_win = candidate_win * as.double(electoral_votes)) %>%
                                        filter((Party == "DEMOCRAT" | 
                                                        Party =="REPUBLICAN") &
                                                   year == input$date) %>%
                                        group_by(Party, Candidate) %>%
                                        summarise('Popular vote' = round(sum(candidatevotes), 0),
                                                    'Electoral vote' = round(sum(electoral_win), 0)
                                                  ), digits = 0
                                     ) 
}

# Run the application 
shinyApp(ui = ui, server = server)

#https://nanojgarcia.shinyapps.io/Covid19/