rm(list=ls())
options(scipen = 10^6)

library(shiny)
library(tidyverse)
library(lubridate)
library(urbnmapr)
library(sf)
library(shinythemes)
library(rsconnect)
library(plotly)
library(scales)
library(kableExtra)

# Victor
wd <- "C:/Users/fuent/OneDrive - The University of Chicago/Winter 2021/Data & Programming II - R/Project/final_project-fuentes-garcia"
# Fernando
#wd <- "C:/Users/Nano/Dropbox/My PC (DellXPS13)/Desktop/MPP/R2/final_project-fuentes-garcia"
setwd(wd)


setAccountInfo(name = 'nanojgarcia', 
               token = '3D27515E9473E538F46606D5C46EA075', 
               secret = 'C9pgrbt7XyTYjYnS3rhH1W89XhFro9VxZMgpDBZ/')

panel_elections <- read_csv(file.path("Intermediate","1976-2020_panel_elections.csv"))
states_sf <- get_urbn_map("states", sf = TRUE)

# Define UI 
ui <- fluidPage(
    theme = shinytheme("flatly"),
    
    # Application title
    titlePanel("US Presidential Electoral Results by State, 1976 - 2020"),
    
    fluidRow(
        column(4,
               sliderInput(inputId = "date", 
                           label = "Choose an election year",
                           min = min(panel_elections$year),
                           max = max(panel_elections$year),
                           step = 4,
                           value = max(panel_elections$year),
                           animate = animationOptions(interval = 4900))
               ),
        
        column(6, htmlOutput("candidates")
        ),
        
        # Show a map of us elections
        mainPanel(width = 11, plotlyOutput("elections")
                  )
        )
)

# Define server 
server <- function(input, output) {
    
    df <-
        reactive({panel_elections %>%
        filter(year == input$date,
               winner == 1)})

    party_colors <- c("#2E74C0", "#CB454A")
    
    output$elections <-
        renderPlotly({
            year_title <- mean(df()$year)
            
            ggplotly(
                states_sf %>%
                    left_join(df(), by = c("state_abbv" = "state_lab")) %>%
                    ggplot() +
                    geom_sf(aes(fill = party), color = "white", size = 0.5) +
                    geom_sf_text(aes(label = paste(state_abbv, "\n", electoral_votes)), 
                                 size = 2)+
                    scale_fill_manual(values = party_colors) + 
                    theme_minimal() + 
                    labs(fill = "") +
                    theme(legend.position = c(0.9, 0.15),
                          panel.background = element_blank(),
                          axis.title = element_blank(),
                          axis.text = element_blank(),
                          panel.grid = element_blank(),
                          plot.title = element_text(hjust = 0.5)),
                tooltip = c("state", "party", "votes")
                ) %>%
                layout(title = year_title) %>%
                highlight("plotly_selected")
            })
    
    output$candidates <-
        renderText(
            panel_elections %>%
                filter(year == input$date, type == "absolute") %>%
                group_by(candidate, party) %>%
                summarise(total = sum(votes),
                          electoral = sum(electoral_votes * winner)) %>%
                kable(col.names = c("Candidate", "Party", "Popular", "Electoral"),
                      full_width = F,
                      align = c("lccc"),
                      format.args = list(big.mark = ",", scientific = FALSE)) %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
                add_header_above(c(" " = 1, " " = 1, "Votes" = 2))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

#https://nanojgarcia.shinyapps.io/Covid19/