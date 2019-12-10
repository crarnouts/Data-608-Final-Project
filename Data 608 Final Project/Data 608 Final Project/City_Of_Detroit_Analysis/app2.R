library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(ggthemes)

states <- read.csv("https://raw.githubusercontent.com/crarnouts/Data_608/master/states.csv", colClasses=c("state_fips"="character"))
df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv') %>% merge(states)
state_options <- sort(df$state_name)


# 2010 Ranking options
options <- filter(df, Year == 2010) %>%
    mutate(ICD.Chapter = as.character(ICD.Chapter))
options <- unique(sort(options$ICD.Chapter))

ui <- navbarPage("Cause of Death Dashboard",
                 tabPanel("Map",
                          selectInput("cause_of_death_3", "Cause of Death:", options, width = "100%"),
                          mainPanel(plotlyOutput("map"), width = 12)
                 )
)

server <- function(input, output) {
  
    
    output$map <- renderPlotly({
        library(usmap)
        library(ggplot2)
        
        statepop <- statepop
        
        statepop$State <- statepop$abbr
        
        test <- merge(statepop,df) %>% filter(Year == 2010)
        
        test2 <- test  %>%
            filter(ICD.Chapter == input$cause_of_death_3)
        
        test2$hover <- with(test2, paste('Deaths per 100,000','<br>',full, '<br>', " Total Deaths", Deaths))
        # give state boundaries a white border
        l <- list(color = toRGB("white"), width = 2)
        # specify some map projection/options
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        
        p <- plot_geo(test2, locationmode = 'USA-states') %>%
            add_trace(
                z = ~Crude.Rate, text = ~hover, locations = ~State,
                color = ~Crude.Rate, colors = 'Reds'
            ) %>%
            colorbar(title = "Deaths per 100,000") %>%
            layout(
                title = '2010 Death Rate Per 100,000 by Disease and State <br>(Hover for breakdown)',
                geo = g
            )
        
        p
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)