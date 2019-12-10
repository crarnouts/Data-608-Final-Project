library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(ggthemes)
library(jsonlite)

#superzip <- read.csv("https://raw.githubusercontent.com/rstudio/shiny-examples/master/063-superzip-example/data/superzip.csv")


Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYXJub3V0YyIsImEiOiJjazNkaGVqMXQwN2N1M2hvbmloc3RmMndpIn0.Enq7kzFNMLHVCl_Q0-bTuw')

states <- read.csv("https://raw.githubusercontent.com/crarnouts/Data_608/master/states.csv", colClasses=c("state_fips"="character"))
df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv') %>% merge(states)
state_options <- sort(df$state_name)
# 
# testing <- fromJSON("https://gis.detroitmi.gov/arcgis/rest/services/OpenData/RMSCrimeIncidents/FeatureServer/0/query?where=1%3D1&outFields=offense_description,offense_category,arrest_charge,charge_description,incident_timestamp,incident_time,day_of_week,hour_of_day,year,precinct,longitude,latitude&outSR=4326&f=json")
# # 
# test2<- testing$features$attributes

detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties
# detroit_rms_crime2$Date <- as.POSIXct(detroit_rms_crime2$incident_timestamp)
# detroit_rms_crime2$Time <- as.POSIXct(detroit_rms_crime2$incident_time)
df <- detroit_rms_crime2 %>% filter(year ==2019)

# df <- test2

#remove(detroit_rms_crime)
#remove(detroit_rms_crime2)

df$Crime <- as.factor(df$offense_category)
## Define the Tooltip Hover Info that I want to display
df$hover <- with(df, paste('Precinct:',precinct, '<br>','Description:',charge_description, '<br>', "Time of Day:", incident_time, '<br>',"Day of Week:",day_of_week))

options <- df %>%  mutate(offense_category = as.character(offense_category))
options <- unique(sort(options$offense_category))

options2 <- df %>%  mutate(day_of_week = as.character(day_of_week))
options2 <- unique(sort(options2$day_of_week))

df <- df %>% mutate(day_of_week = case_when(
    .$day_of_week == 1 ~ "Sunday",
    .$day_of_week == 2 ~ "Monday",
    .$day_of_week == 3 ~ "Tuesday",
    .$day_of_week == 4 ~ "Wednesday",
    .$day_of_week == 5 ~ "Thursday",
    .$day_of_week == 6 ~ "Friday",
    .$day_of_week == 7 ~ "Saturday",
    ))


ui <- navbarPage("Detroit Crime Map",
                 tabPanel("Map",
                          selectInput("Crime", "Crime:", options, width = "100%"),
                          selectInput("day_of_week", "day_of_week:", options2, width = "100%"),
                          mainPanel(plotlyOutput("map",height = "800px"), width = 12)
                 )
)

server <- function(input, output) {
    
    
    output$map <- renderPlotly({
        
        ##perfrom the crime filter
        df <- df  %>%
            filter(Crime == input$Crime)
        
        
        p <- plot_mapbox(mode = 'scattermapbox') %>%
            add_markers(
                data = df, x = ~longitude, y = ~latitude, text=~hover, color=~precinct, size=1, hoverinfo = "text", alpha = 0.5) %>%
            layout(
                plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                mapbox = list(style = 'dark',
                              zoom = 10,
                              center = list(lat = median(df$latitude),
                                            lon = median(df$longitude))),
                margin = list(l = 0, r = 0,
                              b = 0, t = 0,
                              pad = 0),
                showlegend=FALSE)
        
        p
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)