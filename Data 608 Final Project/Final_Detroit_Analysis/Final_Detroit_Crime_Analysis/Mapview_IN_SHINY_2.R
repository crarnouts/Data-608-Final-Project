library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(ggthemes)
library(jsonlite)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(mapview)
library(dplyr)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key("0af36703082a8e7234a5d0e141e673d556edfe3b")

detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties

detroit_rms_crime2$GEOID <- substring(detroit_rms_crime2$block_id, 1, 12)

df <- detroit_rms_crime2 %>% filter(year > 2017)

crimes_by_GEOID <- df %>% group_by(GEOID,offense_category) %>% tally()

Wayne_county <- get_acs(geography = "block group", variables = "B01001_001", 
                        state = "MI", county = "Wayne", geometry = TRUE)


Macomb_county <- get_acs(geography = "block group", variables = "B01001_001", 
                         state = "MI", county = "Macomb", geometry = TRUE)


Oakland_county <- get_acs(geography = "block group", variables = "B01001_001", 
                          state = "MI", county = "Oakland", geometry = TRUE)

counties_df <- rbind(Oakland_county,Wayne_county,Macomb_county)

mergedata <- df %>% distinct(GEOID)

counties_df <- counties_df %>% inner_join(crimes_by_GEOID)

counties_df$CRIME_PER_PERSON <- (counties_df$n/counties_df$estimate)/2

counties_df$moe <- NULL

library(mapview)

mapview(counties_df, zcol = "estimate", legend = TRUE)

counties_df %>% filter(offense_category=="SEX OFFENSES") %>% filter(estimate>0) %>% mapview(zcol = "CRIME_PER_PERSON", legend = TRUE)
counties_df %>% filter(offense_category=="SEX OFFENSES") %>% mapview(zcol = "n", legend = TRUE)

################# bring in more data ###############################


vars <- c("B01001_007","B01002_001","B00001_001")


Wayne_county <- get_acs(geography = "block group", variables = vars , 
                        state = "MI", county = "Wayne", geometry = TRUE)


Macomb_county <- get_acs(geography = "block group", variables = vars, 
                         state = "MI", county = "Macomb", geometry = TRUE)


Oakland_county <- get_acs(geography = "block group", variables = vars, 
                          state = "MI", county = "Oakland", geometry = TRUE)

counties_df2 <- rbind(Oakland_county,Wayne_county,Macomb_county)



counties_df2$moe <- NULL

counties_df2$variable <- case_when(
    counties_df2$variable == "B01001_007" ~ "18 and 19 Year Old Males",
    counties_df2$variable == "B01002_001" ~ "Median Age",
    counties_df2$variable == "B00001_001" ~ "Population",
)


## additional demographics

demographics <- counties_df2 %>% select(GEOID,variable,estimate)

counties_df <- counties_df %>% rename(population = estimate, Crimes = n)

crime_data <- counties_df %>% select(GEOID, population,offense_category,Crimes,CRIME_PER_PERSON)


#mapview_df <- crime_data %>% st_join(demographics)

mapview_df <- crime_data %>% inner_join(as.data.frame(demographics), by = "GEOID")

mapview_df$Crime <- as.factor(mapview_df$offense_category)

options <- mapview_df %>%  mutate(offense_category = as.character(offense_category))
options <- unique(sort(options$offense_category))



ui <- navbarPage("Detroit Crime Map",
                 tabPanel("Map",
                          selectInput("Crime", "Crime:", options, width = "100%"),
                          #selectInput("day_of_week", "day_of_week:", options2, width = "100%"),
                          mainPanel(mapviewOutput("map", width = "100%", height = "100%"))
                 )
)

server <- function(input, output) {
    
    ##perfrom the crime filter
    mapview_df <- mapview_df  %>%
        filter(Crime == input$Crime)
    
    
    output$map <- renderMapview({
        mapview_df %>% mapview(zcol = "Crimes", legend = TRUE)
        #mapview_df %>% filter(variable == "Median Age") %>% mapview(zcol = "estimate", legend = TRUE)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)