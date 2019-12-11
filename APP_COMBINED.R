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
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYXJub3V0YyIsImEiOiJjazNkaGVqMXQwN2N1M2hvbmloc3RmMndpIn0.Enq7kzFNMLHVCl_Q0-bTuw')

detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties

detroit_rms_crime2$GEOID <- substring(detroit_rms_crime2$block_id, 1, 12)

df <- detroit_rms_crime2 %>% filter(year > 2017)

#crimes_by_GEOID <- df %>% group_by(GEOID,offense_category) %>% tally()

crimes_by_GEOID <- df %>% group_by(GEOID) %>% tally()

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

# mapview(counties_df, zcol = "estimate", legend = TRUE)

# counties_df %>% filter(offense_category=="SEX OFFENSES") %>% filter(estimate>0) %>% mapview(zcol = "CRIME_PER_PERSON", legend = TRUE)
# counties_df %>% filter(offense_category=="SEX OFFENSES") %>% mapview(zcol = "n", legend = TRUE)

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

crime_data <- counties_df %>% select(GEOID, population,Crimes,CRIME_PER_PERSON)

mapview_df <- crime_data %>% inner_join(as.data.frame(demographics), by = "GEOID")



mapview_df$geometry <- mapview_df$geometry.y

############################################################## SECOND MAP ######################################################################
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(ggthemes)
library(jsonlite)

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYXJub3V0YyIsImEiOiJjazNkaGVqMXQwN2N1M2hvbmloc3RmMndpIn0.Enq7kzFNMLHVCl_Q0-bTuw')

detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties
detroit_rms_crime2$Date <- as.POSIXct(detroit_rms_crime2$incident_timestamp)
# detroit_rms_crime2$Time <- as.POSIXct(detroit_rms_crime2$incident_time)

detroit_rms_crime2$block_group_id <- substring(detroit_rms_crime2$block_id, 1, 12)

df2 <- detroit_rms_crime2 %>% filter(year > 2015)

df2$Crime <- as.factor(df2$offense_category)
## Define the Tooltip Hover Info that I want to display
df2$hover <- with(df2, paste('Precinct:',precinct, '<br>','Description:',charge_description, '<br>', "Time of Day:", incident_time, '<br>',"Day of Week:",day_of_week))


df2 <- df2 %>% mutate(day_of_week = case_when(
    .$day_of_week == 1 ~ "Sunday",
    .$day_of_week == 2 ~ "Monday",
    .$day_of_week == 3 ~ "Tuesday",
    .$day_of_week == 4 ~ "Wednesday",
    .$day_of_week == 5 ~ "Thursday",
    .$day_of_week == 6 ~ "Friday",
    .$day_of_week == 7 ~ "Saturday",
))


df2 <- df2 %>% mutate(Crime_Type = case_when(
    .$Crime == "STOLEN PROPERTY" ~ "Stealing",
    .$Crime == "STOLEN VEHICLE" ~ "Stealing",
    .$Crime == "ROBBERY" ~ "Stealing",
    .$Crime == "BURGLARY" ~ "Stealing",
    .$Crime == "ASSAULT" ~ "Violent",
    .$Crime == "AGGRAVATED ASSAULT" ~ "Violent",
    .$Crime == "ARSON" ~ "Damage to Property",
    .$Crime == "DAMAGE TO PROPERTY" ~ "Damage to Property",
    .$Crime == "HOMICIDE" ~ "Violent",
    .$Crime == "JUSTIFIABLE HOMICIDE" ~ "Violent",
    .$Crime == "LARCENY" ~ "Stealing",
    .$Crime == "OUIL" ~ "Weapons/Drugs/Gambling",
    .$Crime == "LIQUOR" ~ "Weapons/Drugs/Gambling",
    .$Crime == "DANGEROUS DRUGS" ~ "Weapons/Drugs/Gambling",
    .$Crime == "WEAPONS OFFENSES" ~ "Weapons/Drugs/Gambling",
    .$Crime == "GAMBLING" ~ "Weapons/Drugs/Gambling",
    .$Crime == "SOLICITATION" ~ "Sex Related",
    .$Crime == "STOLEN PROPERTY" ~ "Sex Related",
    .$Crime == "STOLEN PROPERTY" ~ "Sex Related",
    TRUE ~ "Other"
))

## Define the optionsdots that they are able to choose

optionsdots <- df2 %>%  mutate(Crime_Type = as.character(Crime_Type))
optionsdots <- unique(sort(optionsdots$Crime_Type))

optionsdots2 <- df2 %>%  mutate(day_of_week = as.character(day_of_week))
optionsdots2 <- unique(sort(optionsdots2$day_of_week))

############################################################################################################################################
"18 and 19 Year Old Males"


ui <- navbarPage("Detroit Crime Map",
                 tabPanel("Crimes Tab",
                          mainPanel(mapviewOutput("map", width = "100%", height = 800))
                 ),
                 tabPanel("Crimes Per Person Tab",
                          mainPanel(mapviewOutput("crime", width = "100%", height = 800))
                 ),
                 tabPanel("Median Age",
                          mainPanel(mapviewOutput("age", width = "100%", height = 800))
                 ),
                 tabPanel("Population",
                          mainPanel(mapviewOutput("Population", width = "100%", height = 800))
                 ),
                 tabPanel("18 and 19 Year Old Males",
                          mainPanel(mapviewOutput("males", width = "100%", height = 800))
                 ),
                 # ,tabPanel("Plotly",
                 #          sliderInput("Crimes", label = h3("Crimes in Area"), min = 0, max = max(mapview_df$Crimes), 
                 #                      value = c(25, 75)),
                 #          mainPanel(plotlyOutput("map2",height = "800px"), width = 12)
                 #),
                 tabPanel("Crime Deep Dive",
                          selectInput("Crime_Type", "Crime_Type:", optionsdots, width = "100%",multiple = FALSE,selected = "Violent"),
                         # selectInput("day_of_week", "Day of Week:", optionsdots2, width = "100%",multiple = TRUE,selected = "Monday"),
                          dateRangeInput("Date", "Date range:",
                                         start  = "2016-01-01",
                                         end    = "2020-01-01",
                                         min    = "2016-01-01",
                                         max    = "2020-01-01",
                                         format = "mm/dd/yy",
                                         separator = " - "),
                          mainPanel(plotlyOutput("crime_dots",height = "800px"), width = 12)
                 )
)

server <- function(input, output) {
    
    
    output$map <- renderMapview({
        
        mapview_df %>%  mapview(zcol = "Crimes", legend = TRUE)
        
    })
    
    output$crime <- renderMapview({
        
        mapview_df %>% filter(population > 0) %>% mapview(zcol = "CRIME_PER_PERSON", legend = TRUE)
        
    })
    
    
    output$age <- renderMapview({
        
        mapview_df %>% filter(variable == "Median Age") %>%  mapview(zcol = "estimate", legend = TRUE)
        
    })
    
    output$Population <- renderMapview({
        
        mapview_df %>% filter(variable == "Population" ) %>%  mapview(zcol = "estimate", legend = TRUE)
        
    })
    
    output$males <- renderMapview({
        
        mapview_df %>% filter(variable == "18 and 19 Year Old Males" ) %>%  mapview(zcol = "estimate", legend = TRUE)
        
    })
    
    # output$map2 <- renderPlotly({
    #     
    #     p <- mapview_df %>% filter(variable == "Median Age") %>% filter(Crimes > input$Crimes[1]) %>% filter(Crimes < input$Crimes[2]) %>% ggplot( aes(fill = estimate, color = estimate)) + 
    #         geom_sf() + 
    #         #coord_sf(crs = 26910) + 
    #         #facet_wrap(~metro_name, scales = "free", nrow = 1) + 
    #         theme_minimal() + 
    #         #theme(aspect.ratio = 1) + 
    #         scale_fill_viridis() + 
    #         scale_color_viridis()
    #     
    #     
    #     
    #     ggp <- ggplotly(p)
    #     
    #     ggp
    #     
    #     
    # })
    
    
    
    output$crime_dots <- renderPlotly({
        
        ##perfrom the crime filter
        df2 <- df2  %>%
            filter(Crime_Type == input$Crime_Type) %>% filter(Date > input$Date[1]) %>% filter(Date < input$Date[2]) #%>% filter(day_of_week == input$day_of_week)
        
        
        p <- plot_mapbox(mode = 'scattermapbox') %>%
            add_markers(
                data = df2, x = ~longitude, y = ~latitude, text=~hover, color=~Crime_Type, size=1, hoverinfo = "text", alpha = 0.7) %>%
            layout(
                plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
                mapbox = list(style = 'dark',
                              zoom = 10,
                              center = list(lat = median(df2$latitude),
                                            lon = median(df2$longitude))),
                margin = list(l = 0, r = 0,
                              b = 0, t = 0,
                              pad = 0),
                showlegend=FALSE)
        
        p
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)