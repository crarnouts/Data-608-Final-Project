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

Wayne_county <- get_acs(geography = "block group", variables = "B01001_002", 
                                            state = "MI", county = "Wayne", geometry = TRUE)


Macomb_county <- get_acs(geography = "block group", variables = "B01001_002", 
                        state = "MI", county = "Macomb", geometry = TRUE)


Oakland_county <- get_acs(geography = "block group", variables = "B01001_002", 
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

#### Try to bring in the appropriate map data hopefully #####


## Load in the Data Dictionaries
my.vars.acs = load_variables(2017, "acs5", cache = TRUE)
head(my.vars.acs)

my.vars.acs$label <- str_replace_all(my.vars.acs$label,"!!","_")

#write.csv(my.vars.acs,file= "C:/Users/arnou/Documents/Data 608 Final Project/Data 608 Final Project/acs5_variables.csv")
## Load in the Data Dictionaries
my.vars.acsprofile = load_variables(2017,"acs5/profile", cache = TRUE)
head(my.vars.acs)

my.vars.acsprofile$label <- str_replace_all(my.vars.acsprofile$label,"!!","_")

#write.csv(my.vars.acs,file= "C:/Users/arnou/Documents/Data 608 Final Project/Data 608 Final Project/acs5_profile_variables.csv")
## Load in the Data Dictionaries



########

test <- my.vars.acs %>% filter(name == 'DP04_0134')

metros <- core_based_statistical_areas(cb = TRUE)

metros <- core_based_statistical_areas(cb = TRUE) %>%
  filter(GEOID %in% c("19820")) %>%
  select(metro_name = NAME)

wc_rent <- st_join(rent, metros, join = st_within, 
                   left = FALSE) 

head(wc_rent)


ggplot(wc_rent, aes(x = estimate)) + 
  geom_histogram() + 
  facet_wrap(~metro_name)


library(viridis)


df <- wc_rent %>% filter(metro_name =='Detroit-Warren-Dearborn, MI')
  
p <- ggplot(df, aes(fill = estimate, color = estimate,text=NAME)) + 
  geom_sf() + 
  #coord_sf(crs = 26910) + 
  #facet_wrap(~metro_name, scales = "free", nrow = 1) + 
  theme_minimal() + 
  #theme(aspect.ratio = 1) + 
  scale_fill_viridis() + 
  scale_color_viridis()

p

ggp <- ggplotly(p)

ggp

#plotly::style(ggp, hoverinfo = "NAME")


df$hover <- with(df, paste('Precinct:',precinct, '<br>','Description:',charge_description, '<br>', "Time of Day:", incident_time, '<br>',"Day of Week:",day_of_week))

