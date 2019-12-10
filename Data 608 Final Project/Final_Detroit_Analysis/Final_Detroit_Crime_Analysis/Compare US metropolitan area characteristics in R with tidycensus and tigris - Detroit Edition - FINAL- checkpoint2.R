library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(mapview)
library(dplyr)
library(jsonlite)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key("0af36703082a8e7234a5d0e141e673d556edfe3b")

#################################### Load in the Data Dictionaries #################################################
#################################### Load in the Data Dictionaries #################################################
my.vars.acs = load_variables(2017, "acs5", cache = TRUE)
head(my.vars.acs)

my.vars.acs$label <- str_replace_all(my.vars.acs$label,"!!","_")
my.vars.acs$concept <- str_replace_all(my.vars.acs$concept," ","_")

#write.csv(my.vars.acs,file= "C:/Users/arnou/Documents/Data 608 Final Project/Data 608 Final Project/acs5_variables.csv")
## Load in the Data Dictionaries
my.vars.acsprofile = load_variables(2017,"acs5/profile", cache = TRUE)
head(my.vars.acs)

my.vars.acsprofile$label <- str_replace_all(my.vars.acsprofile$label,"!!","_")


#write.csv(my.vars.acs,file= "C:/Users/arnou/Documents/Data 608 Final Project/Data 608 Final Project/acs5_profile_variables.csv")
## Load in the Data Dictionaries
#################################### Load in the Data Dictionaries #################################################
#################################### Load in the Data Dictionaries #################################################


detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties

detroit_rms_crime2$GEOID <- substring(detroit_rms_crime2$block_id, 1, 12)

df <- detroit_rms_crime2 %>% filter(year > 2017)

crimes_by_GEOID <- df %>% group_by(GEOID,offense_category) %>% tally()

vars <- c("B01001_007","B01002_001","B00001_001")


Wayne_county <- get_acs(geography = "block group", variables = vars , 
                                            state = "MI", county = "Wayne", geometry = TRUE)


Macomb_county <- get_acs(geography = "block group", variables = vars, 
                        state = "MI", county = "Macomb", geometry = TRUE)


Oakland_county <- get_acs(geography = "block group", variables = vars, 
                        state = "MI", county = "Oakland", geometry = TRUE)

counties_df <- rbind(Oakland_county,Wayne_county,Macomb_county)

mergedata <- df %>% distinct(GEOID)

counties_df <- as.data.frame(counties_df)

crime_with_geo <- counties_df %>% distinct(GEOID,NAME,geometry) %>% inner_join(crimes_by_GEOID)
crime_with_geo <- crime_with_geo %>% distinct(GEOID,NAME,offense_category,n,geometry)

#counties_df$CRIME_PER_PERSON <- (counties_df$n/counties_df$estimate)/2

counties_df$moe <- NULL

counties_df$variable <- case_when(
  counties_df$variable == "B01001_007" ~ "18 and 19 Year Old Males",
  counties_df$variable == "B01002_001" ~ "Median Age",
  counties_df$variable == "B00001_001" ~ "Population",
)

crime_with_geo <- crime_with_geo %>% rename(
  variable = offense_category,
  estimate = n,
)


mapview_df <- rbind(crime_with_geo,counties_df)

mapview_df <- mapview_df %>% drop_na(estimate)

library(mapview)

mapview(counties_df, zcol = "estimate", legend = TRUE)

counties_df %>% filter(offense_category=="SEX OFFENSES") %>% filter(estimate>0) %>% mapview(zcol = "CRIME_PER_PERSON", legend = TRUE)
counties_df %>% filter(offense_category=="SEX OFFENSES") %>% mapview(zcol = "n", legend = TRUE)

#### Try to bring in the appropriate map data hopefully #####




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

