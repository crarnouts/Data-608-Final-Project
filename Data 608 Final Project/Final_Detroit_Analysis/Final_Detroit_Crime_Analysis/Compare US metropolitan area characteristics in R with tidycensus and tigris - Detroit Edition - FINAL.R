library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(mapvie)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key("0af36703082a8e7234a5d0e141e673d556edfe3b")

detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties

detroit_rms_crime2$block_group_id <- substring(detroit_rms_crime2$block_id, 1, 12)

df <- detroit_rms_crime2 %>% filter(year > 2017)


Wayne_county <- get_acs(geography = "block group", variables = "B01001_002", 
                                            state = "MI", county = "Wayne", geometry = TRUE)


p <- ggplot(Wayne_county, aes(fill = estimate, color = estimate,text=NAME)) + 
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
#######
#### Try to bring in the appropriate map data hopefully #####
library(mapview)

mapview(Wayne_county, zcol = "estimate", legend = TRUE)

#### Try to bring in the appropriate map data hopefully #####


## Load in the Data Dictionaries
my.vars.acs = load_variables(2017, "acs5", cache = TRUE)
head(my.vars.acs)

my.vars.acs$label <- str_replace_all(my.vars.acs$label,"!!"," ")

write.csv(my.vars.acs,file= "C:/Users/arnou/Documents/Data 608 Final Project/Data 608 Final Project/acs5_variables.csv")
## Load in the Data Dictionaries
my.vars.acs = load_variables(2017,"acs5/profile", cache = TRUE)
head(my.vars.acs)

my.vars.acs$label <- str_replace_all(my.vars.acs$label,"!!"," ")

write.csv(my.vars.acs,file= "C:/Users/arnou/Documents/Data 608 Final Project/Data 608 Final Project/acs5_profile_variables.csv")
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

