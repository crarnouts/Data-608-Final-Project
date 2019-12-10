library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
census_api_key("0af36703082a8e7234a5d0e141e673d556edfe3b")


rent <- get_acs(geography = "tract", variables = "DP04_0134", 
                state = c("MI"), geometry = TRUE)

rent_block <- get_acs(geography = "block group", variables = "DP04_0134", 
                state = "MI", geometry = TRUE)

Lapeer_county <- get_acs(geography = "block group", variables = "B01003_001", 
                   state = "MI", county = "Lapeer", geometry = TRUE)


vars10 <- c("P005003", "P005004", "P005006", "P004003")

il <- get_decennial(geography = "block", variables = vars10, year = 2010,
                    summary_var = "P001001", state = "MI", geometry = TRUE)

age <- get_acs(geography = "tract", "B01001_008", 
                state = c("MI"), geometry = TRUE)

head(rent)


p <- ggplot(rent, aes(fill = estimate, color = estimate,text=NAME)) + 
  geom_sf() + 
  #coord_sf(crs = 26910) + 
  #facet_wrap(~metro_name, scales = "free", nrow = 1) + 
  theme_minimal() + 
  #theme(aspect.ratio = 1) + 
  scale_fill_viridis() + 
  scale_color_viridis()

p

### this part doesn't work for some reason
ggp <- ggplotly(p)

ggp
#######

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

