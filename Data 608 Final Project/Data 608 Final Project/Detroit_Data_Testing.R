
## Just trying to bring in as many data sources about Detroit as possible and then figure out what to do with them later !!


library(jsonlite)

### https://datadrivendetroit.org/blog/2018/03/20/open-data-portals/ ###


## https://data.detroitmi.gov/datasets/building-permits-current/geoservice?selectedAttribute=estimated_cost ##
detroit_building_permits <- fromJSON("https://opendata.arcgis.com/datasets/ef79f182b3c549bbaaf8c9a7b3b44421_0.geojson")

permits <- detroit_building_permits$features

building <- data$properties
geometry <- data$geometry

detroit_building_permits <- cbind(building,geometry)
## this still needs to have the lat long extracted from the string

#### Look at Crime Data Package
library(crimedata)

Detroit_df <- get_crime_data(years = 2015:2018, cities = "Detroit")


## Housing and Urban Development Data 
### http://hudgis-hud.opendata.arcgis.com/datasets/acs-5yr-socioeconomic-estimate-data-by-tract?geometry=25.804%2C-0.614%2C-25.173%2C76.538 ##

## Grab all of the HUD data and then filter it down to michigan, the census tract is as granular as it gets

HUD <- fromJSON("https://opendata.arcgis.com/datasets/07e803b771c14476b3e0df319c48ad73_0.geojson")
HUD_data <- HUD$features$properties %>% filter(STUSAB =="MI")


## liquor would be interesting but we would have to geocode the addresses
liquor <- fromJSON("https://opendata.arcgis.com/datasets/9b71ecd76f68400fb7777c438f33a927_0/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")

liquor$fields
liquor$spatialReference
test <- liquor$features
test2 <- test$attributes

### This is the production version
detroit_rms_crime <- fromJSON("https://gis.detroitmi.gov/arcgis/rest/services/OpenData/RMSCrimeIncidents/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json")
detroit_rms_crime2 <- detroit_rms_crime$features

detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties
detroit_rms_crime2$incident_timestamp2 <- as.POSIXct(detroit_rms_crime2$incident_timestamp)


## HUD Location Affordability Index ##
location_afforability <- fromJSON("https://opendata.arcgis.com/datasets/b7ffe3607e8c4212bf7cf2428208dbb6_0.geojson")
location_afforability_data <- location_afforability$features$properties %>% filter(STUSAB =="MI")


#### MAPPING DETROIT CRIMES #####

library("ggplot2")
theme_set(theme_bw())
library("sf")
library(rgeos)
library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

world <- as.data.frame(world)

## figure out the range I need

minlat <- min(detroit_rms_crime2$latitude)
maxlat <- max(detroit_rms_crime2$latitude)

minlong <- min(detroit_rms_crime2$longitude)
maxlong <- max(detroit_rms_crime2$longitude)

detroit_rms_crime2 <- fortify(detroit_rms_crime2)

## Working but ugly

ggplot(data = world) +
  #geom_sf() +
  geom_point(data = detroit_rms_crime2, aes(x = longitude, y = latitude), size = 1, 
             shape = 5, fill = "darkred") +
  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat,maxlat), expand = FALSE)





#######################################



