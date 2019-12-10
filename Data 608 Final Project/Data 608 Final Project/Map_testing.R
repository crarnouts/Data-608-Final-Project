#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))


library("ggplot2")
theme_set(theme_bw())
library("sf")
library(rgeos)
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
                                                                      26.83)))


ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)




library("RSocrata")
library(tidyverse)

crime <- read.socrata(
  "https://data.oaklandnet.com/resource/ym6k-rx7a.json",
  app_token = "nolfN1RBD5OaLYn9x5ZlrodpY",
  email     = "arnoutc@ferris.edu",
  password  = "ElsieLuna55!"
)



crime <- crime %>% rename(lat = location_1.latitude, long = location_1.longitude )

crime$lat <- as.numeric(crime$lat)

crime$long <- as.numeric(crime$long)

## filter out some nulls baby

crime <- crime %>% filter(!is.na(lat)) %>% filter(!is.na(long))


## figure out the range I need

minlat <- min(crime$lat)
maxlat <- max(crime$lat)

minlong <- min(crime$long)
maxlong <- max(crime$long)

## Working but ugly

ggplot(data = world) +
  geom_sf() +
  geom_point(data = crime, aes(x = long, y = lat), size = 1, 
             shape = 5, fill = "darkred") +
  coord_sf(xlim = c(minlong, maxlong), ylim = c(minlat,maxlat), expand = FALSE)





#### Look at Crime Data Package
library(crimedata)

Detroit_df <- get_crime_data(years = 2015:2018, cities = "Detroit")

