# install.packages("mapproj")
# install.packages("ggmap")
# install.packages("DeducerSpatial")

library(mapproj)
library(ggmap)
library(DeducerSpatial)

require(maps)
require(ggmap)

par(mfrow = c(2, 1))
map("usa")

map("county")

map("world", "China")
map.cities(country = "China", capitals = 2)

map("state", "GEORGIA")
data(us.cities)
map.cities(us.cities, country = "GA")

############################################# Plot unemployment in each county ######################################3

data(unemp)
data(county.fips)

# Plot unemployment by country
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", 
           "#980043")
head(unemp)

head(county.fips)


unemp$colorBuckets <- as.numeric(cut(unemp$unemp, c(0, 2, 4, 6, 8, 
                                                    10, 100)))
colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]

map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0, 
    lty = 0, projection = "polyconic")



# Add border around each State
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1, lwd = 0.2, 
    projection = "polyconic")
title("unemployment by county, 2009")

leg.txt <- c("<2%", "2-4%", "4-6%", "6-8%", "8-10%", ">10%")
legend("topright", leg.txt, horiz = TRUE, fill = colors)



################################## USING OPEN STREET MAPS #############################################################
## These might be the best options for some maps ##


library(UScensus2000tract)
library(DeducerSpatial)

lat <- c(43.834526782236814,30.334953881988564)
lon <- c(-131.0888671875  ,-107.8857421875)
southwest <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),5,'bing')
data(california.tract)
california.tract <- spTransform(california.tract,osm())

plot(southwest,removeMargin=TRUE)
#choro_plot(california.tract,dem = slot(california.tract,"data")[,'med.age'], legend.title = 'Median Age',alpha=1)


################# MAPPING HOUSTON CRIMES ##########################
HoustonMap <- qmap('houston', zoom = 14,color = 'bw', legend = 'topleft')





######################## TRYING SOMETHING ELSE ###############################################
#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")

#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)


#the first five features
#building the query
q <- getbb("Madrid")%>%
  opq()%>%
  add_osm_feature("amenity", "cinema")

str(q) #query structure


cinema <- osmdata_sf(q)
cinema


#our background map
mad_map <- get_map(getbb("Madrid"),maptype = "toner-background")


#final map
p <- ggmap(mad_map)+
  geom_sf(data=cinema$osm_points,
          inherit.aes =FALSE,
          colour="#238443",
          fill="#004529",
          alpha=.5,
          size=4,
          shape=21)+
  labs(x="",y="")


library(plotly)

ggplotly(p)
