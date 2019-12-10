library(plotly)
library(dplyr)
library(jsonlite)

# airport locations
air <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')

# flights between airports
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
flights$id <- seq_len(nrow(flights))

p <- plot_mapbox(mode = 'scattermapbox') %>%
  add_markers(
    data = air, x = ~long, y = ~lat, text=~airport, color=I("red"),
    size = ~cnt, hoverinfo = "text", alpha = 0.5) %>%
  layout(
    plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
    mapbox = list(style = 'dark',
                  zoom = 1.5,
                  center = list(lat = median(air$lat),
                                lon = median(air$long))),
    margin = list(l = 0, r = 0,
                  b = 0, t = 0,
                  pad = 0),
    showlegend=FALSE)

p

p <- plot_mapbox(mode = 'scattermapbox') %>%
  add_markers(
    data = air, x = ~long, y = ~lat, text=~airport, color=I("red"),
    size = ~cnt, hoverinfo = "text", alpha = 0.5) %>%
  add_segments(
    data = group_by(flights, id),
    x = ~start_lon, xend = ~end_lon,
    y = ~start_lat, yend = ~end_lat,
    alpha = 0.3, size = I(1), hoverinfo = "none",
    color=I("red")) %>%
  layout(
    plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
    mapbox = list(style = 'dark',
                  zoom = 1.5,
                  center = list(lat = median(air$lat),
                                lon = median(air$long))),
    margin = list(l = 0, r = 0,
                  b = 0, t = 0,
                  pad = 0),
    showlegend=FALSE)

p



detroit_rms_crime <- fromJSON("https://opendata.arcgis.com/datasets/0825badfe6304620a998d162be0e135e_0.geojson")
detroit_rms_crime2 <- detroit_rms_crime$features$properties
detroit_rms_crime2$Date <- as.POSIXct(detroit_rms_crime2$incident_timestamp)
detroit_rms_crime2$Time <- as.POSIXct(detroit_rms_crime2$incident_time)

df <- detroit_rms_crime2 %>% filter(detroit_rms_crime2$offense_category =="HOMICIDE")
df$hover <- with(df, paste('Offense','<br>',offense_category, '<br>', "Hour of Day", hour_of_day))

p <- plot_mapbox(mode = 'scattermapbox') %>%
  add_markers(
    data = df, x = ~longitude, y = ~latitude, text=~hover, color=~hour_of_day, size=3, hoverinfo = "text", alpha = 0.5) %>%
  layout(
    plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
    mapbox = list(style = 'dark',
                  zoom = 12,
                  center = list(lat = median(df$latitude),
                                lon = median(df$longitude))),
    margin = list(l = 0, r = 0,
                  b = 0, t = 0,
                  pad = 0),
    showlegend=FALSE)

p

### Take a look at Offense Category

p <- plot_mapbox(mode = 'scattermapbox') %>%
  add_markers(
    data = df, x = ~longitude, y = ~latitude, text=~hover, color=~offense_category, hoverinfo = "text", alpha = 0.5) %>%
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


