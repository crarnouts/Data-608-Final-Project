library(plotly)

blank_layer <- list(
  title = "",
  showgrid = F,
  showticklabels = F,
  zeroline = F)

p <- map_data("county") %>%
  filter(region == 'michigan') %>%
  group_by(group) %>%
  plot_ly(
    x = ~long,
    y = ~lat,
    fillcolor = 'white',
    hoverinfo = "none") %>%
  add_polygons(
    line = list(color = 'black', width = 0.5)) %>%
  layout(
    xaxis = blank_layer,
    yaxis = blank_layer)

p

