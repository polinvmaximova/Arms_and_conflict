library(tidyverse)
library(rgdal)
library(maps)
library(maptools)
library(tmap)
library(leaflet)
library(plotly)
library(cartography)
library(sp)
library(mapview)

load("~/conflict.trends/ucdp_ged.RData")

# Interactive map of conflict sites in Africa (1991-2010)

ucdp_ged <- ucdp_ged %>% filter(type_of_violence=="1", year>=1991, best>=25, region=="Africa")

mytext <- paste(
  "Country: ", ucdp_ged$country, "<br/>",
  "Province: ", ucdp_ged$where_coordinates, "<br/>",
  "Battle Deaths: ", ucdp_ged$best, sep="") %>%
  lapply(htmltools::HTML)

interactive_map <- leaflet(ucdp_ged) %>%
  addTiles() %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(~longitude, ~latitude,
    fillColor=~circles_palette2(best), color="white", radius=4, stroke=FALSE, fillOpacity=0.7,
    label=mytext,
    labelOptions=labelOptions(style=list("font-family"="Times New Roman", "font-weight" = "normal", padding="3px 8px"), textsize="13px", direction="auto"))
interactive_map

mapshot(interactive_map, url="interactive_map.html")
