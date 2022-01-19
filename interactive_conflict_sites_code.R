library(tidyverse)
library(leaflet)
library(mapview)
library(viridis)

load("~/Downloads/Arms_and_conflict-main/conflict_sites.RData")

# Interactive map of conflict sites in Africa (1991-2020)
## It displays all conflict locations with at least 25 battle deaths and their proximity to a capital city: the darker the closer to a state centre.
### Conflict_sites is an adjusted and complemented dataframe based on the UCDP Georeferenced Dataset (GED) Global Version 21.1. (https://ucdp.uu.se/downloads/index.html#ged_global)

mytext <- paste(
  "Country: ", conflict_sites$country, "<br/>",
  "Province: ", conflict_sites$location_name, "<br/>",
  "Year: ", conflict_sites$year, "<br/>",
  "Rebel forces: ", conflict_sites$side_b, "<br/>",
  "Distance from capital (km) : ", "app. ", conflict_sites$distance,"<br/>",
  "Battle Deaths: ", conflict_sites$best, sep="") %>%
  lapply(htmltools::HTML)

pal <- colorNumeric(
  palette = "inferno",
  domain = conflict_sites$distance
)

interactive_map <- leaflet(conflict_sites) %>%
  addTiles() %>%
  addProviderTiles("Stamen.TonerLite") %>%
  addCircleMarkers(~conflict_long, ~conflict_lat,
                   color=~pal(distance), radius=5, stroke=FALSE, fillOpacity=0.7,
                   label=mytext,
                   labelOptions=labelOptions(style=list("font-family"="Times New Roman",
                                                        "font-weight" = "normal", padding="3px 8px"),
                                             textsize="13px", direction="auto")) %>%
  addLegend("bottomright", pal = pal, title="Proximity of Armed Clashes to a Capital City (km)", values = conflict_sites$distance)
interactive_map

mapshot(interactive_map, url="interactive_conflict_sites.html")
