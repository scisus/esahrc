library(leaflet)
library(magrittr)
library(ggmap)



citydat <- read.csv("ESAMeetingsLatLon.csv")


m <- leaflet(width = "100%") %>%
    addProviderTiles("Stamen.TonerLite") %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(lng=citydat$lon, lat=citydat$lat,
                     radius = 3,
                     color = " #019390",
                     popup=paste(citydat$Location, citydat$Year, sep = "<br />"))
m  # Print the map
