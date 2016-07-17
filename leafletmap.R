library(leaflet)
library(magrittr)
library(stringr)
library(XML)

file <- "~/Dropbox/website redesign/content/Maps_Graphs/Full Annual Meetings Table The Ecological Society of Americas History and Records.csv"

getDocNodeVal=function(doc, path)
{
    sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}

gGeoCode=function(str)
{
    u=paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
    doc = xmlTreeParse(u, useInternal=TRUE)
    str=gsub(' ','%20',str)
    lat=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lat")
    lng=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lng")
    c(lat,lng)
}

getlatlong <- function(locations) { #get lat and long for each location using gGeoCode
    dat <- cbind(lat=numeric(0), long=numeric(0))
    for (i in 1:length(locations)) {
        latlon <- gGeoCode(locations[i])[c(1:2)] #choose only the first location returned. sometimes gGeoCode returns an empty list instead of a vector of latitude and longitude. That breaks this loop.
        dat <- rbind(dat, latlon, deparse.level=0)
        Sys.sleep(.5) #returns null values without this
    }
    dat <- data.frame(location=locations, dat)
}

#read in and clean city data

citydat <- read.csv(file, stringsAsFactors=FALSE)
#cities <- str_replace(citydat$Location, ",", "") #strip commas from cities so search doesn't return a list

# get lat/long for cities and put in a dataframe
dat <- getlatlong(citydat$Location) #takes a few minutes

##fix classes
dat$location <- as.character(dat$location)
dat$lat <- as.numeric(unlist(dat$lat))
dat$long <- as.numeric(unlist(dat$long))

m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=dat$long, lat=-dat$lat, popup=citydat$Year)
m  # Print the map
