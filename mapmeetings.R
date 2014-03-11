# Make a map of all cities ESA has been held at
## This code has an MIT license
## functions getDocNodeVale and gGeoCode are from https://github.com/ezgraphs/R-Programs/blob/master/GoogleGeocodeMap.R

library(ggmap)
library(mapproj)
library(XML)
library(stringr)

file <- "~/Dropbox/website redesign/content/Maps_Graphs/Full Annual Meetings Table  The History of ESA from the Historical Records Committee.csv"

getDocNodeVal=function(doc, path)
{
    sapply(getNodeSet(doc, path), function(el) xmlValue(el))
}

gGeoCode=function(str)
{
    library(XML)
    u=paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
    doc = xmlTreeParse(u, useInternal=TRUE)
    str=gsub(' ','%20',str)
    lat=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lat")
    lng=getDocNodeVal(doc, "/GeocodeResponse/result/geometry/location/lng")
    c(lat,lng)
}


#read in and clean city cata

citydat <- read.csv(file, stringsAsFactors=FALSE)
cities <- str_replace(cities, ",", "") #strip commas from cities so search doesn't return a list

getlatlong <- function(locations) { #get lat and long for each location using gGeoCode
    dat <- cbind(lat=numeric(0), long=numeric(0))
    for (i in 1:length(locations)) {
        latlon <- gGeoCode(locations[i])[c(1:2)] #choose only the first location returned. sometimes gGeoCode returns an empty list instead of a vector of latitude and longitude. That breaks this loop.
        dat <- rbind(dat, latlon, deparse.level=0) 
        Sys.sleep(.5) #returns null values without this
    }
dat <- data.frame(location=locations, dat) 
}

dat <- getlatlong(cities) 

##fix classes
dat$location <- as.character(dat$location)
dat$lat <- as.numeric(levels(dat$lat))[dat$lat]
dat$long <- as.numeric(levels(dat$long))[dat$long]

# map all the cities!
map <- get_map(location = "Iowa", zoom = 3, maptype = "roadmap") #make a basemap centered on kansas
p <- ggmap(map)
p <- p + labs(title = "ESA Meetings")
p <- p + geom_point(data = dat, aes(x = long, y = lat), size = 5)
#p <- p + geom_text(data = dat, aes(x = long, y = lat, label = location), hjust = -.2)

print(p)
