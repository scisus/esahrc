# Make meeting attendance graph
#

library(ggplot2)
library(ggrepel)
library(viridis)
library(ggmap)
#source('leafletmap.R')

# functions



# data preparation

file <- "Full Annual Meetings Table – The Ecological Society of Americas History and Records.csv"
citydat <- read.csv(file, stringsAsFactors = FALSE)

cities <- unique(citydat$Location)

#get latitude and longitude for cities. Thanks to google!
lonlat <- data.frame(lon = numeric(0), lat = numeric(0))
for (i in 1:length(cities)) {
    if (cities[i] == "University of Maryland") {
        lonlat <- rbind(lonlat, geocode("College Park, MD"))
    }  else {
        lonlat <- rbind(lonlat, geocode(cities[i]))
    }
}
lonlat <- cbind(Location = cities, lonlat) # get lat lon for each city

citydat <- merge(citydat, lonlat[!is.na(lonlat$lon),]) # add lat lon to meetings df
write.csv(citydat, "ESAMeetingsLatLon.csv", row.names = FALSE)

citydat$Location = factor(citydat$Location, levels=unique(citydat$Location[order(citydat$lon, citydat$lat)]), ordered=TRUE) # order cities by latitude and longitude

# create attendance graph

attendance <- citydat[which(citydat$Attendance > 0),]
city_colors <- viridis_pal(direction=-1)(length(unique(attendance$Location)))

attendplot <- ggplot(attendance, aes(Year, Attendance)) +
    geom_line() +
    geom_point(aes(color = Location), size = 10) +
    geom_text_repel(aes(label = Location, color = Location), size = 15) +
    #labels
    ggtitle("ESA Annual Meeting Attendance") +
    ylab("Number of Registered Attendees") +
    #aesthetics
    theme_bw(base_size = 40) +
    theme(axis.title.x = element_blank(), legend.position = "none") +
    scale_colour_manual(values = city_colors)


attendplot
ggsave(file = "MeetingAttendance.png", plot = attendplot, width=30, height=20)
