# Make meeting attendance graph
#

library(ggplot2)
library(ggrepel)
library(viridis)
#source('leafletmap.R')

file <- "Full Annual Meetings Table The Ecological Society of Americas History and Records.csv"
citydat <- read.csv(file, stringsAsFactors = FALSE)

attendance <- citydat[which(citydat$Attendance > 0),]
city_colors <- viridis_pal()(length(unique(attendance$Location)))

attendplot <- ggplot(attendance, aes(Year, Attendance)) +
    geom_line() +
    geom_point(aes(color = Location), size = 5) +
    geom_text_repel(aes(label = Location, color = Location), size = 5) +
    #labels
    ggtitle("ESA Annual Meeting Attendance") +
    ylab("Number of Registered Attendees") +
    #aesthetics
    theme_bw(base_size = 20) +
    theme(
          axis.title.x = element_blank(),
          legend.position = "none") +
    scale_colour_manual(values = city_colors)


ggsave(file = "MeetingAttendance.svg", plot = attendplot, width=15, height=10)
