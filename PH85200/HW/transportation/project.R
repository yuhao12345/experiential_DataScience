library(ggplot2)
library(ggmap)
library(tidyverse)
library(chron)

df <- read.csv(file="Transportation_Network_Providers_-_Trips.csv",stringsAsFactors=FALSE, header=TRUE,colClasses=
                 c("NULL",NA,"NULL", NA, NA,"NULL","NULL","NULL","NULL",NA,NA,"NULL",NA,"NULL","NULL",NA,NA,"NULL",NA,NA,"NULL"),nrows=20000)
df$time<-strptime(as.character(df$Trip.Start.Timestamp),format="%m/%d/%Y %I:%M:%S %p")
df$time2 <- sapply(strsplit(as.character(df$time), " "), "[", 2)

df$time2<-times(df$time2)
breaks <- c(0, 6, 12, 18, 24) / 24 # times are internally fractions of a day
labels <- c("early_morning","morning","daylight", "evening")
df$ind <- cut(df$time2, breaks, labels, include.lowest = TRUE)

m<-get_stamenmap(bbox = c(left = -88, bottom = 41.6, right =
                            -87.3, top = 42.2), zoom = 10, maptype = c("terrain",
                                                                       "terrain-background", "terrain-labels", "terrain-lines", "toner",
                                                                       "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                                                                       "toner-labels", "toner-lines", "toner-lite", "watercolor"),
                 crop = TRUE, messaging = FALSE, urlonly = FALSE,
                 color = c("color", "bw"), force = FALSE)


gg<-ggmap(m, extent = "normal")

gg <- gg + stat_density2d(data=df, show.legend=F, 
                          aes(x=Pickup.Centroid.Longitude, y=Pickup.Centroid.Latitude, fill=..level.., alpha=..level..), geom="polygon", size=2, bins=10)

gg <- gg + facet_wrap(~ind)
gg

ggplot(df, aes(x=Trip.Seconds, y=Fare)) + geom_point()+ facet_wrap(~ind)