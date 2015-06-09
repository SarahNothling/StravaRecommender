#'Work in progress
#'
#' ek gebruik die file net om die structure van die data te verstaan :P
#'
#'


# installs and loads required packages====
packages <- c(
  "ggplot2",
  "plyr",
  "XML"
  "ggmap"
  )
for (i in 1:length(packages)) {
  if !(packages[i] %in% installed.packages()) install.packages(packages[i]) 
}
require(ggplot2); require(plyr); require(XML); require(ggmap);

#custom functions====
#strip <shit>
remove.html <- function(x) gsub("<.*?>", "", x)
#strip whitespace
strip.white <- function(x) gsub("\\s+$|^\\s+","",x)
#multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# eh====
setwd("C:/Github/StravaRecommender")
running.tcx.files <- list.files(file.path(getwd(), "data/running"), pattern = ".TCX", recursive = T)

GR <- readLines(file.path(getwd(), "data/running",running.tcx.files[1]))

track.point.start <- which(as.character(gregexpr("<Trackpoint>", GR) ) != "-1")
track.point.end <- which(as.character(gregexpr("</Trackpoint>", GR) ) != "-1")
tp <- list()

for (i in 1:length(track.point.start)) {
  tp[[i]] <- GR[track.point.start[i]:track.point.end[i]] 
  print(c(floor(i/100)*100,length(track.point.start)))
}
event <- data.frame()
for (i in 1:length(tp)) {
event[i,1] <- if(length(tp[[i]][which(as.character(gregexpr("<Time>",tp[[i]])) != -1)]) == 0) NA else 
  remove.html(strip.white(tp[[i]][which(as.character(gregexpr("<Time>",tp[[i]])) != -1)]))
event[i,2] <- if(length(tp[[i]][which(as.character(gregexpr("<LatitudeDegrees>",tp[[i]])) != -1)]) == 0) NA else 
  remove.html(strip.white(tp[[i]][which(as.character(gregexpr("<LatitudeDegrees>",tp[[i]])) != -1)]))
event[i,3] <- if(length(tp[[i]][which(as.character(gregexpr("<LongitudeDegrees>",tp[[i]])) != -1)]) == 0) NA else 
  remove.html(strip.white(tp[[i]][which(as.character(gregexpr("<LongitudeDegrees>",tp[[i]])) != -1)]))
event[i,4] <- if(length(tp[[i]][which(as.character(gregexpr("<AltitudeMeters>",tp[[i]])) != -1)]) == 0) NA else 
  remove.html(strip.white(tp[[i]][which(as.character(gregexpr("<AltitudeMeters>",tp[[i]])) != -1)]))
event[i,5] <- if(length(tp[[i]][which(as.character(gregexpr("<DistanceMeters>",tp[[i]])) != -1)]) == 0) NA else 
  remove.html(strip.white(tp[[i]][which(as.character(gregexpr("<DistanceMeters>",tp[[i]])) != -1)]))
print(c(floor(i/100)*100,length(tp)))
}

colnames(event) <- c("Time", "Lat", "Lon", "Alt", "Dist")

event$Lat <- as.numeric(event$Lat)
event$Lon <- as.numeric(event$Lon)
event$Alt <- as.numeric(event$Alt)
event$Dist <- as.numeric(event$Dist)
event$Time <- strptime(event$Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "GMT")


a <- ggplot(event) + geom_line(aes(x = Time, y = Alt, col = Alt))

mean.lon <- mean(range(event$Lon[!is.na(event$Lon)]))
mean.lat <- mean(range(event$Lat[!is.na(event$Lat)]))

event.map <- get_googlemap(center = c(lon = mean.lon, lat = mean.lat), zoom = 10)
b <- ggmap(event.map) + geom_line(data = event, aes(x = Lon, y = Lat, col = delta.alt), lwd = 2)

c <- ggplot(event) + geom_line(aes(x = Time, y = delta.alt, col = delta.alt))

multiplot(a,c)

for (i in 1:(nrow(event)-1)) {
  event$delta.alt[i] <- event$Alt[i+1]-event$Alt[i]
 
}
event$delta.alt[nrow(event)] <- event$delta.alt[nrow(event)-1]
e









as.POSIXlt.numeric(event$Time)






