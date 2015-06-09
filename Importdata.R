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
require(ggplot2); require(plyr); require(XML); require(ggmap)

#custom functions====
#strip <shit>
remove.html <- function(x) gsub("<.*?>", "", x)
#strip whitespace
strip.white <- function(x) gsub("\\s+$|^\\s+","",x)

# eh====
setwd("C:/Github/StravaRecommender")
running.tcx.files <- list.files(file.path(getwd(), "data/running"), pattern = ".TCX", recursive = T)

GR <- readLines(file.path(getwd(), "data/running",running.tcx.files[1]))

track.point.start <- which(as.character(gregexpr("<Trackpoint>", GR) ) != "-1")
track.point.end <- which(as.character(gregexpr("</Trackpoint>", GR) ) != "-1")
tp <- list()

for (i in 1:length(track.point.start)) {
  tp[[i]] <- GR[track.point.start[i]:track.point.end[i]] 
}

