library(leaflet)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(scales)
# Function to Camel Case the passed character object
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
# legendTitle <- "State-level Assaults<br />per 100 Officers"
stateAbbrNbrs <- stateAbbrNbrs[order(stateAbbrNbrs$StateAbbr), ]
# using a for loop because i still don't know the apply functions
for(i in 1:length(stateAbbrNbrs$StateName)) {
  stateAbbrNbrs[i, "StateName"] <- tolower(stateAbbrNbrs[i, "StateName"])
  stateAbbrNbrs[i, "StateName"] <- .simpleCap(stateAbbrNbrs[i, "StateName"])
}
usshapefile <- "cb_2015_us_state_5m.shp"
# Read in the shapefile for US states:
usgeo <- read_shape(file=usshapefile)
usgeo <- usgeo[order(usgeo@data$STUSPS), ]
# There are extra "states" / FIPS codes in usgeo
identical(factor(stateAbbrNbrs$StateAbbr), usgeo@data$STUSPS)
# Do a quick plot of the shapefile and check its structure:
qtm(usgeo)
# Merge data with tmaptools' append_data function
usmap <- append_data(usgeo, stateAbbrNbrs,
                     key.shp = "STUSPS", key.data="StateAbbr")
midwestFIPS <- c("17", "18", "19", "20", "26", "27", "29", "31", "38", "39",
                 "46", "55")
northeastFIPS <- c("09", "23", "25", "33", "34", "36", "42", "44", "50")
southFIPS <- c("01", "05", "10", "11", "12", "13", "21", "22", "24", "28",
               "37", "40", "45", "47", "48", "51", "54")
westFIPS <- c("02", "04", "06", "08", "15", "16", "30", "32", "35", "41",
              "49", "53", "56")
contWestFIPS <- c("04", "06", "08", "16", "30", "32", "35", "41",
                  "49", "53", "56")
contUSFIPS <- c(midwestFIPS, northeastFIPS, southFIPS, contWestFIPS)
wholeUSFIPS <- c(midwestFIPS, northeastFIPS, southFIPS, westFIPS)
# RGB values of Greens 3:8 on with 8 colors
# 199	233	192 #C7E9C0
# 161	217	155 #A1D99B
# 116	196	118 #74C476
# 65	171	93  #41AB5D
# 35	139	69  #238B45
# 0	90	50    #005A32
# RGB values of Blues 3:8 palette with 8 colors
# 198	219	239 #C6DBEF
# 158	202	225 #9ECAE1
# 107	174	214 #6BAED6
#  66	146	198 #4292C6
#  33	113	181 #2171B5
#   8	69	148 #084594
# RGB values of Oranges 2:7 palette with 8 colors
# 254	230	206 #FEE6CE
# 253	208	162 #FDD0A2
# 253	174	107 #FDAE6B
# 253	141	60  #FD8D3C
# 241	105	19  #F16913
# 217	72	1   #D94801
# Hex values of Purples 2:7 palette with 8 colors
#             #efedf5
#             #dadaeb
#             #bcbddc
#             #9e9ac8
#             #807dba
#             #6a51a3
pal <- colorNumeric(palette = "Blues", domain = usmap@data$officerAssault100)
palGreens <- colorNumeric(palette = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D",
                                      "#238B45", "#005A32"),
                          domain = usmap@data$officerAssault100)
palBlues <- colorNumeric(palette = c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6",
                                     "#2171B5", "#084594"),
                         domain = usmap@data$percPop)
palOranges <- colorNumeric(palette = c("#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C",
                                       "#F16913", "#D94801"),
                           domain = usmap@data$percPop)
palPurples <- colorNumeric(palette = c("#efedf5", "#dadaeb", "#bcbddc", "#9e9ac8",
                                       "#807dba", "#6a51a3"),
                           domain = usmap@data$percPop)
continentalUSgeo <- usmap[usmap@data$STATEFP %in% contUSFIPS, ]
qtm(continentalUSgeo)
dev.off()
map <- leaflet(continentalUSgeo)
contUSMap <- map %>% addPolygons(stroke = TRUE, smoothFactor = 0.2,
                                 fillOpacity = 0.8,
                                 color= ~palBlues(continentalUSgeo@data$percPop))
# Below is a ugly map - probably don't use
contUSMap
# Subset just the midwest data from the US shapefile
#midwestgeo <- usgeo[usgeo@data$STATEFP %in% midwestFIPS, ]
midwestgeo <- usmap[usmap@data$STATEFP %in% midwestFIPS, ]
pdf("P32 midwest bw map.pdf", width = 11, height = 8.5)
qtm(midwestgeo)
dev.off()
map <- leaflet(midwestgeo)
midwestMap <- map %>% addPolygons(stroke = TRUE, smoothFactor = 0.2,
                                  fillOpacity = 0.8, color = ~palOranges(midwestgeo@data$percPop))
midwestMap
northeastgeo <- usmap[usmap@data$STATEFP %in% northeastFIPS, ]
pdf("P32 northeast bw map.pdf", width = 11, height = 8.5)
qtm(northeastgeo)
dev.off()
map <- leaflet(northeastgeo)
northeastMap <- map %>% addPolygons(stroke = TRUE, smoothFactor = 0.2,
                                    fillOpacity = 0.8, color = ~palPurples(northeastgeo@data$percPop))
northeastMap
southmap <- usmap[usmap@data$STATEFP %in% southFIPS, ]
pdf("P32 south bw map.pdf", width = 11, height = 8.5)
qtm(southmap)
dev.off()
southpopup <- paste0("<b>State: ", southmap@data$NAME, "</b><br />Assaults: ",
                  southmap@data$OfficersAssaulted,
                  "</b><br />Assaults Per 100 Officers: ",
                  southmap@data$officerAssault100)
# Now the interactive map:
leaflet(southmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = .3, 
              popup=southpopup, 
              color= ~palBlues(southmap@data$percPop)
  %>% fitBounds(-125, 30, -102, 49)
  )
westmap <- usmap[usmap@data$STATEFP %in% westFIPS, ]
westpopup <- paste0("<b>State: ", westmap@data$NAME, "</b><br />Assaults: ",
                     westmap@data$OfficersAssaulted,
                     "</b><br />Assaults per 100 Officers: ",
                     westmap@data$officerAssault100,
                     "</b><br />Region: ",
                     westmap@data$Region,
                     "</b><br />Assaults per 100 Officers: ",
                     westmap@data$RegionAssaultsPer100)
# Now the interactive map:
leaflet(westmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = .3, 
              popup=westpopup, 
              color= ~palGreens(westmap@data$officerAssault100))
# Test - interactive US map - is PR, VI, GU on the map?
us1map <- usmap[usmap@data$STATEFP %in% westFIPS |
                usmap@data$STATEFP %in% southFIPS |
                usmap@data$STATEFP %in% northeastFIPS |
                usmap@data$STATEFP %in% midwestFIPS, ]
uspopup <- paste0("<b>State: ", us1map@data$NAME, "</b><br />Assaults: ",
                    us1map@data$OfficersAssaulted,
                    "</b><br />Assaults Per 100 Officers: ",
                    us1map@data$officerAssault100)
leaflet(us1map) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = .3, 
              popup=uspopup, 
              color= ~palBlues(us1map@data$percPop)
  )
usregionshapefile <- "cb_2015_us_region_5m.shp"
# Read in the shapefile for US regions:
usregiongeo <- read_shape(file=usregionshapefile)
# Do a quick plot of the shapefile and check its structure:
pdf("P32 region bw map.pdf", width = 11, height = 8.5)
qtm(usregiongeo)
dev.off()
cityColClasses <- c(rep("character", 2), rep("integer", 2), rep("numeric", 2),
                    rep("character", 2))
top10Cities <- read.csv("P3 top10 cities.csv", stringsAsFactors = FALSE,
                        colClasses = cityColClasses)
for(i in 1:length(top10Cities$v14)) {
  top10Cities[i, "v14"] <- tolower(top10Cities[i, "v14"])
  top10Cities[i, "v14"] <- .simpleCap(top10Cities[i, "v14"])
}
top10Cities$AssaultsPer100 <- round((top10Cities$TotalAssaults /
                              (top10Cities$NbrOfficers / 100)), 1)
# This works for the map as a whole. otherwise, i think i would need 4 
# dataframes and individual popups to spread across my 4 individual maps.
# can't disable zoom on this one.
leaflet(us1map) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2, 
              fillOpacity = .3, 
              popup=uspopup, 
              color= ~palBlues(us1map@data$percPop)) %>%
  addCircleMarkers(lng = ~top10Cities$Lon, lat = ~top10Cities$Lat,
                   radius = ~top10Cities$AssaultsPer100,
                   color = ~ifelse(top10Cities$AssaultsPer100 > 17.5, "red", "orange"),
                   stroke = TRUE,
                   fillOpacity = 0.2,
                   popup = ~as.character(top10Cities$v14)
  )