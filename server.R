  # This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(leaflet)
library(shiny)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(scales)
legendTitle <- "Assaults per 100 Officers"
stateColClasses <- c(rep("character", 7), rep("integer", 7),  rep("numeric", 2))
stateAbbrNbrs <- read.csv("P3 state abbr.csv", stringsAsFactors = FALSE,
                          colClasses = stateColClasses)
stateAbbrNbrs$officers100 <- stateAbbrNbrs$NbrOfOfficers / 100
stateAbbrNbrs$officerAssault100 <- round((stateAbbrNbrs$OfficersAssaulted /
                                         stateAbbrNbrs$officers100), 1)
usshapefile <- "cb_2015_us_state_5m.shp"
# Read in the shapefile for US states:
usgeo <- read_shape(file=usshapefile)
usgeo <- usgeo[order(usgeo@data$STUSPS), ]
stateAbbrNbrs <- stateAbbrNbrs[order(stateAbbrNbrs$StateAbbr), ]
# There are extra "states" / FIPS codes in usgeo from the shape file,
# including PR, VI that are not part of the UCR data
identical(factor(stateAbbrNbrs$StateAbbr), usgeo@data$STUSPS)
usmap <- append_data(usgeo, stateAbbrNbrs,
                     key.shp = "STUSPS", key.data="StateAbbr")
midwestFIPS <- c("17", "18", "19", "20", "26", "27", "29", "31", "38", "39",
                 "46", "55")
northeastFIPS <- c("09", "23", "25", "33", "34", "36", "42", "44", "50")
southFIPS <- c("01", "05", "10", "11", "12", "13", "21", "22", "24", "28",
               "37", "40", "45", "47", "48", "51", "54")
westFIPS <- c("02", "04", "06", "08", "15", "16", "30", "32", "35", "41",
              "49", "53", "56")
palGreens <- colorNumeric(palette = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D",
                                      "#238B45", "#006D2C", "#00441B"),
                          domain = usmap@data$officerAssault100)
midwestmap <- usmap[usmap@data$STATEFP %in% midwestFIPS, ]
midwestpopup <- paste0("<b>State: ", midwestmap@data$NAME,
                       "</b><br />Assaults: ",
                       midwestmap@data$OfficersAssaulted,
                       "</b><br />Assaults Per 100 Officers: ",
                       midwestmap@data$officerAssault100)
#                       "</b><br />Region: ",
#                       midwestmap@data$Region,
#                       "</b><br />Assaults per 100 Officers: ",
#                       midwestmap@data$RegionAssaultsPer100)
northeastmap <- usmap[usmap@data$STATEFP %in% northeastFIPS, ]
northeastpopup <- paste0("<b>State: ", northeastmap@data$NAME,
                       "</b><br />Assaults: ",
                       northeastmap@data$OfficersAssaulted,
                       "</b><br />Assaults Per 100 Officers: ",
                       northeastmap@data$officerAssault100)
southmap <- usmap[usmap@data$STATEFP %in% southFIPS, ]
southpopup <- paste0("<b>State: ", southmap@data$NAME,
                       "</b><br />Assaults: ",
                       southmap@data$OfficersAssaulted,
                       "</b><br />Assaults Per 100 Officers: ",
                       southmap@data$officerAssault100)
westmap <- usmap[usmap@data$STATEFP %in% westFIPS, ]
westpopup <- paste0("<b>State: ", westmap@data$NAME,
                    "</b><br />Assaults: ",
                    westmap@data$OfficersAssaulted,
                    "</b><br />Assaults Per 100 Officers: ",
                    westmap@data$officerAssault100)
us1map <- usmap[usmap@data$STATEFP %in% midwestFIPS |
                usmap@data$STATEFP %in% northeastFIPS |
                usmap@data$STATEFP %in% southFIPS |
                usmap@data$STATEFP %in% westFIPS, ]
us1popup <- paste0("<b>Region: ", us1map@data$Region,
                    "</b><br />Assaults per 100 Officers: ",
                    us1map@data$RegionAssaultsPer100)
#binPalRdYlBu <- colorBin(palette = "RdYlBu",
#binGreens <- colorBin(palette = "Greens",
#                      domain = us1map@data$RegionAssaultsPer100,
#                      8, pretty = FALSE)
shinyServer(function(input, output) {

#  output$about <- renderImage({
#    filename <- normalizePath(file.path('./images', "P32 police image copy.jpg"))
#    filename <- normalizePath(file.path('./images', "P33 cops.pdf"))
#    list(src = filename)
#  }, deleteFile = FALSE)
  
      output$midwest <- renderLeaflet(leaflet(midwestmap) %>%
                                    addProviderTiles("CartoDB.Positron") %>%
                                    addPolygons(stroke=FALSE, 
                                                smoothFactor = 0.2, 
                                                fillOpacity = .3, 
                                                popup=midwestpopup, 
                                                color= ~palGreens(midwestmap@data$officerAssault100)
                                    ) %>% fitBounds(-102.5, 36.5, -79, 49.5)
                                      %>% addLegend("bottomright",
                                                    title = legendTitle,
                                                    pal = palGreens,
                                                    values = ~usmap@data$officerAssault100,
                                                    opacity = 0.8
                                                    )
                                   )
      output$northeast <- renderLeaflet(leaflet(northeastmap) %>%
                                        addProviderTiles("CartoDB.Positron") %>%
                                        addPolygons(stroke=FALSE, 
                                                    smoothFactor = 0.2, 
                                                    fillOpacity = .3, 
                                                    popup=northeastpopup, 
                                                    color= ~palGreens(northeastmap@data$officerAssault100))
                                        %>% fitBounds(-81, 39, -67, 48)
#                                        %>% setView(-80, 39, zoom = 5)
                                      %>% addLegend("bottomright",
                                                    title = legendTitle,
                                                    pal = palGreens,
                                                    values = ~usmap@data$officerAssault100,
                                                    opacity = 0.8
                                      )
      )
      output$south <- renderLeaflet(leaflet(southmap) %>%
                                          addProviderTiles("CartoDB.Positron") %>%
                                          addPolygons(stroke=FALSE, 
                                                      smoothFactor = 0.2, 
                                                      fillOpacity = .3, 
                                                      popup=southpopup, 
                                                      color= ~palGreens(southmap@data$officerAssault100))
                                        %>% fitBounds(-107, 24, -75, 40)
                                        %>% addLegend("bottomright",
                                                      title = legendTitle,
                                                      pal = palGreens,
                                                      values = ~usmap@data$officerAssault100,
                                                      opacity = 0.8
                                        )
      )
      output$west <- renderLeaflet(leaflet(westmap) %>%
                                      addProviderTiles("CartoDB.Positron") %>%
                                      addPolygons(stroke=FALSE, 
                                                  smoothFactor = 0.2, 
                                                  fillOpacity = .3, 
                                                  popup=westpopup, 
                                                  color= ~palGreens(westmap@data$officerAssault100))
                                    %>% fitBounds(-125, 30, -102, 49)
                                    %>% addLegend("bottomright",
                                                  title = legendTitle,
                                                  pal = palGreens,
                                                  values = ~usmap@data$officerAssault100,
                                                  opacity = 0.8
                                    )
      )
      output$wholeus <- renderLeaflet(leaflet(us1map) %>%
                                     addProviderTiles("CartoDB.Positron") %>%
                                     addPolygons(stroke = FALSE, 
                                                 smoothFactor = 0.2, 
                                                 fillOpacity = .3, 
                                                 popup = us1popup,
#                                                 color = ~binGreens(us1map@data$RegionAssaultsPer100))
                                                 color= ~palGreens(us1map@data$RegionAssaultsPer100))
                                   %>% setView(-96.5, 37, zoom = 4)
                                   %>% addLegend("bottomright",
                                                 title = legendTitle,
                                                 pal = palGreens,
                                                 values = ~us1map@data$RegionAssaultsPer100,
                                                 opacity = 0.8
                                   )
      )
#  output$conclusion <- renderImage({
#    filename <- normalizePath(file.path('./images', "MapOfBostonBPD.png"))
#    list(src = filename)
#  }, deleteFile = FALSE)

})
