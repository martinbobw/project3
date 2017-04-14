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
library(ggplot2)
legendTitle <- "Assaults per 100 Officers"
stateColClasses <- c("character", rep("integer", 2), rep("character", 6),
                     rep("integer", 4))
stateAbbrNbrs <- read.csv("P3 state abbr orig.csv", stringsAsFactors = FALSE,
                          colClasses = stateColClasses)
stateAbbrNbrs$officers100 <- stateAbbrNbrs$NbrOfOfficers / 100
stateAbbrNbrs$officerAssault100 <- round((stateAbbrNbrs$OfficersAssaulted /
                                            stateAbbrNbrs$officers100), 1)
stateAbbrNbrs$regOfficers100 <- stateAbbrNbrs$RegionOfficers / 100
stateAbbrNbrs$RegionAssaultsPer100 <- round((stateAbbrNbrs$RegionAssaults /
                                               stateAbbrNbrs$regOfficers100), 1)
stateAbbrNbrs$divOfficers100 <- stateAbbrNbrs$DivisionOfficers / 100
stateAbbrNbrs$DivisionAssaultsPer100 <- round((stateAbbrNbrs$DivisionAssaults /
                                                 stateAbbrNbrs$divOfficers100), 1)
stateAbbrNbrsNoNA <- stateAbbrNbrs[!is.na(stateAbbrNbrs$OfficersAssaulted), ]
# could do this in a compound OR statement but doing it in 2
stateAbbrNbrsNoNA <- stateAbbrNbrsNoNA[!is.na(stateAbbrNbrsNoNA$NbrOfOfficers), ]
# FIPS codes for all 50 states and DC
midwestFIPS <- c("17", "18", "19", "20", "26", "27", "29", "31", "38", "39",
                 "46", "55")
northeastFIPS <- c("09", "23", "25", "33", "34", "36", "42", "44", "50")
southFIPS <- c("01", "05", "10", "11", "12", "13", "21", "22", "24", "28",
               "37", "40", "45", "47", "48", "51", "54")
westFIPS <- c("02", "04", "06", "08", "15", "16", "30", "32", "35", "41",
              "49", "53", "56")
statesDCFIPS <- c(midwestFIPS, northeastFIPS, southFIPS, westFIPS)
regionNumbers <- unique(stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% statesDCFIPS,
                                      c("Region", "RegionAssaultsPer100")])
# These FIPS codes below indicate the states that have data for 1997
midwestFIPS1 <- c("18", "19", "26", "27", "29", "31", "38", "39", "46", "55")
northeastFIPS1 <- c("09", "23", "25", "33", "34", "36", "42", "44")
southFIPS1 <- c("01", "05", "10", "11", "13", "21", "22", "24", "28",
                "37", "40", "45", "47", "48", "51", "54")
westFIPS1 <- c("02", "04", "06", "08", "15", "16", "30", "32", "35", "41",
               "49", "53", "56")
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
palGreens <- colorNumeric(palette = c("#C7E9C0", "#A1D99B", "#74C476", "#41AB5D",
                                      "#238B45", "#006D2C", "#00441B"),
                          domain = usmap@data$officerAssault100)
palMidwest <- colorNumeric(palette = c("#BACDC6", "#98B499", "#678674",
                                       "#4C5C58", "#3C3C68", "#8987CC"),
                           domain = usmap@data$officerAssault100)
palNortheast <- colorNumeric(palette = c("#E0E7E0", "#F1E3D2", "#FE97BE",
                                         "#E65B51", "#2A5F9E", "#6FB882"),
                           domain = usmap@data$officerAssault100)
palSouth <- colorNumeric(palette = c("#F6EDEA", "#F3DBD3", "#F8A28C",
                                     "#8F7B69", "#8FA67A", "#EDE0A6"),
                             domain = usmap@data$officerAssault100)
palWest <- colorNumeric(palette = c("#95C4B2", "#6CB284", "#427951",
                                    "#295333", "#103A25", "#222D26"),
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
binGreens <- colorBin(palette = "Greens",
                      domain = us1map@data$RegionAssaultsPer100,
                      8, pretty = FALSE)
shinyServer(function(input, output) {

      output$midwestBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% midwestFIPS1, ],
                                             aes(x = reorder(StateAbbr, -officerAssault100),
                                                 y = officerAssault100)) +
                                        geom_bar(stat = "identity", fill = "#98B499") +
                                        ggtitle("Officers Assaulted by State - Midwest") +
                                        xlab("State") +
                                        ylab("Assaults per 100 Officers") +
                                        theme(panel.grid.major.x = element_blank(),
                                              panel.grid.major = element_line(color = "grey60"),
                                              panel.grid.minor = element_line(color = "grey40"),
                                              panel.background = element_rect(fill = "white"),
                                              axis.ticks = element_blank()))
      output$midwest <- renderLeaflet(leaflet(midwestmap) %>%
                                    addProviderTiles("CartoDB.Positron") %>%
                                    addPolygons(stroke=FALSE, 
                                                smoothFactor = 0.2, 
                                                fillOpacity = .3, 
                                                popup=midwestpopup, 
                                                color= ~palMidwest(midwestmap@data$officerAssault100)
                                    ) %>% fitBounds(-102.5, 36.5, -79, 49.5)
                                      %>% addLegend("bottomright",
                                                    title = legendTitle,
                                                    pal = palMidwest,
                                                    values = ~usmap@data$officerAssault100,
                                                    opacity = 0.8
                                                    )
                                   )
      output$northeastBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% northeastFIPS1, ],
                                             aes(x = reorder(StateAbbr, -officerAssault100),
                                                 y = officerAssault100)) +
                                        geom_bar(stat = "identity", fill = "#F1E3D2") +
                                        ggtitle("Officers Assaulted by State - Northeast") +
                                        xlab("State") +
                                        ylab("Assaults per 100 Officers") +
                                        theme(panel.grid.major.x = element_blank(),
                                              panel.grid.major = element_line(color = "grey60"),
                                              panel.grid.minor = element_line(color = "grey40"),
                                              panel.background = element_rect(fill = "white"),
                                              axis.ticks = element_blank()))
      output$northeast <- renderLeaflet(leaflet(northeastmap) %>%
                                        addProviderTiles("CartoDB.Positron") %>%
                                        addPolygons(stroke=FALSE, 
                                                    smoothFactor = 0.2, 
                                                    fillOpacity = .3, 
                                                    popup=northeastpopup, 
                                                    color= ~palNortheast(northeastmap@data$officerAssault100))
                                        %>% fitBounds(-81, 39, -67, 48)
                                      %>% addLegend("bottomright",
                                                    title = legendTitle,
                                                    pal = palNortheast,
                                                    values = ~usmap@data$officerAssault100,
                                                    opacity = 0.8
                                      )
      )
      output$southBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% southFIPS1, ],
                                               aes(x = reorder(StateAbbr, -officerAssault100),
                                                   y = officerAssault100)) +
                                          geom_bar(stat = "identity", fill = "#F3DBD3") +
                                          ggtitle("Officers Assaulted by State - South") +
                                          xlab("State") +
                                          ylab("Assaults per 100 Officers") +
                                          theme(panel.grid.major.x = element_blank(),
                                                panel.grid.major = element_line(color = "grey60"),
                                                panel.grid.minor = element_line(color = "grey40"),
                                                panel.background = element_rect(fill = "white"),
                                                axis.ticks = element_blank()))
      output$south <- renderLeaflet(leaflet(southmap) %>%
                                          addProviderTiles("CartoDB.Positron") %>%
                                          addPolygons(stroke=FALSE, 
                                                      smoothFactor = 0.2, 
                                                      fillOpacity = .3, 
                                                      popup=southpopup, 
                                                      color= ~palSouth(southmap@data$officerAssault100))
                                        %>% fitBounds(-107, 24, -75, 40)
                                        %>% addLegend("bottomright",
                                                      title = legendTitle,
                                                      pal = palSouth,
                                                      values = ~usmap@data$officerAssault100,
                                                      opacity = 0.8
                                        )
      )
      output$westBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% westFIPS1, ],
                                           aes(x = reorder(StateAbbr, -officerAssault100),
                                               y = officerAssault100)) +
                                      geom_bar(stat = "identity", fill = "#6CB284") +
                                      ggtitle("Officers Assaulted by State - West") +
                                      xlab("State") +
                                      ylab("Assaults per 100 Officers") +
                                      theme(panel.grid.major.x = element_blank(),
                                            panel.grid.major = element_line(color = "grey60"),
                                            panel.grid.minor = element_line(color = "grey40"),
                                            panel.background = element_rect(fill = "white"),
                                            axis.ticks = element_blank()))
      output$west <- renderLeaflet(leaflet(westmap) %>%
                                      addProviderTiles("CartoDB.Positron") %>%
                                      addPolygons(stroke=FALSE, 
                                                  smoothFactor = 0.2, 
                                                  fillOpacity = .3, 
                                                  popup=westpopup, 
                                                  color= ~palWest(westmap@data$officerAssault100))
                                    %>% fitBounds(-125, 30, -102, 49)
                                    %>% addLegend("bottomright",
                                                  title = legendTitle,
                                                  pal = palWest,
                                                  values = ~usmap@data$officerAssault100,
                                                  opacity = 0.8
                                    )
      )
      output$wholeBar <- renderPlot(ggplot(data = regionNumbers,
                                          aes(x = reorder(Region, -RegionAssaultsPer100),
                                              y = RegionAssaultsPer100)) +
                                     geom_bar(stat = "identity", fill = "#6CB284") +
                                     ggtitle("Officers Assaulted By Region") +
                                     xlab("Region") +
                                     ylab("Assaults per 100 Officers") +
                                     theme(panel.grid.major.x = element_blank(),
                                           panel.grid.major = element_line(color = "grey60"),
                                           panel.grid.minor = element_line(color = "grey40"),
                                           panel.background = element_rect(fill = "white"),
                                           axis.ticks = element_blank()))
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
