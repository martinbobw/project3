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
.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
legendTitle <- "Assaults per 100 Officers"
plotStateTitle <- "Assault Rate By State"
stateColClasses <- c("character", rep("integer", 2), rep("character", 6),
                     rep("integer", 4))
stateAbbrNbrs <- read.csv("P3 state abbr orig.csv", stringsAsFactors = FALSE,
                          colClasses = stateColClasses)
cityColClasses <- c(rep("character", 2), rep("integer", 2), rep("numeric", 2),
                    rep("character", 2))
top10Cities <- read.csv("P3 top10 cities.csv", stringsAsFactors = FALSE,
                        colClasses = cityColClasses)
stateAbbrNbrs$officers100 <- stateAbbrNbrs$NbrOfOfficers / 100
stateAbbrNbrs$officerAssault100 <- round((stateAbbrNbrs$OfficersAssaulted /
                                            stateAbbrNbrs$officers100), 1)
stateAbbrNbrs$regOfficers100 <- stateAbbrNbrs$RegionOfficers / 100
stateAbbrNbrs$RegionAssaultsPer100 <- round((stateAbbrNbrs$RegionAssaults /
                                               stateAbbrNbrs$regOfficers100), 1)
stateAbbrNbrs$divOfficers100 <- stateAbbrNbrs$DivisionOfficers / 100
stateAbbrNbrs$DivisionAssaultsPer100 <- round((stateAbbrNbrs$DivisionAssaults /
                                                 stateAbbrNbrs$divOfficers100), 1)
for(i in 1:length(top10Cities$v14)) {
  top10Cities[i, "v14"] <- tolower(top10Cities[i, "v14"])
  top10Cities[i, "v14"] <- .simpleCap(top10Cities[i, "v14"])
}
top10Cities$AssaultsPer100 <- round((top10Cities$TotalAssaults /
                                       (top10Cities$NbrOfficers / 100)), 1)
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
myPalette <- colorRampPalette(c("#A0BCA6", "#93A4A9", "#868DAC", "#7A75AF",
                                "#6D5DB1", "#6046B4", "#532EB7"))
myPal <- colorNumeric(c("#A0BCA6", "#93A4A9", "#868DAC", "#7A75AF",
                        "#6D5DB1", "#6046B4", "#532EB7"),
                      domain = usmap@data$officerAssault100)
bins <- c(10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14)
pal <- colorBin("YlOrRd", domain = usmap@data$RegionAssaultsPer100,
                bins = bins)
top10popup <- paste0("<b>City: ", top10Cities$v14,
                     "</b><br />Officers: ",
                     top10Cities$NbrOfficers,
                     "</b><br />Assaults: ",
                     top10Cities$TotalAssaults,
                     "</b><br />Assaults Per 100 Officers: ",
                     top10Cities$AssaultsPer100)
midwestmap <- usmap[usmap@data$STATEFP %in% midwestFIPS, ]
midwestpopup <- paste0("<b>State: ", midwestmap@data$NAME,
                       "</b><br />Officers: ",
                       midwestmap@data$NbrOfOfficers,
                       "</b><br />Assaults: ",
                       midwestmap@data$OfficersAssaulted,
                       "</b><br />Assaults Per 100 Officers: ",
                       midwestmap@data$officerAssault100)
northeastmap <- usmap[usmap@data$STATEFP %in% northeastFIPS, ]
northeastpopup <- paste0("<b>State: ", northeastmap@data$NAME,
                       "</b><br />Officers: ",
                       northeastmap@data$NbrOfOfficers,
                       "</b><br />Assaults: ",
                       northeastmap@data$OfficersAssaulted,
                       "</b><br />Assaults Per 100 Officers: ",
                       northeastmap@data$officerAssault100)
southmap <- usmap[usmap@data$STATEFP %in% southFIPS, ]
southpopup <- paste0("<b>State: ", southmap@data$NAME,
                       "</b><br />Officers: ",
                       southmap@data$NbrOfOfficers,
                       "</b><br />Assaults: ",
                       southmap@data$OfficersAssaulted,
                       "</b><br />Assaults Per 100 Officers: ",
                       southmap@data$officerAssault100)
westmap <- usmap[usmap@data$STATEFP %in% westFIPS, ]
westpopup <- paste0("<b>State: ", westmap@data$NAME,
                    "</b><br />Officers: ",
                    westmap@data$NbrOfOfficers,
                    "</b><br />Assaults: ",
                    westmap@data$OfficersAssaulted,
                    "</b><br />Assaults Per 100 Officers: ",
                    westmap@data$officerAssault100)
us1map <- usmap[usmap@data$STATEFP %in% midwestFIPS |
                usmap@data$STATEFP %in% northeastFIPS |
                usmap@data$STATEFP %in% southFIPS |
                usmap@data$STATEFP %in% westFIPS, ]
us1popup <- paste0("<b>Region: ", us1map@data$Region,
                   "</b><br />Officers: ",
                   us1map@data$RegionOfficers,
                   "</b><br />Assaults: ",
                   us1map@data$RegionAssaults,
                   "</b><br />Assaults per 100 Officers: ",
                   us1map@data$RegionAssaultsPer100)
shinyServer(function(input, output) {

      output$midwestBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% midwestFIPS1, ],
                                             aes(x = reorder(StateAbbr, -officerAssault100),
                                                 y = officerAssault100, fill = officerAssault100)) +
                                        geom_bar(stat = "identity") +
                                        geom_text(aes(label = officerAssault100), vjust = -0.75) +
                                        ggtitle(plotStateTitle) +
                                        xlab("") +
                                        ylab("Assaults per 100 Officers") +
                                        scale_fill_gradientn(colors = myPalette(7)) +
                                        theme(panel.grid.major.x = element_blank(),
                                              panel.grid.major = element_line(color = "grey60"),
                                              panel.grid.minor = element_line(color = "grey40"),
                                              panel.background = element_rect(fill = "white"),
                                              axis.ticks = element_blank(),
                                              legend.position = "none") +
                                        geom_hline(yintercept = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% midwestFIPS1,
                                                                     "RegionAssaultsPer100"], linetype = 2) +
                                        geom_text(aes(8, y = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% midwestFIPS1,
                                                                           "RegionAssaultsPer100"] + 1,
                                                      label = paste("Average assaults = ", stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% midwestFIPS1,
                                                                                                      "RegionAssaultsPer100"]),
                                                                    vjust = 1), size = 5)
                                      )
      output$midwest <- renderLeaflet(leaflet(midwestmap) %>%
                                    addProviderTiles("CartoDB.Positron",
                                                     options = providerTileOptions(minZoom = 5, maxZoom = 5,
                                                                                   zoomControl = FALSE)) %>%
                                    addPolygons(stroke=FALSE, 
                                                smoothFactor = 0.2, 
                                                fillOpacity = .3, 
                                                popup=midwestpopup, 
                                                color= ~myPal(midwestmap@data$officerAssault100)
                                    )                                     %>% fitBounds(-102.5, 36.5, -79, 49.5)
                                %>% addLegend("bottomright",
                                              title = legendTitle,
                                              pal = myPal,
                                              values = ~usmap@data$officerAssault100,
                                              opacity = 0.8
                                              )
                                   )
      output$northeastBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% northeastFIPS1, ],
                                             aes(x = reorder(StateAbbr, -officerAssault100),
                                                 y = officerAssault100, fill = officerAssault100)) +
                                        geom_bar(stat = "identity") +
                                        geom_text(aes(label = officerAssault100), vjust = -0.75) +
                                        ggtitle(plotStateTitle) +
                                        xlab("") +
                                        ylab("Assaults per 100 Officers") +
                                        scale_fill_gradientn(colors = myPalette(7)) +
                                        theme(panel.grid.major.x = element_blank(),
                                              panel.grid.major = element_line(color = "grey60"),
                                              panel.grid.minor = element_line(color = "grey40"),
                                              panel.background = element_rect(fill = "white"),
                                              axis.ticks = element_blank(),
                                              legend.position = "none") +
                                        geom_hline(yintercept = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% northeastFIPS1,
                                                                              "RegionAssaultsPer100"], linetype = 2) +
                                        geom_text(aes(6.75, y = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% northeastFIPS1,
                                                                           "RegionAssaultsPer100"] + 1.5,
                                                        label = paste("Average assaults = ", stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% northeastFIPS1,
                                                                                                           "RegionAssaultsPer100"]),
                                                        vjust = 0.1), size = 5)
                                        )
      output$northeast <- renderLeaflet(leaflet(northeastmap) %>%
                                        addProviderTiles("CartoDB.Positron",
                                                         options = providerTileOptions(minZoom = 6, maxZoom = 6,
                                                                                       zoomControl = FALSE)) %>%
                                        addPolygons(stroke=FALSE, 
                                                    smoothFactor = 0.2, 
                                                    fillOpacity = .3, 
                                                    popup=northeastpopup, 
                                                    color= ~myPal(northeastmap@data$officerAssault100))
#                                        %>% fitBounds(-81, 39, -67, 48)
                                      %>% setView(-72, 43.25, zoom = 6)
                                      %>% addLegend("bottomright",
                                                    title = legendTitle,
                                                    pal = myPal,
                                                    values = ~usmap@data$officerAssault100,
                                                    opacity = 0.8
                                      )
      )
      output$southBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% southFIPS1, ],
                                               aes(x = reorder(StateAbbr, -officerAssault100),
                                                   y = officerAssault100, fill = officerAssault100)) +
                                          geom_bar(stat = "identity") +
                                          geom_text(aes(label = officerAssault100), vjust = -0.75) +
                                          ggtitle(plotStateTitle) +
                                          xlab("") +
                                          ylab("Assaults per 100 Officers") +
                                          scale_fill_gradientn(colors = myPalette(7)) +
                                          theme(panel.grid.major.x = element_blank(),
                                                panel.grid.major = element_line(color = "grey60"),
                                                panel.grid.minor = element_line(color = "grey40"),
                                                panel.background = element_rect(fill = "white"),
                                                axis.ticks = element_blank(),
                                                legend.position = "none") +
                                      geom_hline(yintercept = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% southFIPS1,
                                                                            "RegionAssaultsPer100"], linetype = 2) +
                                      geom_text(aes(13, y = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% southFIPS1,
                                                                         "RegionAssaultsPer100"] + 1.5,
                                                    label = paste("Average assaults = ", stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% southFIPS1,
                                                                                                       "RegionAssaultsPer100"]),
                                                    vjust = 0.25), size = 5)
                                      )
      output$south <- renderLeaflet(leaflet(southmap) %>%
                                          addProviderTiles("CartoDB.Positron",
                                                           options = providerTileOptions(minZoom = 5, maxZoom = 5,
                                                                                         zoomControl = FALSE)) %>%
                                          addPolygons(stroke=FALSE, 
                                                      smoothFactor = 0.2, 
                                                      fillOpacity = .3, 
                                                      popup=southpopup, 
                                                      color= ~myPal(southmap@data$officerAssault100))
                                        %>% fitBounds(-107, 24, -75, 40)
                                        %>% addLegend("bottomright",
                                                      title = legendTitle,
                                                      pal = myPal,
                                                      values = ~usmap@data$officerAssault100,
                                                      opacity = 0.8
                                        )
      )
      output$westBar <- renderPlot(ggplot(data = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% westFIPS1, ],
                                           aes(x = reorder(StateAbbr, -officerAssault100),
                                               y = officerAssault100, fill = officerAssault100)) +
                                      geom_bar(stat = "identity") +
                                      geom_text(aes(label = officerAssault100), vjust = -0.75) +
                                      ggtitle(plotStateTitle) +
                                      xlab("") +
                                      ylab("Assaults per 100 Officers") +
                                      scale_fill_gradientn(colors = myPalette(7)) +
                                      theme(panel.grid.major.x = element_blank(),
                                            panel.grid.major = element_line(color = "grey60"),
                                            panel.grid.minor = element_line(color = "grey40"),
                                            panel.background = element_rect(fill = "white"),
                                            axis.ticks = element_blank(),
                                            legend.position = "none") +
                                     geom_hline(yintercept = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% westFIPS1,
                                                                           "RegionAssaultsPer100"], linetype = 2) +
                                     geom_text(aes(10.5, y = stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% westFIPS1,
                                                                         "RegionAssaultsPer100"] + 1.5,
                                                   label = paste("Average assaults = ", stateAbbrNbrs[stateAbbrNbrs$FIPSCode %in% westFIPS1,
                                                                                                      "RegionAssaultsPer100"]),
                                                   vjust = 1), size = 5)
                                     )
      output$west <- renderLeaflet(leaflet(westmap) %>%
                                      addProviderTiles("CartoDB.Positron",
                                                       options = providerTileOptions(minZoom = 5, maxZoom = 5,
                                                                                     zoomControl = FALSE)) %>%
                                      addPolygons(stroke=FALSE, 
                                                  smoothFactor = 0.2, 
                                                  fillOpacity = .3, 
                                                  popup=westpopup, 
                                                  color= ~myPal(westmap@data$officerAssault100))
#                                     %>% fitBounds(-125, 30, -102, 49)
                                    %>% setView(-113, 40.75, zoom = 5)
                                    %>% addLegend("bottomright",
                                                  title = legendTitle,
                                                  pal = myPal,
                                                  values = ~usmap@data$officerAssault100,
                                                  opacity = 0.8
                                    )
      )
#      output$wholeBar <- renderPlot(ggplot(data = regionNumbers,
#                                          aes(x = reorder(Region, -RegionAssaultsPer100),
#                                              y = RegionAssaultsPer100)) +
#                                     geom_bar(stat = "identity", fill = palBar) +
#                                     ggtitle("Assault Rate By Region") +
#                                     xlab("") +
#                                     ylab("Assaults per 100 Officers") +
#                                     theme(panel.grid.major.x = element_blank(),
#                                           panel.grid.major = element_line(color = "grey60"),
#                                           panel.grid.minor = element_line(color = "grey40"),
#                                           panel.background = element_rect(fill = "white"),
#                                           axis.ticks = element_blank()))
      output$wholeus <- renderLeaflet(leaflet(us1map) %>%
                                     addProviderTiles("CartoDB.Positron",
                                                      options = providerTileOptions(zoomControl = FALSE,
                                                                minZoom = 4, maxZoom = 4)) %>%
                                     addPolygons(stroke = FALSE, 
                                                 smoothFactor = 0.2, 
                                                 fillOpacity = .3, 
                                                 popup = us1popup,
                                                 fillColor = ~pal(us1map@data$RegionAssaultsPer100))
                               %>%   addCircleMarkers(lng = ~top10Cities$Lon,
                                                 lat = ~top10Cities$Lat,
                                                 radius = ~top10Cities$AssaultsPer100,
                                                 color = "red",
#                                                 color = ~ifelse(top10Cities$AssaultsPer100 > 17.5, "red", "orange"),
                                                 stroke = TRUE,
                                                 fillOpacity = 0.2,
                                                 popup = ~top10popup) 
#                                                 color = ~binGreens(us1map@data$RegionAssaultsPer100))
#                                                 color= ~palWhole(us1map@data$RegionAssaultsPer100))
                                   %>% setView(-96.5, 37, zoom = 4)
                                   %>% addLegend("bottomright",
                                                 title = legendTitle,
#                                                 pal = palWhole,
                                                 pal = pal,
                                                 values = ~us1map@data$RegionAssaultsPer100,
                                                 opacity = 0.8
                                                )
      )
#  output$conclusion <- renderImage({
#    filename <- normalizePath(file.path('./images', "MapOfBostonBPD.png"))
#    list(src = filename)
#  }, deleteFile = FALSE)

})
