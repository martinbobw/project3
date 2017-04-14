
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
shinyUI(pageWithSidebar(
  headerPanel("Law Enforcement Officers Assaulted - 1997"),
  sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       h4("About"),
                       p(),
                       p("The Law Enforcement Officers Killed and Assaulted (LEOKA)
                         data is compiled and published by the FBI on an annual
                         basis. The data is collected through several methods
                         including individual police agencies (sheriff, city)
                         submitting reports to the FBI, local FBI offices
                         reporting data, as well as the FBI collecting data on
                         law enforcement agents killed or assaulted while
                         outside U.S. jurisdiction. Assaults are tracked by
                         type of weapon - hands, feet, knives, guns, with
                         personal weapons (e.g. hands, feet) being the primary
                         method of assault. Statistics are compiled at the
                         police agency level and have been aggregated at the state
                         and regional levels which are the focus of this 
                         presentation."),
                       h4("Hypothesis"),
                       p(),
                       p("My hypothesis is that there is a difference between the four U.S.
                         regions in terms of number of assaults per 100 officers. The
                         hypothesis will be tested using known statistical methods.")),
      conditionalPanel(condition="input.tabselected==2",
                       h4("Midwestern Region"),
                       p(),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p(),
                       plotOutput("midwestBar")),
      conditionalPanel(condition="input.tabselected==3",
                       h4("Northeastern Region"),
                       p(),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p("Zoom or pan to see smaller states"),
                       plotOutput("northeastBar")),
      conditionalPanel(condition="input.tabselected==4",
                       h4("Southern Region"),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p("Zoom or pan to see smaller states"),
                       plotOutput("southBar")),
      conditionalPanel(condition="input.tabselected==5",
                       h4("Western Region"),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p("Zoom or pan to see Alaska and Hawaii"),
                       plotOutput("westBar")),
      conditionalPanel(condition="input.tabselected==6",
                       h4("Methodology"),
                       p(),
                       p("The original agency level data set was cleaned to
                         remove police agencies that did not report the number
                         of officers or that did not report full year assaults.
                         Assaults were then summarized at the state and 
                         regional levels."),
                       h4("Results"),
                       p(),
                       p("Through statistical analysis, the four regions'
                         average number of assaults per 100 officers have
                         been compared to one another. There is no significant
                         difference between U.S. regions."),
                       p(),
                       p("Click inside a state to see assault statistics for that
                         U.S. region. Zoom or pan to see Alaska and Hawaii."),
                       plotOutput("wholeBar"))
    ),
  mainPanel(
      tabsetPanel(
        tabPanel("Introduction", value = 1, img(src = "P33_Tab_1.png", height = "600px",
                                                width = "900px"),
#         tabPanel("Introduction", value = 1, img(src = "/Users/robertmartin/Desktop/Northeastern/PPUA 6302 Info Design Visual Analytics/P33_Martin_R/images/P33_Tab_1.png",
#                                                 height = "600px"),
#        tabPanel("Introduction", value = 1, tags$iframe(src="https://www.dropbox.com/s/coqiwi49vf3layf/P33%20Tab%201.pdf?raw=1",
#                                                        width = "900", height = "600"),
                 includeCSS(path = "www/bootstrap.css")),
        tabPanel("Midwest", value = 2, 
                 leafletOutput("midwest", height = "600px")),
        tabPanel("Northeast", value = 3, leafletOutput("northeast",
                                                       height = "600px")),
        tabPanel("South", value = 4, leafletOutput("south",
                                                   height = "600px")),
        tabPanel("West", value = 5, leafletOutput("west",
                                                  height = "600px")),
        tabPanel("Conclusion", value = 6, leafletOutput("wholeus",
                                                        height = "600px")),
        id = "tabselected"
      ),
      style = 'height:600px'
    )
  )
)