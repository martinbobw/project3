
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
shinyUI(pageWithSidebar(
  headerPanel("Law Enforcement Officers Killed and Assaulted"),
  sidebarPanel(
      conditionalPanel(condition="input.tabselected==1",
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
                       p("Click inside a state to see assault statistics")),
      conditionalPanel(condition="input.tabselected==3",
                       h4("Northeastern Region"),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p("Zoom or pan to see smaller states")),
      conditionalPanel(condition="input.tabselected==4",
                       h4("Southern Region"),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p("Zoom or pan to see smaller states")),
      conditionalPanel(condition="input.tabselected==5",
                       h4("Western Region"),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p("Zoom or pan to see Alaska and Hawaii")),
      conditionalPanel(condition="input.tabselected==6",
                       h4("Methodology"),
                       p(),
                       p(""),
                       p(),
                       h4("Results"),
                       p(),
                       p("Through statistical analysis, the four regions'
                         average number of assaults per 100 officers have
                         been compared to one another. There is no significant
                         difference in the average number of assaults per 100
                         officers between U.S. regions."),
                       p(),
                       p(),
                       p("Click inside a state to see assault statistics for that
                         U.S. region. Note the color scale has shifted from state
                         level to region level."),
                       p(),
                       p(),
                       p("Zoom or pan to see Alaska and Hawaii."))
    ),
  mainPanel(
      tabsetPanel(
        tabPanel("Introduction", value = 1, tags$iframe(src="https://www.dropbox.com/s/coqiwi49vf3layf/P33%20Tab%201.pdf?raw=1",
                                                        width = "900", height = "600")),
        tabPanel("Midwest", value = 2, leafletOutput("midwest")),
        tabPanel("Northeast", value = 3, leafletOutput("northeast")),
        tabPanel("South", value = 4, leafletOutput("south")),
        tabPanel("West", value = 5, leafletOutput("west")),
        tabPanel("Conclusion", value = 6, leafletOutput("wholeus")),
        id = "tabselected"
      )
    )
  )
)