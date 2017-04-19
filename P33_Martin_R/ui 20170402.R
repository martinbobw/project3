
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
                       p("The Law Enforcement Officers Killed and Assaulted (LEOKA)
                         data is compiled and published by the FBI on an annual basis. 
                         The data is collected through several methods
                         including individual police agencies (sheriff, city)
                         submitting reports to the FBI, local FBI offices
                         reporting data, as well as FBI collecting data on
                         law enforcement agents killed or assaulted while
                         outside U.S. jurisdiction. Assaults are tracked by
                         type of weapon - hands, feet, knives, guns, with
                         personal weapons (e.g. hands, feet) being the primary
                         method of assault. Statistics are compiled at the
                         state and regional levels, and are the focus of
                         this presentation.")),
      conditionalPanel(condition="input.tabselected==2",
                       h3("Midwestern states"),
                       p("Click inside a state to see assault statistics")),
      conditionalPanel(condition="input.tabselected==3",
                       h3("Northeastern states"),
                       p("Click inside a state to see assault statistics")),
      conditionalPanel(condition="input.tabselected==4",
                       h3("Southern states"),
                       p("Click inside a state to see assault statistics")),
      conditionalPanel(condition="input.tabselected==5",
                       h3("Western states"),
                       p("Click inside a state to see assault statistics"),
                       p(),
                       p(),
                       p("Zoom out to see Alaska and Hawaii")),
      conditionalPanel(condition="input.tabselected==6",
#                       plotOutput("conclusion"),
                       p("Through statistical analysis, the four regions'
                         average number of assaults per 100 officers have
                         been compared to one another."))

    ),

  mainPanel(
      tabsetPanel(
        tabPanel("About", value = 1, tags$iframe(src="https://www.dropbox.com/s/z5r45tbnuq8u2ky/P33%20Tab%201.pdf?raw=1",
                                                 width = "900", height = "600")),
        tabPanel("Midwest", value = 2, leafletOutput("midwest")),
        tabPanel("Northeast", value = 3, leafletOutput("northeast")),
        tabPanel("South", value = 4, leafletOutput("south")),
        tabPanel("West", value = 5, leafletOutput("west")),
        tabPanel("Conclusion", value = 6, tags$iframe(src="https://www.dropbox.com/s/z5r45tbnuq8u2ky/P33%20Tab%201.pdf?raw=1",
                                                      width = "900", height = "600")),
        id = "tabselected"
      )
    )
  )
)