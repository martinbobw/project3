
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

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
                       radioButtons("region", "Choose a region", choices = c(
                                    "Midwest" = 1, "Northeast" = 2,
                                    "South" = 3, "West" = 4),
                                    selected = 1),
                       p(),
                       p(),
                       p(),
                       p("Click inside a state to see assault statistics")
                       ),
      conditionalPanel(condition="input.tabselected==3",
#                       plotOutput("conclusion"),
                       p("Through statistical analysis, the four regions'
                         average number of assaults per 100 officers have
                         been compared to one another."))

    ),

  mainPanel(
      tabsetPanel(
        tabPanel("About", value = 1, tags$iframe(src="https://www.dropbox.com/s/z5r45tbnuq8u2ky/P33%20Tab%201.pdf?raw=1",
                                                 width = "900", height = "600")),
        tabPanel("Maps", value = 2, leafletOutput("region")),
        tabPanel("Conclusion", value = 3, tags$iframe(src="https://www.dropbox.com/s/z5r45tbnuq8u2ky/P33%20Tab%201.pdf?raw=1",
                                                      width = "900", height = "600")),
        id = "tabselected"
      )
    )
  )
)