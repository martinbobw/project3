library(shiny)
library(leaflet)
shinyUI(pageWithSidebar(
  headerPanel("Law Enforcement Officers Assaulted - 1997"),
  sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1",
                       h4("Data"),
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
                         hypothesis will be tested using known statistical methods."),
                       h4("Methodology"),
                       p(),
                       p("The original police agency level data set was cleaned to
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
                       h6("by Bob Martin, Northeastern University, PPUA 6302 Spring 2017")),
      conditionalPanel(condition="input.tabselected==2",
                       h4("Midwestern Region"),
                       p(),
                       em("Click inside a state on the map to see assault statistics.",
                          style = "color:#532EB7"),
                       p(),
                       p(),
                       p(),
                       plotOutput("midwestBar")),
      conditionalPanel(condition="input.tabselected==3",
                       h4("Northeastern Region"),
                       p(),
                       em("Click inside a state on the map to see assault statistics.",
                          style = "color:#532EB7"),
                       p(),
                       p(),
                       p(),
                       plotOutput("northeastBar")),
      conditionalPanel(condition="input.tabselected==4",
                       h4("Southern Region"),
                       em("Click inside a state on the map to see assault statistics.",
                          style = "color:#532EB7"),
                       p(),
                       p(),
                       p(),
                       plotOutput("southBar")),
      conditionalPanel(condition="input.tabselected==5",
                       h4("Western Region"),
                       em("Click inside a state on the map to see assault statistics.
                          Zoom out or drag the map to see Alaska and Hawaii.",
                          style = "color:#532EB7"),
                       p(),
                       p(),
                       p(),
                       plotOutput("westBar"))
    ),
  mainPanel(
      tabsetPanel(
        tabPanel("About", value = 1, leafletOutput("wholeus",
                                                        height = "475px"),
                 h4("Remarks"),
                 p("Assaults with injury are 3 per 100 officers, the same
                   rate across the four U.S. regions."),
                 h4("Most Populous Cities"),
                 p("The top 10 cities in terms of population as of the 2000 U.S. Census
                   are shown above - New York, Los Angeles, Houston, Philadelphia, Phoenix,
                   San Diego, Dallas, San Antonio, Detroit, and San Jose are
                   shown above. Chicago, the third most populous city, is missing data.
                   The size of the circles correspond to the
                   assault rate. Figures for county level
                   police agencies are not included in the city figures."),
                 em("Click on a region or city above to see assault
                    statistics.", style = "color:blue"),
#        tabPanel("Introduction", value = 1, img(src = "P33_Tab_1.png", height = "600px",
#                                                width = "900px"),
#        tabPanel("Introduction", value = 1, tags$iframe(src="https://www.dropbox.com/s/coqiwi49vf3layf/P33%20Tab%201.pdf?raw=1",
#                                                        width = "900", height = "600"),
                 includeCSS(path = "www/bootstrap.css")),
        tabPanel("Midwest", value = 2, 
                 leafletOutput("midwest", height = "550px"),
                 h4("Remarks"),
                 p("Illinois and Kansas have missing data. Cities with
                   populations between 100,000 and 249,999 have the highest
                   assault rate at 17 assaults per 100 officers. Rural areas
                   with populations under 10,000 have the lowest assault
                   rates at 5 assaults per 100 officers.")),
        tabPanel("Northeast", value = 3, leafletOutput("northeast",
                                                       height = "550px"),
                 h4("Remarks"),
                 p("Vermont has missing data. Cities with populations between
                   100,000 and 250,000 recorded the highest assault with
                   injury rate at 5 per 100 officers. Populations under 10,000
                   record assaults with injury at 2.3 per 100 officers.")),
        tabPanel("South", value = 4, leafletOutput("south",
                                                   height = "550px"),
                 h4("Remarks"),
                 p("Florida has missing data. Across the U.S., firearms
                   account for 3.8% of assaults against officers. 83.5%
                   of assaults use personal weapons - hands, feet, and fists
                   as weapons.")),
        tabPanel("West", value = 5, leafletOutput("west",
                                                  height = "550px"),
                 h4("Remarks"),
                 p("Disturbance calls (family quarrels, man with gun, etc.)
                   represent 31.7% of circumstances where officers were
                   assaulted, and 86.1% of those assaults were with
                   personal weapons. Ambush with no warning were only 0.3%
                   of assaults, but 43.6% of those incidents involved
                   firearms.")),
        id = "tabselected"
      ),
      style = 'height:600px'
    )
  )
)