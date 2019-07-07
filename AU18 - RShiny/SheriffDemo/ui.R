library(shiny)
library(shinydashboard)
dashboardPage(skin="red",
              dashboardHeader(title="BDAA-ShinyDemo",
                              dropdownMenuOutput("login_info")
                              ),
              dashboardSidebar(
                uiOutput("sidebar_ui")
                
              ),
              dashboardBody(
                useShinyjs(),
                uiOutput("ui")
              )
)