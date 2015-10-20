
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Confidence Intervals for a Mean"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "population",
        "Population",
        choices =
          list( normal = "norm",
                beta = "beta",
                gamma = "gamma"
          )
      ),
      uiOutput("paramControls"),
      selectInput(
        "n", "Sample Size",
        choices =
          list(5, 10, 20, 40, 80, 160, 320),
        selected = 20
        ),
      sliderInput("level", "Confidence Level",
                  min=.50, max = 0.99, value = 0.95, step = 0.01),
      textOutput("message"),
      actionButton("more", "New Samples"),
      textOutput("runningTotal")
      ),  # end sidebarPanel

    mainPanel(
#      plotOutput("populationPlot", height = "110px"),
      plotOutput("bigPlot", height = "180px"),
#      plotOutput("samplePlot", height = "110px"),
      plotOutput("intervalsPlot", height = "400px", click = "plot_click"),
      p("Click on an interval to show a histogram for the underlying sample.")
    )
  )
))
