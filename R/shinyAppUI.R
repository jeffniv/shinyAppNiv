#' Shiny app server object
#'
#' @import shiny
#' @import shinydashboard
#'

shinyAppUI <- dashboardPage(
    dashboardHeader(title = paste("Simulation of Xbar and ", expression(theta) ), titleWidth = 500),
    dashboardSidebar(sliderInput("nnumber", "Sample size",30,300,60,step=30),
                     numericInput("iternumber", "Iterations", 1000),numericInput("mu1", "mu 1", 0),
                     numericInput("mu2", "mu 2", 0),numericInput("s11", "variance x1", 100),
                     numericInput("s12", "covariance x1,x2", 40),
                     numericInput("s22", "variance x2", 400)
                     ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotOutput("plot1", height = 500))

            )
        )
    )
