#' Shiny app server function
#'
#' @param input provided by Shiny
#'
#' @param output provided by Shiny
#'

shinyAppServer <- function(input, output) {


  output$plot1 <- renderPlot({



    mat = matrix(c(input$s11,input$s12,input$s12, input$s22), nr=2,nc=2)
    mui = c(input$mu1,input$mu2)

    xbarthetadist(n=input$nnumber, iter=input$iternumber, mu = mui, sigma=mat)
    })

}

