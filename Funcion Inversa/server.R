
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(datasets)
library(ggplot2)
#install.packages("cowplot")
library("cowplot")

shinyServer(function(input, output) {

  
  
  output$distPlot <- renderPlot({
    set.seed(10)
    u <- runif(1000) 
    x <- (1/input$lambda)*log(1/(1-u))

    #hist(x, breaks = 50, col = 'darkgray', border = 'white')
    plot1 <- qplot(u, geom="histogram", fill=I("blue"),main = "Generacion de numero uniformes", col=I("red"), alpha=I(.2))
    plot2 <- qplot(x, geom="density", fill=I("blue"),main = "Distribucion Exponencial", col=I("red"), alpha=I(.2))
    plot_grid(plot1, plot2, ncol = 1, nrow = 2)
    
  })
  
  output$view <- renderTable({
    set.seed(10)
    u <- runif(1000)
    x <- (1/input$lambda)*log(1/(1-u))
    data <- as.data.frame(cbind(x,u))
    names(data) <- c("X","Probabilidad Acum")
    
    data  
  })

})
