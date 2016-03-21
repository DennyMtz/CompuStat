
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
#install.packages('ggplot2')
library(shiny)
library(ggplot2)
#install.packages("cowplot")
library(cowplot)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    set.seed(5)
    
    a <- runif(input$size)
    b <- runif(input$size)
    
    x <- rep(0,input$size)
    y <- rep(0,input$size)
    
    for (i in 1:input$size){
      x[i] <- sqrt(-2*log(a[i]))*cos(2*pi*b[i])
      y[i] <- sqrt(-2*log(a[i]))*sin(2*pi*b[i])
      
    }
    #par(mflow=c(2,1))
    #plot(density(c(a,b)))
    #plot(density(c(x,y)))
    plot1 <- qplot(c(a,b), geom="histogram", fill=I("blue"),main = "Generacion de numero aleatorios uniformes", col=I("red"), alpha=I(.2))
    plot2 <- qplot(c(x,y), geom="histogram", fill=I("blue"),main = "Generacion de numero aleatorios normales", col=I("red"), alpha=I(.2))
    plot_grid(plot1, plot2, ncol = 1, nrow = 2)
  })

})
