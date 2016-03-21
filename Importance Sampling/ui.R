# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Importance Sampling"),
  
  sidebarPanel(
    sliderInput("nsim",
                "Numero de v.a.",
                min = 1000,
                max = 1000000,
                value = 10000),
    
    
    
    sliderInput("tam_muestras",
                "Tamano muestra",
                min = 100,
                max = 10000,
                value = 5000),
  
    
    sliderInput("lambda2",
                "Valor de Lambda fun obj:",
                min = 0.1,
                max = 5,
                value = 2)
    
    
    #sliderInput("a",
    #            "alpha:",
    #            min = 0.5,
    #            max = 5,
    #            value = 1),
    
    #sliderInput("b",
    #            "beta:",
    #            min = 0.5,
    #            max = 5,
    #            value = 3.0)
    
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot1"),
    plotOutput("distPlot2"),
    plotOutput("distPlot3"),
    tableOutput("view")
  )
  
  
))