
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
#install.packages('ggplot2')
library(ggplot2)
library(cowplot)
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("BoxMuller"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("size",
                  "Selecciona el tamano:",
                  min = 1,
                  max = 1000,
                  value = 10)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
