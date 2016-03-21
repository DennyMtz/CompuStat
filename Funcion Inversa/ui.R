
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Tarea 1 - Funcion Inversa"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda",
                  "Seleccionar parametro Lambda:",
                  min = 0.00000000000001,
                  max = 10,
                  value = 1
                  )
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("view")
    )
  )
))
