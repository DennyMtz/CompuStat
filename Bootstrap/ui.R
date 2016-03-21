
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Correccion de sesgo mediante Bootstrap"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("n_rep",
                  "No de pruebas",
                  min = 500,
                  max = 5000,
                  value = 1000),
      
      sliderInput("va",
                  "Elementos de la muestra",
                  min = 100,
                  max = 1000,
                  value = 100),
      
      sliderInput("n_bootstrap",
                  "Numero de muestras bootstrap con MC",
                  min = 0,
                  max = 1000,
                  value = 100)
    ),

    mainPanel(
      p("Se muestra la diferencia entre obtener el estimador (media) directamente de la muestra o utilizando",
      strong("bootstrap"),
      " para reducir el sesgo."),
      plotOutput("distPlot"),
      p("theta n es el estimador directo de la muestra realizando varias pruebas, y bias star es el estimador utilizando bootstrap con Monte Carlo."),
      tableOutput("view"),
      
      p("Se puede ver como el Error Medio Cuadratico (EMC) se redujo hasta 6 veces.")
    )
  )
))
