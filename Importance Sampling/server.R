# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyServer(function(input, output) {
  
  output$distPlot1 <- renderPlot({
    
    Fun1 <- function (nsim,lambda1,lambda2,a,b){
      
      u <- runif(nsim)
      c <- (1/lambda1)*log(1/(1-(1-exp(-2*lambda1))*u))
      #Funcion objetivo:
      f_obj <- function(x){
        (lambda2*exp(-lambda2*x))
      }
      #Importance weight
      w <- function(x){
        dexp(x,lambda1)/(1-exp(-2*lambda1))
      }
      #Para calcular la integral
      f_obj2<- function(x){
        (f_obj(x)/w(x))
      }
      #Intervalos Importance Sampling Exponencial
      alpha=0.05
      estim1<- mean(f_obj2(c))
      S2_1 <-var(f_obj2(c))
      quant1 <- qnorm(alpha/2,lower.tail=FALSE)
      int.upper1 <- estim1 + sqrt(S2_1/input$nsim)*quant1
      int.lower1 <- estim1 - sqrt(S2_1/input$nsim)*quant1
      
      #MONTECARLO
      un <- runif(nsim,0,2)
      d <- function(u){
        (lambda2*exp(-lambda2*u))*2
      }
      #Intervalos Monte Carlo Crudo
      estim2<- mean(d(un))
      S2_2 <-var(d(un))
      quant2 <- qnorm(alpha/2,lower.tail=FALSE)
      int.upper2 <- estim2 + sqrt(S2_2/nsim)*quant2
      int.lower2 <- estim2 - sqrt(S2_2/nsim)*quant2
      
      #BETA
      y <- rbeta(nsim,a,b)
      f_objb <- function(x){
        lambda1*exp(-lambda1*x)
      }
      w2 <- function(x){
        dexp(x,lambda2)/(dbeta(x,a,b,ncp = 0,log = FALSE))
      }
      f_objb2 <- function(x){
        (f_objb(x)/w2(x))
      }
      #Intervalos Importance Sampling Beta
      estim3<- mean(f_objb2(y))
      S2_3 <-var(f_objb2(y))
      quant3 <- qnorm(alpha/2,a,b)
      int.upper3 <- estim3 + sqrt(S2_3/nsim)*quant3
      int.lower3 <- estim3 - sqrt(S2_3/nsim)*quant3
      
      return(data.frame(estim1,int.lower1,int.upper1,estim2,int.lower2,int.upper2,estim3,int.upper3,int.lower3))
    }
    
    #Fun1(input$nsim,1,input$lambda2,1,2.5)
    
    
    
    resultado <- data.frame()
    vec <- seq(1,input$tam_muestras,10)
    for(i in vec){
      resultado <- rbind(resultado,Fun1(i,1,input$lambda2,1,2.5))
    }
    
    
    plot(resultado$estim1, type="l", col="violetred3", ylim=(c(0.9,1.1)),main = "Imp Sampl Exp",lwd=2,ylab = "Media", xlab = "tamano muestra")
    lines(resultado$int.lower1, type="l", col ="darkgray",lty=1)
    lines(resultado$int.upper1, type="l", col ="darkgray",lty=1)
    abline(a=NULL,b=NULL,h=mean(resultado$estim1), col="black")
    
    output$distPlot2 <- renderPlot({
      plot(resultado$estim2, type="l", col="violetred3",ylim=(c(0.9,1.1)),main = "Monte Carlo Crudo",lwd=2,ylab = "Media",xlab = "tamano muestra")
      lines(resultado$int.lower2, type="l", col ="darkgray",lty=1)
      lines(resultado$int.upper2, type="l", col ="darkgray",lty=1)
      abline(a=NULL,b=NULL,h=mean(resultado$estim2), col="black")
    })
    
    output$distPlot3 <- renderPlot({
      plot(resultado$estim3, type="l", col="violetred3",ylim=(c(0.9,1.1)), main = "Imp Sampl Beta",lwd=2,ylab = "Media",xlab = "tamano muestra")
      lines(resultado$int.lower3, type="l", col ="darkgray",lty=1)
      lines(resultado$int.upper3, type="l", col ="darkgray",lty=1)
      abline(a=NULL,b=NULL,h=mean(resultado$estim3), col="black")
    })
    
    output$view <- renderTable({
      data <- as.data.frame(cbind((resultado$estim1),(resultado$int.lower1),(resultado$int.upper1),(resultado$estim2),(resultado$int.lower2),(resultado$int.upper2),(resultado$estim3),(resultado$int.lower3),(resultado$int.upper3)))  
      
      names(data)<- c("Exponencial M","LInf Exp","LSup Exp","MC Crudo","LInf MCC","LSup MCC","Beta","LInf Beta","LSup Beta")
      
      data})
  })
  
})