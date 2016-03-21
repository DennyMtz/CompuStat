
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plyr)

shinyServer(function(input, output) {
  #va <- 1000
  #tam_muestra <- 100
  #n_muestras <- 100
  
  g <- function(mu) { exp(mu) }
  
  fun <- function(no_va,n_rep,n_bootstrap) {
    
    rep <- n_rep
    n_muestras <- n_bootstrap
    #no_va <- 100
    
    theta_0 <- 1
    bias_star <- c()
    theta_n <- c()
    for(i in 1:rep)
    {
      X <- rnorm(no_va,0,6)
      mu <- mean(X)
      theta_n[i] <- g(mu)
      #samples<-list()
      theta_star <- c()
      for(j in 1:n_muestras)
      {
        Y <- sample(X, no_va, replace=TRUE)
        #no se guarda el resampleo, no se considera necesario
        #samples <- append(samples, Y)
        theta_star[j] <- g(mean(Y))
      }
      bias_star[i] <- mean(theta_star)-theta_n[i]
    }
    
    #se regresan los arreglos con los datos, los calculos se hacen en las diferentes funciones
    return (data.frame(theta_n, bias_star))
  }
  
  output$distPlot <- renderPlot({
    datos <- fun(input$va,input$n_rep,input$n_bootstrap)
    
    theta_0 <- 1
    
    n <- data.frame(datos$theta_n - theta_0, "theta n")
    b <- data.frame(datos$theta_n - datos$bias_star - theta_0, "bias star")
    
    colnames(n) <- c('sesgo', 'class')
    colnames(b) <- c('sesgo', 'class')
    
    #se combino el dataframe en uno solo para poder graficar mediante la columna class
    data <- rbind(n,b)
    
    #calcular la media para los vline
    mu <- ddply(data, "class", summarise, grp.mean=mean(sesgo))
    
    h1 <- ggplot(data, aes(x=sesgo, color=class)) + geom_density(alpha = 0.2, fill="blue") 
    h1 <- h1 + geom_vline(data=mu, aes(xintercept=grp.mean, color=class), linetype="dashed")
    h1 <- h1 + theme(legend.position="top")
    
    print(h1)
  })

  
  output$view <- renderTable({
    res <- fun(input$va,input$n_rep,input$n_bootstrap)
    
    theta_0 <- 1
    
    estimador_original <- mean(res$theta_n)
    estimador_bootstrap <- mean(res$theta_n - res$bias_star)
    
    sesgo_original <- mean(res$theta_n - theta_0)
    sesgo_bootstrap <- mean(res$theta_n - res$bias_star - theta_0)
    
    mse_original <- mean((res$theta_n - theta_0)^2)
    mse_bootstrap <- mean(res$theta_n - res$bias_star - theta_0)^2
    
    data <- (data.frame(estimador_original, estimador_bootstrap,sesgo_original,sesgo_bootstrap,mse_original,mse_bootstrap))
    colnames(data) <- c('Estimador original', 'Estimador bootstrap','Sesgo original', 'Sesgo bootstrap', 'EMC original', 'EMC bootstrap')
    
    options(digits=4)
    
    data
    
   })
  
})
