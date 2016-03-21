install.packages("mvtnorm")

library(foreign)
library(mvtnorm)


# ver si un pais es democratico
# tengo muchos datos faltantes

#setwd("/Volumes/EHZ/ITAM/ESTADISTICA COMPUTACIONAL/tareafinal")
setwd("C:/Users/Ariana/Documents/MCD/2015_1/EC/EMcondicional")
fuente <- read.dta("datos_politicos.dta")

anio<- 1986
data<- fuente[fuente$year==anio,]
labels<- paste(names(data),attributes(data)$var.labels, sep=":") # NOMBRES

Y <- data$reg # INDICADORA DE SI ES ONO UNA DEMOCRACIA
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt", "fertil", "lfagric", "popg", "femsec", "EnergProd") #VARIABLES

X <- subset(data, select=list.depend)

for (j in 1:ncol(X)) X[,j] <- as.numeric(X[,j])
row.names(X) <- data$name
X.comp <- X[complete.cases(X),]

nrow(X.comp) # CON CUANTO HARIAMOS EL ESTUDIO SI NO USARAMOS IMPUTACION
View(X.comp)
View(round(cor(X.comp),2))

data.full <- data.frame(Y[complete.cases(X)],X.comp)
names(data.full)[1]<-"Y"
res <- glm(Y ~., data=data.full, family="binomial")
guess <-  round(predict(res[[rep]], type="response"))
pred.success[rep]<- sum(guess==data.full$Y)/nrows


nrow <- nrow(X)
ncols<- ncol(X)
m=5 # numero imputaciones bootstrap
tol <- 1e-3
res<- list()
imputed.sets<- list()
pred.success<- numeric(m)

for (rep in 1:m){
  #BOOTSTRAP
  print(paste("Bootstrap:", rep))
  samp <- sample(1:nrow, nrow, replace=TRUE)
  Xb <- X[samp, ]
  #INICIALIZACION
  M<- is.na(Xb)
  Sigma<- cov(Xb[complete.cases(Xb),])
  sd.vec<- sqrt(diag(Sigma))
  mu<- apply(Xb[complete.cases(Xb),],2,mean)
  for(i in 1:nrow) for(j in 1:ncols) if (M[i,j]) Xb[i,j] <- rnorm(1, mu[j], sd.vec[j])
  logv<- sum(apply(Xb,1,function(row) log(dmvnorm(row, mu, Sigma))))
  # ITERACION
  iter <-1
  repeat{
    # VALOR ACTUAL DE LA VEROSIMILITUD
    # ITERACIONES POR VARIABLES
    for(j in 1:ncol(Xb)){
      ind<- as.matrix(Xb[,j], ncols=1) # SE GUARDA COMO MATRIZ PARA PODER METER LM
      dep<-  as.matrix(Xb[,-j])
      mod<- lm(ind~dep)
      pred<- predict(mod)
      Xb[M[,j],j]<- pred[M[,j]] 
    }
    # nueva matriz de var y covar
    Sigma<- cov(Xb)
    mu<- apply(Xb,2,mean)
    logv[iter+1]<- sum(apply(Xb,1,function(row) log(dmvnorm(row, mu, Sigma))))
    if (abs(logv[iter+1]-logv[iter])<tol) break
    iter <- iter +1
  }
  print(paste("   -iteraciones totales:", iter))
  imputed.sets[[rep]] <- Xb
  # GRAFICA
  plot(logv[-(1:3)], type="l", col="blue", main=paste("Bootstrap", rep))
  # MODELO
  data.full<- data.frame(Y[samp], Xb)
  names(data.full)[1]<- "Y"
  res[[rep]] <- glm(Y~ ., data=data.full, family="binomial") #regresion log
  guess <- round(predict(res[[rep]], type="response"))
  pred.success[rep]<- sum(guess==data.full$Y)/nrow
}


#antes esta la parte de modelo
#POOLING
beta.all <- matrix(0, nrow=ncols, ncol=m)
for (j in 1:m){
  beta.all[,rep] <- coef(res[[rep]])[-1]
}
#PROMEDIO DE LAS BETAS
beta.estims <- apply(beta.all,2,mean)

#ESTIMACION DE LAS VARIANZAS
beta.var.within <- numeric(ncols)
for (rep in 1:m){ 
beta.var.within <- beta.var.within + (summary(res[[rep]])$coefficients[,2][-1])^2/m
}
beta.var.between <- apply(beta.all,1,var)
beta.var <- beta.var.within + (1+1/m)*beta.var.between

#Z-VALUES FINALES
table <-data.frame(beta=beta.estims, sd=sqrt(beta.var))
round(table,5)