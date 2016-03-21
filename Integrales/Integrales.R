#Primer integral
n1=10000
alfa=0.95

montecarlo <- function(n1,alfa){
  x1 <- runif(n1,0,2)
  fun <- 2*sqrt(4-x1^2)
  mediaX1 <- mean(fun)
  varX1 <- var(fun)
  z1 <- qnorm(alfa,lower.tail=FALSE)
  limsup <- (mediaX1) - z1*sqrt(varX1/n1)
  liminf <- (mediaX1) + z1*sqrt(varX1/n1)
  
  return(data.frame(mediaX1,limsup,liminf)) 
}

montecarlo(10000,0.95)


res<-data.frame()
vec<-seq(100,10000,by=10)
for (i in vec){
  res <- rbind(res,montecarlo(i,.95))
}

plot(res$mediaX1,type="l")
lines(res$liminf)
lines(res$limsup)
abline(h=pi)

#Segunda integral
n2=10000
alfa=0.95

montecarlo2 <- function(n2,alfa){
  x2 <- runif(n2,0,1)
  fun2 <- 4/(1+x2^2)
  mediaX2 <- mean(fun2)
  varX2 <- var(fun2)
  z2 <- qnorm(alfa,lower.tail=FALSE)
  limsup2 <- (mediaX2) - z2*sqrt(varX2/n2)
  liminf2 <- (mediaX2) + z2*sqrt(varX2/n2)
  
  return(data.frame(mediaX2,limsup2,liminf2)) 
}

montecarlo2(10000,0.95)

res2<-data.frame()
vec2<-seq(100,10000,by=10)
for (i in vec2){
  res2 <- rbind(res2,montecarlo2(i,.95))
}

plot(res2$mediaX2,type="l")
lines(res2$liminf2)
lines(res2$limsup2)
abline(h=pi)


#Tercera integral
n3=10000
alfa=0.95

montecarlo3 <- function(n3,alfa){
  x3 <- runif(n3,0,1)
  fun3 <- 6/sqrt(4-x3^2)
  mediaX3 <- mean(fun3)
  varX3 <- var(fun3)
  z3 <- qnorm(alfa,lower.tail=FALSE)
  limsup3 <- (mediaX3) - z3*sqrt(varX3/n3)
  liminf3 <- (mediaX3) + z3*sqrt(varX3/n3)
  
  return(data.frame(mediaX3,limsup3,liminf3)) 
}

montecarlo3(10000,0.95)

res3<-data.frame()
vec3<-seq(100,10000,by=10)
for (i in vec3){
  res3 <- rbind(res3,montecarlo3(i,.95))
}

plot(res3$mediaX3,type="l")
lines(res3$liminf3)
lines(res3$limsup3)
abline(h=pi)



#Distribución Normal

n4=100
alfa=0.95

montecarlo_normal <- function(n4,alfa){
  x4 <- runif(n4)
  fun4 <- ((1/(sqrt(2*pi)))*exp((-1/2)*(x4^2)))
  mediaX4 <- mean(fun4<2)
  varX4 <- var(fun4)
  z4 <- qnorm(alfa,lower.tail=FALSE)
  limsup4 <- (mediaX4) - z4*sqrt(varX4/n4)
  liminf4 <- (mediaX4) + z4*sqrt(varX4/n4)
  
  return(data.frame(mediaX4,limsup4,liminf4)) 
}

montecarlo_normal(10000,0.95)

res4<-data.frame()
vec4<-seq(100,10000,by=10)
for (i in vec4){
  res4 <- rbind(res4,montecarlo_normal(i,.95))
}

plot(res4$mediaX4,type="l")
lines(res4$liminf4)
lines(res4$limsup4)
abline(h=1)

erf<- (pnorm(2)-pnorm(-2))^n4
print(erf)

#trapecio <- function(N,fun,a,b){
#  pnts <- seq(from=a,to=b,by=(b-a/N))
#  integral<-0
#  for(i in 1:N)
#}

#integral <- integral+(fun(pnts(i)+fun))
