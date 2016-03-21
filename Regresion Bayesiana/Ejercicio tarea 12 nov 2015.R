#library(ggplot2)

data(iris)
mod <- lm(iris$Sepal.Length ~ iris$Sepal.Width+iris$Petal.Length+iris$Petal.Width)
summary(mod)
plot(mod)

# #A MANO:
# X <- as.matrix(cbind(1,iris[,2:4]))
# Y <- IRIS[,1]
# beta.hat <- solve(t(X)%*% X, t(X) %*% Y)
# pred <- X %*% beta.hat
# residuals <- Y - pred
# hist(residuals)
# qqnorm(residuals)
# plot(residuals,pred)
# plot(Y,pred)
# SS <- sqrt(sum(Y-pred)^2)/(150-4)
# cov.betas <- (SS^2)*solve(t(X) %*% X)
# sqrt(diag(cov.betas))
########################################################################################


library(Rcpp)
data(iris)

vec <- rep(1,150) #Genero el vector de 1 para B0

# BAYESIAN APPROACH
data <- matrix(cbind(vec,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width,iris$Sepal.Length),ncol=5)

#Elegi los valores un poco en funcion del approach frecuentista!
prior.mean1 <- function(x) dnorm(x, 2, 3)
prior.mean2 <- function(x) dnorm(x, 1, 3)
prior.mean3 <- function(x) dnorm(x, 1, 3)
prior.mean4 <- function(x) dnorm(x, 0, 3)
prior.sd <- function(x) dgamma(x, 5, 40)

# WE WILL USE AND MCMC METHOD.
# NEED 
# 1: A objective density: 2) a proposal density
# Recall obj ~~ L(theta|X)prior(X)
# But as we want logarithms, we have log(obj) = log(L) + log(prior)

# 1)
cppFunction('
            double objdens(NumericMatrix data, NumericVector theta){
            double lkh, logprior, mu;
            int m=data.nrow();
            NumericVector Y(m);
            NumericMatrix X(m,theta.size());
            Y = data(_,4); // In this example is redundant but helps to generalise
            X = data;

            // Compute loglikelihood
            
            lkh=0.0;
            for (int i=0; i<150; i++){
            mu=0.0;
            for (int j=0; j<4; j++){
            mu += theta[j]*X(i,j);
            }
            lkh += -.5*pow((Y[i]-mu)/theta[4],2)-log(theta[4]);
            //printf("L:%f,m:%f","mu",lkh);            
            }
            
            // Compute logprior
            logprior = R::dnorm(theta[0], 2.0,3.0, true) + R::dnorm(theta[1], 1.0,3.0, true) + R::dnorm(theta[2], 1.0,3.0, true) 
            + R::dnorm(theta[3], 0.0,3.0, true)+ R::dgamma(theta[4], 5.0, 1.0/40.0, true);
            // Log of target density
            return lkh + logprior;
            }')

objdens(data,c(2,2,2,2,3))


# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
            NumericVector proposal(NumericVector theta){
            int nparam = theta.size();
            double jump = .15; 
            NumericVector newtheta(nparam);
            for (int i=0; i<nparam; i++){
            newtheta[i] = R::rnorm(theta[i], jump);
            }
            return newtheta;
            }')
proposal(c(2,2,2,2,3))


# 3) METROPOLIS

#source("BayesianMH.cpp")

nsim <- 1000
init <- c(1,3,3,3,1)
MHBayes(500, init, objdens, proposal, data)
mh.samp <- MHBayes(nsim, init, objdens, proposal, data)
estims <- mh.samp$theta

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:

#install.packages('calibrate')
library(calibrate)
pts <- seq(1,100,by=5)
plot(estims[pts, ], type="l", xlab="mean", ylab="sd")
textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)

### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")

### 2) AUTOCORRELATION
acf(estims[ , 1])
acf(estims[ , 2]) # WARNING HERE!
acf(estims[ , 3])
acf(estims[ , 4])
acf(estims[ , 5])

# burnin and subsampling
burnin <- 100
estim <- estims[-(1:burnin), ]
thinning <- .9 # meaning we'll keep 75% of observations to reduce autocorrelation

# OBS: thinning is rarely usefull!!!! check that nothing changes
sub <- sample.int(nsim-burnin, size=round(thinning*nsim))
estims <- estims[sub, ]
acf(estims[ , 1])
acf(estims[ , 2]) 
acf(estims[ , 3])
acf(estims[ , 4])
acf(estims[ , 5])

# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE
hist(estims[ ,1], prob=TRUE, breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the B0") # posterior distribution of B0
#plot(prior.mean, col="darkred", lwd="2", add=TRUE)
lines(density(estims[ ,1]), col="darkblue", lwd="2")

hist(estims[ ,2], prob=TRUE, breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the B1") # posterior distribution of B1
plot(prior.mean1, col="darkred", lwd="2",  add=TRUE)
lines(density(estims[ ,2]), col="darkblue", lwd="2")

hist(estims[ ,3], prob=TRUE, breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the B2") # posterior distribution of B2
plot(prior.mean2, col="darkred", lwd="2", add=TRUE)
lines(density(estims[ ,3]), col="darkblue", lwd="2")

hist(estims[ ,4], prob=TRUE, breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the B3") # posterior distribution of B3
plot(prior.mean3, col="darkred", lwd="2",  add=TRUE)
lines(density(estims[ ,4]), col="darkblue", lwd="2")

hist(estims[ ,5], prob=TRUE, breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of S.D.
plot(prior.sd, col="darkred", lwd="2",  add=TRUE)
lines(density(estims[ ,5]), col="darkblue", lwd="2")


mean(estims[ ,1]) # approx. mean-value of the posterior of B0
mean(estims[ ,2]) # approx. mean-value of the posterior of B1
mean(estims[ ,3]) # approx. mean-value of the posterior of B2
mean(estims[ ,4]) # approx. mean-value of the posterior of B3
mean(estims[ ,5]) # approx. mean-value of the posterior of standard deviation







# # CERTAINTY INTERVALS
# alpha <- .05
# intervals3 <- quantile(estims[ ,1], c(alpha/2, 1-alpha/2))
# intervals3
# quantile(estims[ ,2], c(alpha/2, 1-alpha/2)) # ALSO FOR SSSDDDD
# 
# # COMPARISON OF ALL RESULTS
# 
# data(iris)
# samp <- iris$Sepal.Width
# hist(samp)
# N <- length(samp)
# 
# est.mean <- sum(samp)/N # mean(samp)
# est.sd <- sqrt(sum((samp-est.mean)^2)/(N-1)) # var(samp)
# 
# meanestims <- c(est.mean, params[1], mean(estims[ ,1]))
# sdestims <- c(est.sd, params[2], mean(estims[ ,2]))
# intmeanlow <- c(intervals[1], intervals2[1], intervals3[1])
# intmeanhigh <- c(intervals[2], intervals2[2], intervals3[2])
# Comparison <- data.frame(meanestims, sdestims, intmeanlow, intmeanhigh)
# row.names(Comparison) <- c("Pivot", "Likelihood", "Bayesian")
# Comparison