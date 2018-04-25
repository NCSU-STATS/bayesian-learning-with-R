rm(list=ls())

setwd("c:/e/brucebcampbell-git/bayesian-learning-with-R")
load("OzoneJuly2005.RData")
ls()
## [1] "s" "Y"
dim(Y)
## [1] 1106   31
dim(s)
## [1] 1106    2
ns   <- nrow(Y)
nt   <- ncol(Y)
Ybar <- mean(Y,na.rm=TRUE)

plot(s,axes=FALSE,xlab="",ylab="",main="Monitor locations")
boxplot(Y,xlab="Day (in July 2005)",ylab="Ozone (ppb)",main="Ozone by day")
abline(75,0,col=2)
boxplot(t(Y),xlab="Station",ylab="Ozone (ppb)",main="Ozone by site")
abline(75,0,col=2)


Ozone_model <- "model{

# Likelihood
for(i in 1:ns){for(j in 1:nt){
Y[i,j]    ~ dnorm(mean[i,j],taue)
mean[i,j] <- mu + alpha[i] + gamma[j]
}}

# Random effects
for(i in 1:ns){
alpha[i] ~ dnorm(0,taus)
}
for(j in 1:nt){
gamma[j] ~ dnorm(0,taut)
}

# Priors
mu   ~ dnorm(0,0.01)
taue ~ dgamma(0.1,0.1)
taus ~ dgamma(0.1,0.1)
taut ~ dgamma(0.1,0.1)

# Output the parameters of interest
sigma2[1] <- 1/taue
sigma2[2] <- 1/taus
sigma2[3] <- 1/taut
sigma[1]  <- 1/sqrt(taue)
sigma[2]  <- 1/sqrt(taus)
sigma[3]  <- 1/sqrt(taut)
pct[1]    <- sigma2[1]/sum(sigma2[])   
pct[2]    <- sigma2[2]/sum(sigma2[])   
pct[3]    <- sigma2[3]/sum(sigma2[])   

}"
Fit the model
library(rjags)
dat    <- list(Y=Y,ns=ns,nt=nt)
init   <- list(mu=Ybar)
model1 <- jags.model(textConnection(Ozone_model),inits=init,data = dat, n.chains=1)
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
##    Graph Size: 69733
## 
## Initializing model
update(model1, 10000, progress.bar="none")

samp   <- coda.samples(model1, 
                       variable.names=c("sigma","pct","gamma"), 
                       n.iter=20000, progress.bar="none")

plot(samp)