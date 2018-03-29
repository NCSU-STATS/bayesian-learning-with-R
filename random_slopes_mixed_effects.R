rm(list = ls())
setwd("d:/brucebcampbell-git/bayesian-learning-with-R")

M   <- 4
N   <- 20
age <- c(8.0, 8.5, 9.0, 9.5)
Y   <- c(47.8, 48.8, 49.0, 49.7,
         46.4, 47.3, 47.7, 48.4,
         46.3, 46.8, 47.8, 48.5,
         45.1, 45.3, 46.1, 47.2,
         47.6, 48.5, 48.9, 49.3,
         52.5, 53.2, 53.3, 53.7,
         51.2, 53.0, 54.3, 54.5,
         49.8, 50.0, 50.3, 52.7,
         48.1, 50.8, 52.3, 54.4,
         45.0, 47.0, 47.3, 48.3,
         51.2, 51.4, 51.6, 51.9,
         48.5, 49.2, 53.0, 55.5,
         52.1, 52.8, 53.7, 55.0,
         48.2, 48.9, 49.3, 49.8,
         49.6, 50.4, 51.2, 51.8,  
         50.7, 51.7, 52.7, 53.3,
         47.2, 47.7, 48.4, 49.5,
         53.3, 54.6, 55.1, 55.3,
         46.2, 47.5, 48.1, 48.4,
         46.3, 47.6, 51.3, 51.8)

Y <- matrix(Y,20,4,byrow=TRUE)
X <- cbind(1,age)

plot(NA,xlim=range(age),ylim=range(Y),xlab="Age",ylab="Bone height")
for(j in 1:N){
  lines(age,Y[j,])
  points(age,Y[j,]) 
}

RE_model <- "model{

# Likelihood
for(i in 1:N){for(j in 1:3){
Y[i,j]    ~ dnorm(meanY[i,j],taue)
meanY[i,j] <- beta[i,1] + beta[i,2]*age[j]
}}

# Random effects
for(i in 1:N){
beta[i,1:2] ~ dmnorm(mu[1:2],Omega[1:2,1:2])
}

# Priors
taue  ~ dgamma(0.1,0.1)
mu[1] ~ dnorm( 0,0.001)
mu[2] ~ dnorm( 0,0.001)

Omega[1:2,1:2] ~ dwish(R[,],2.1)
R[1,1]<-1/2.1
R[1,2]<-0
R[2,1]<-0
R[2,2]<-1/2.1

# Output the parameters of interest

for(i in 1:N){
pred[i] <- beta[i,1] + beta[i,2]*age[4]
}
}"
library(rjags)
dat    <- list(Y=Y[,1:3],age=age,N=N)
init   <- list(taue=1)
model1 <- jags.model(textConnection(RE_model),inits=init,data = dat, n.chains=1)
update(model1, 20000, progress.bar="none")

samp   <- coda.samples(model1, 
                       variable.names=c("pred"), 
                       n.iter=100000, thin=10,progress.bar="none")

post_mean <- summary(samp)$statistics[,1]
Yp        <- Y
Yp[,4]    <- post_mean

matplot(t(Y),type="l",xlab="Visit",ylab="Bone density",main="Data")
matplot(t(Y),add=TRUE,pch=19)
