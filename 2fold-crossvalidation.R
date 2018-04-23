
rm(list = ls())
setwd("c:/brucebcampbell-git/bayesian-learning-with-R")
library(kable)
load("election_2008_2016.RData")

junk <- is.na(Y+rowSums(X))
Y    <- Y[!junk]
X    <- X[!junk,]
n    <- length(Y)
p    <- ncol(X)
test    <- rep(1:2,n)[1:n]

Ytrain  <- Y[test==1]
Xtrain  <- X[test==1,]

Ytest   <- Y[test==2]
Xtest   <- X[test==2,]

ntrain  <- length(Ytrain)
ntest   <- length(Ytest)
model_string1 <- "model{

# Likelihood
for(i in 1:ntrain){
Ytrain[i]  ~ dnorm(mutrain[i],inv.var)
mutrain[i] <- alpha + inprod(Xtrain[i,],beta[])
}

#Prediction
for(i in 1:ntest){
Ytest[i]  ~ dnorm(mutest[i],inv.var)
mutest[i] <- alpha + inprod(Xtest[i,],beta[])
}

#Priors
for(j in 1:p){
beta[j] ~ dnorm(0,0.00001)
}
alpha   ~ dnorm(0,0.00001)
inv.var ~ dgamma(0.01,0.01)
}"

 # (2) Bayesian lasso
 model_string2 <- "model{
 
 # Likelihood
 for(i in 1:ntrain){
 Ytrain[i]  ~ dnorm(mutrain[i],inv.var)
 mutrain[i] <- alpha + inprod(Xtrain[i,],beta[])
 }
 
 #Prediction
 for(i in 1:ntest){
 Ytest[i]  ~ dnorm(mutest[i],inv.var)
 mutest[i] <- alpha + inprod(Xtest[i,],beta[])
 }
 
 #Priors
 for(j in 1:p){
 beta[j] ~ ddexp(0,inv.var.b)
 }
 alpha     ~ dnorm(0,0.00001)
 inv.var   ~ dgamma(0.01,0.01)
 inv.var.b ~ dgamma(0.01,0.01)
 }"
 
 library(rjags)
 
 model1 <- jags.model(textConnection(model_string1), 
                      data = list(Ytrain=Ytrain,Xtrain=Xtrain,ntrain=ntrain,
                                  Xtest=Xtest,ntest=ntest,p=p),
                      quiet=TRUE)
 update(model1, 10000, progress.bar="none")
 samps1  <- coda.samples(model1, 
                         variable.names=c("Ytest"), 
                         n.iter=20000, progress.bar="none")
 Ytest1  <- samps1[[1]]
 
 
 model2 <- jags.model(textConnection(model_string2), 
                      data = list(Ytrain=Ytrain,Xtrain=Xtrain,ntrain=ntrain,
                                  Xtest=Xtest,ntest=ntest,p=p),
                      quiet=TRUE)
 update(model2, 10000, progress.bar="none")
 samps2  <- coda.samples(model2, 
                         variable.names=c("Ytest"), 
                         n.iter=20000, progress.bar="none")
 Ytest2  <- samps2[[1]]
 
 
 post_mn1   <- apply(Ytest1,2,mean)
 post_sd1   <- apply(Ytest1,2,sd)
 post_low1  <- apply(Ytest1,2,quantile,0.05)
 post_high1 <- apply(Ytest1,2,quantile,0.95)
 
 post_mn2   <- apply(Ytest2,2,mean)
 post_sd2   <- apply(Ytest2,2,sd)
 post_low2  <- apply(Ytest2,2,quantile,0.05)
 post_high2 <- apply(Ytest2,2,quantile,0.95)
 
 MSE1   <- mean((post_mn1-Ytest)^2) #For every test element we have a chain. How far n average is the squared distance of posterior mean of that to the actual test value?
 BIAS1  <- mean(post_mn1-Ytest)
 AVESD1 <- mean(post_sd1)
 COV1   <- mean(Ytest>post_low1 & Ytest<post_high1)
 
 MSE2   <- mean((post_mn2-Ytest)^2)
 BIAS2  <- mean(post_mn2-Ytest)
 AVESD2 <- mean(post_sd2)
 COV2   <- mean(Ytest>post_low2 & Ytest<post_high2)
 
 MSE    <- c(MSE1,MSE2)
 BIAS   <- c(BIAS1,BIAS2)
 AVESD  <- c(AVESD1,AVESD2)
 COV90  <- c(COV1,COV2)
 
 OUTPUT <- cbind(MSE,BIAS,AVESD,COV90)
 rownames(OUTPUT) <- c("Gaussian","BLASSO")
 
 kable(OUTPUT,digits=2)