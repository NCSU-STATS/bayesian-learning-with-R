rm(list = ls())
setwd("c:/e/brucebcampbell-git/bayesian-learning-with-R")
library(latex2exp)   
library(pander)
library(ggplot2)
library(GGally)
library(rjags)
library(coda)
library(modeest)
nSamples <- 1000
n.chains <- 1
load("E2.RData")

dim(Y1)

cor(Y1, use = "pairwise.complete.obs")
N <- nrow(Y1)
p = 6
Y1.scaled <- scale(Y1)

model_string <- "model{

  # Likelihood for theta[1,1:p]:
  #theta[1,1:p] ~ dmnorm(mu[1,1:p],precision1)
  
  #Prior for theta[1,1:p]
  #mu[1:1,p] ~ ?
  
  #for(i in 2:N)
  for(i in 2:N)
  {
    theta[i,1:p]   ~ dnorm(mu[i,1:p] ,precision2)
    mu[i,1:p] <- mu2[1:p] + rho * Y1[i-1,1:p]
  }
  
  # Prior for likelihood parameters: mu2, precision2, rho
  rho ~ dunif(-1,1)
  for(j in 1:p)
  {
    mu2[j] ~ dnorm(0,0.01)
  }
  
  precision2[1:p,1:p]~dwish(R[,],k)

  # Missing data model for Y1
  for(i in 1:N)
  {
    Y1[i,1:p]~dmnorm(x_mn[],x_prec[,])
  }

  # Priors for missing-data model parameters
  for(j in 1:p)
  {
    x_mn[j]~dnorm(0,0.01)
  }
  x_prec[1:p,1:p]~dwish(R[,],k)
  x_cov[1:p,1:p]<-inverse(x_prec[,])

  k<-p+0.1
  for(j1 in 1:p)
  {
    for(j2 in 1:p)
    {
      R[j1,j2]<-0.1*equals(j1,j2)
    }
  }
  
}"



mlr_model  <-  function(){
  for(i in 2:N)
  {
    theta[i,1:p] ~ dnorm(mu[i,1:p] ,precision2)
    mu[i,1:p]  <-  mu2[1:p] + rho * Y1[i-1,1:p]
  }
  
  # Prior for likelihood parameters: mu2, precision2, rho
  rho  ~  dunif(-1,1)
  
  for(j in 1:p)
  {
    mu2[j]  ~  dnorm(0,0.01)
  }
  
  precision2[1:p,1:p] ~ dwish(R[,],k)
  
  # Missing data model for Y1
  for(i in 1:N)
  {
    Y1[i,1:p] ~ dmnorm(x_mn[],x_prec[,])
  }
  
  # Priors for missing-data model parameters
  for(j in 1:p)
  {
    x_mn[j] ~ dnorm(0,0.01)
  }
  x_prec[1:p,1:p] ~ dwish(R[,],k)
  x_cov[1:p,1:p] <- inverse(x_prec[,])
  
  k <- p+0.1
  for(j1 in 1:p)
  {
    for(j2 in 1:p)
    {
      R[j1,j2] <- 0.1*equals(j1,j2)
    }
  }
}

library(R2OpenBUGS)

stacks_dat <- list(Y1=Y1.scaled,N=N, p = p)

mlr_inits <- function() {
  list(intercept = mean(stacks_dat$Y1,na.rm=T), tau = 0.01)
}

samps <- bugs(data = stacks_dat, 
              inits = mlr_inits, 
              parameters.to.save = c("theta", "rho", "x_mn","x_cov"), 
              model.file =mlr_model, 
              n.chains = 3, n.burnin=5000, n.iter = 20000, n.thin=10, DIC=F,debug=TRUE)

plot(samps)
