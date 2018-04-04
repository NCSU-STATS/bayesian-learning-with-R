rm(list = ls())
setwd("d:/brucebcampbell-git/bayesian-learning-with-R")
library(latex2exp)   
library(pander)
library(ggplot2)
library(GGally)

library(R2OpenBUGS)
library(rjags)
library(coda)
library(MASS)
library(R2jags)
library(modeest)

nSamples <- 2000
n.chains <- 2

load("E2.RData")


dim(Y1)
cor.y1 <- cor(Y1, use = "pairwise.complete.obs")
cov.y1  <- cov(Y1, use = "pairwise.complete.obs")
N <- nrow(Y1)
p = 6
Y1.scaled <- scale(Y1)
library("Matrix")

sig <- nearPD(cov.y1)
T = 365
Sigma = sig$mat 
Phi = .6
A = matrix(1:p, p, 1)
y = matrix(NA, T,p)
y[1,] = A
for(t in 2:T) 
{
  y[t,] = mvrnorm(1, A + Phi * y[t-1,], Sigma)
}

# Jags code to fit the model to the simulated data
model_code = '
model
{
  # Likelihood
  for (t in 2:T) 
  {
    y[t, ] ~ dmnorm(mu[t, ], Sigma.Inv)
    mu[t, 1:p] <- A + Phi * y[t-1,]
  }
  Sigma.Inv ~ dwish(I, p+1)
  Sigma <- inverse(Sigma.Inv)  
  
  # Priors
  Phi ~ dunif(-1, 1)
  for(i in 1:p) 
  {
    A[i] ~ dnorm(0, 0.01)
  }

  # Missing data model for y
  #for(i in 1:T)
  #{
    #y[i,1:p]~dmnorm(x_mn[],x_prec[,])
  #}
  
  # Priors for missing-data model parameters
  for(j in 1:p)
  {
    x_mn[j]~dnorm(0,0.01)
  }
  x_prec[1:p,1:p]~dwish(R[,],k)
  x_cov[1:p,1:p]<-inverse(x_prec[,])
  k <- p+0.1
  for(j1 in 1:p)
  {
    for(j2 in 1:p)
    {
      R[j1,j2] <- 0.1*equals(j1,j2)
    }     
  }
}
'
# Set up the data
model_data = list(T = T, p = p, y = y, I = diag(p))
# Choose the parameters to watch
model_parameters =  c("A", "Phi", "Sigma")

model <- jags.model(textConnection(model_code),data = model_data,n.chains = n.chains)#Compile Model Graph

update(model, nSamples, progress.bar="none"); # Burnin
out.coda  <- coda.samples(model, variable.names=model_parameters,n.iter=2*nSamples) 

plot(out.coda )

if(n.chains > 1)
{
  gelman.srf <-gelman.diag(out.coda)
  plot(gelman.srf$psrf,main = "Gelman Diagnostic")
}

chains.ess <- lapply(out.coda,effectiveSize)
first.chain.ess <- chains.ess[1]
plot(unlist(first.chain.ess), main="Effective Sample Size")

chain <- out.coda[[1]]
posterior.means <- list()
posterior.modes <- list()
for( i in 1:length(colnames(chain)) )
{  
  colname <- colnames(chain)[i]
  print(colname)
  samples <- chain[,i]
  posterior.means[colname] <-mean(samples)
  posterior.modes[colname] <-mlv(samples)$M
}

##################END JAGS PORTION

#NOW DO THE MISSING DATA VERSION

mlr_model2 <- function(){
  
  # Likelihood
  for (t in 2:N) {
    theta[t, 1:p] ~ dmnorm(y[t, 1:p], Sigma.Inv[,])
    #y[t, 1:p] ~ dmnorm(mu[t, 1:p], Sigma.Inv[,])
    #mu[t, 1:p] <- A[1:p] +  y[t-1,1:p]
  }
  Sigma.Inv ~ dwish(I, p+1)
  
  Sigma <- inverse(Sigma.Inv[,])  
  
  # Priors
  Phi ~ dunif(-1, 1)
  for(i in 1:p) 
  {
    A[i] ~ dnorm(0, 0.01)
  }
  
  # Missing data model for y
  for(i in 1:N){
    y[i,1:p]~dmnorm(x_mn[1:p],x_prec[,])
  }
  
  # Priors for missing-data model parameters
  for(j in 1:p){
    x_mn[j]~dnorm(0,0.01)
  }
  x_prec[1:p,1:p]~dwish(R[,],k)
  x_cov[1:p,1:p]<-inverse(x_prec[,])
  
  k <- p+0.1
  for(j1 in 1:p)
  {
    for(j2 in 1:p)
    {
      R[j1,j2] <- 0.1*equals(j1,j2)
    }
  }
}


stacks_dat <- list(y=y,p = 6,   N = 365,I = diag(p))
mlr_inits <- function() {  list( Phi = 0.00)}

samps <- bugs(data = stacks_dat, 
              inits = mlr_inits, 
              parameters.to.save =  c("A","Phi","Sigma.Inv","y","theta"), 
              model.file = mlr_model2, 
              codaPkg = TRUE,
              n.chains = 1, n.burnin=100, n.iter = 200, DIC=F)

out.coda <- read.bugs(samps) 

if(n.chains > 1)
{
  gelman.srf <-gelman.diag(out.coda)
  plot(gelman.srf$psrf,main = "Gelman Diagnostic")
}

chains.ess <- lapply(out.coda,effectiveSize)
first.chain.ess <- chains.ess[1]
plot(unlist(first.chain.ess), main="Effective Sample Size")

chain <- out.coda[[1]]
posterior.means <- list()
posterior.modes <- list()
for( i in 1:length(colnames(chain)) )
{  
  colname <- colnames(chain)[i]
  print(colname)
  samples <- chain[,i]
  posterior.means[colname] <-mean(samples)
  posterior.modes[colname] <-mlv(samples)$M
}









































x <- scale(y)
mlr_model2 <- function(){
  for(i in 2:N)
  {
    x[i,1:p] ~ dmnorm(rho * x[i-1,1:p] ,precision2[,])
  }
  
  # Prior for likelihood parameters: mu2, precision2, rho
  rho  ~  dunif(-1,1)
  
  for(j in 1:p)
  {
    mu2[j]  ~  dnorm(0,0.01)
  }
  
  precision2[1:p,1:p] ~ dwish(R[,],k)
  
  # Missing data model for x
  for(i in 1:N){
    x[i,1:p]~dmnorm(x_mn[],x_prec[,])
  }
  
  # Priors for missing-data model parameters
  for(j in 1:p){
    x_mn[j]~dnorm(0,0.01)
  }
  x_prec[1:p,1:p]~dwish(R[,],k)
  x_cov[1:p,1:p]<-inverse(x_prec[,])
  
  k <- p+0.1
  for(j1 in 1:p)
  {
    for(j2 in 1:p)
    {
      R[j1,j2] <- 0.1*equals(j1,j2)
    }
  }
}
stacks_dat <- list(x=x,p = 6,   N = 365)
mlr_inits <- function() { list( rho = 0.00)}
samps <- bugs(data = stacks_dat, 
              inits = mlr_inits, 
              parameters.to.save = c("x"), 
              model.file = mlr_model2, 
              codaPkg = TRUE,
              n.chains = 1, n.burnin=100, n.iter = 2000, n.thin=10, DIC=F)
