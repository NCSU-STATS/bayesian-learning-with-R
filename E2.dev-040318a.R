rm(list = ls())
setwd("d:/brucebcampbell-git/bayesian-learning-with-R")

load("E2.RData")
library(R2OpenBUGS)
library(rjags)
library(coda)
library(modeest)

dim(Y1)

cor(Y1, use = "pairwise.complete.obs")
N <- nrow(Y1)
p = 6


x <- scale(Y1)

stacks_dat <- list(x=x,p = 6,   N = 365)
mlr_inits <- function() {
  list( rho = 0.00)
}

mlr_model2 <- function(){
  for(i in 1:N)
  {
    theta[i,1:p] ~ dmnorm(x[i,1:p] ,precision2[,])
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
n.chains = 1
nSamples=5000
samps <- bugs(data = stacks_dat, 
              inits = mlr_inits, 
              parameters.to.save = c("theta","x"), 
              model.file = mlr_model2, 
              codaPkg = TRUE,
              n.chains = n.chains, n.burnin=1000, n.iter = nSamples, n.thin=10, DIC=F)

out.coda <- read.bugs(samps) 

if(n.chains > 1)
{
  gelman.srf <-gelman.diag(out.coda)
  count.coeff.gt <- sum(gelman.srf$psrf>1.1)
  count.coeff.gt
  #pander(data.frame(count.coeff.gt=count.coeff.gt ),caption="Number of coefficients with a a gelman scale reduction factor greater than 1.1" )
}

chains.ess <- lapply(out.coda,effectiveSize)

first.chain.ess <- chains.ess[1]
plot(unlist(first.chain.ess), main="Effective Sample Size")

chain <- out.coda[[1]]
posterior.means <- vector(365*6)
posterior.modes <- matrix(365*6)
for( i in 1:(365*6) )
{  colname <- colnames(chain)[i]
  print(colname)
  samples <- chain[,i]
  
  posterior.means[i] <-mean(samples)
  
  posterior.modes[i] <-mlv(samples)$M
}

plot(posterior.means, posterior.modes)

plot(out.coda)
     
     
     
plot(samps)


library(coda)
library(modeest)
library(MASS) 
library(lattice) 



densityplot(out.coda)
xyplot(out.coda)







