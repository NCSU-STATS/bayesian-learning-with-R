
load("E2.RData")

dim(Y1)

cor(Y1, use = "pairwise.complete.obs")
N <- nrow(Y1)
p = 6
x <- scale(Y1)
stacks_dat <- list(x=x,p = 6,   N = 365)
library(R2OpenBUGS)
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
samps <- bugs(data = stacks_dat, 
              inits = mlr_inits, 
              parameters.to.save = c("theta",  "x_mn","x_cov"), 
              model.file = mlr_model2, 
              n.chains = 1, n.burnin=10, n.iter = 20, n.thin=10, DIC=F)

plot(samps)








