setwd("d:/brucebcampbell-git/bayesian-learning-with-R")
load("E2.RData")

x<- as.matrix(Y1)

stacks_dat <- list(x=x,p = 6,   N = 365)

all_data           <- cbind(x)


mlr_model <- function(){

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
  
  k<-p+0.1
  for(j1 in 1:p){for(j2 in 1:p){R[j1,j2]<-0.1*equals(j1,j2)}} #R is diagonal
}

library(rjags)
library(coda)

library(R2OpenBUGS)

mlr_inits <- function() {
  list(intercept = mean(stacks_dat$Y,na.rm=T), beta = rnorm(stacks_dat$p), tau = 0.01)
}

samps <- bugs(data = stacks_dat, 
              inits = mlr_inits, 
              parameters.to.save = c("x_cov", "x"), 
              model.file = mlr_model, debug=FALSE,
              n.chains = 1, n.burnin=1000, n.iter = 10000, n.thin=2, DIC=F)

library(coda)

out.coda <- read.bugs(samps)
#save(out.coda,file = "out.coda_OPENBUGS_VAR1.RData")

if(n.chains > 1)
{
  g <- matrix(NA, nrow=nvar(out.coda), ncol=2)
  for (v in 1:nvar(out.coda)) {
    g[v,] <- gelman.diag(out.coda[,v])$psrf
  }
  count.coeff.gt <- sum(g[,1]>1.1)
  count.coeff.gt
  plot(g[,1],main="Gelman-Rubin ")
}

chains.ess <- lapply(out.coda,effectiveSize)

first.chain.ess <- chains.ess[1]
plot(unlist(first.chain.ess), main="Effective Sample Size")

chain <- out.coda[[1]]

Y1.posterior.modes <- list()

for( i in 1:(365*6+36) )
{  
  colname <- colnames(chain)[i]
  if(grepl("Sigma",colname)  )
  {
    print(paste("skipping ",colname,sep=""))
    next
  }
  samples <- chain[,i]
  samplesId <- samples<4
  red <- samples[samplesId]
  Y1.posterior.modes[colname] <-mlv(red)$M
}

theta.map <-  matrix(unlist(Y1.posterior.modes),ncol=6, byrow=FALSE)

unscaled.theta.map <- ( theta.map +colMeans(Y1,na.rm = TRUE)) *  apply(Y1, 2,sd,na.rm = TRUE)

image(Y1, main="Y1")

image(unscaled.theta.map, main="Imputed")

SigmaVAR.posterior.modes <- list()

for( i in 1:(365*6+36) )
{  
  colname <- colnames(chain)[i]
  if(grepl("x_cov",colname) ==FALSE )
  {
    next
  }
  samples <- chain[,i]
  SigmaVAR.posterior.modes[colname] <-mlv(samples)$M
}

Sigma.map <-  matrix(unlist(SigmaVAR.posterior.modes),ncol=6, byrow=FALSE)
image(Sigma.map, main = "MAP Sigma")


b <- unscaled.theta.map[!is.na(Y1)]
c <- Y1[!is.na(Y1)]

plot(b,c)
