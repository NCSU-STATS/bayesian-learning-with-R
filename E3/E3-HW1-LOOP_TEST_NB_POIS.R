rm(list = ls())
library(pander)
setwd("c:/e/brucebcampbell-git/bayesian-learning-with-R/E3")
load("heatwaves.RData")
n.chains =2;
nSamples = 10000
load("HWD2.RData")
k =5
# Fit JAGS GLM Models for Poisson
library(rjags)
library(coda)
model_code = '
model
{
    ## Likelihood
    for(i in 1:N){
      Y[i] ~ dpois(lambda[i])
      log(lambda[i]) <- mu[i]
      mu[i] <- intercept + beta*t[i]
    }

  ## Priors
  beta ~ dnorm(mu.beta,tau.beta)
  intercept ~ dnorm(mu.intercept,tau.intercept)

  ## Posterior Predictive Checks
  for(i in 1:N){
    Y2[i]    ~ dpois(lambda[i])
  }
  D[1] <- mean(Y2[])
  D[2] <- sd(Y2[])
}
'
print(paste("JAGS GLM Models for Poisson ", k,sep = " "))
  # Set up the data
  model_data = list(N = 41, t=seq(1:41),Y=X.num[,k],mu.beta=0,tau.beta=.0001,mu.intercept=0,tau.intercept=.0001  )
  # Choose the parameters to watch
  model_parameters =  c("beta", "intercept","D")
   model <- jags.model(textConnection(model_code),data = model_data,n.chains = n.chains)#Compile Model Graph
  update(model, nSamples, progress.bar="none"); # Burnin
  out.coda  <- coda.samples(model, variable.names=model_parameters,n.iter=2*nSamples) 
  plot(out.coda)
  #assess the posterior's stationarity, by looking at the Heidelberg-Welch convergence diagnostic:
  heidel.diag(out.coda) 
  # check that our chain's length is satisfactory.
  raftery.diag(out.coda)  
  
  geweke.diag(out.coda)
  if(n.chains > 1)
  {
    gelman.srf <-gelman.diag(out.coda)
    plot(gelman.srf$psrf,main = "Gelman Diagnostic")
  }

  chains.ess <- lapply(out.coda,effectiveSize)
  first.chain.ess <- chains.ess[1]
  plot(unlist(first.chain.ess), main="Effective Sample Size")
  
  # Compute the test stats for the data
  D0   <- c(   mean(X.num[,k]),   sd(X.num[,k]))
  Dnames <- c("mean Y", "sd Y")
  # Compute the test stats for the models
  chain <- out.coda[[1]]
  D1   <- cbind(chain[,"D[1]"],chain[,"D[2]"])
  pval1 <- rep(0,2)
  names(pval1)<-Dnames
  pval2 <- pval1
  for(j in 1:2){
   pval1[j] <- mean(D1[,j]>D0[j]) 
  }
  
  pander(data.frame(p.vals = pval1) ,caption=paste("Posterior Perdictive check p-values ",k,sep = " "))
  
  chain <- out.coda[[1]]

  posterior.means <- list()
  
  for( i in 1:length(colnames(chain)) )
  {  
    colname <- colnames(chain)[i]
    samples <- chain[,i]
    posterior.means[colname] <-mean(samples)
  }
    pander(data.frame(posterior.means))
    
###Frequentist
    
    
    df <- data.frame(t=seq(1:41),Y=X.num[,k])
    model.pois <- glm( Y~ t, family=poisson, df)
    sp <- summary.glm(model.pois)
    sp
####
    
    
# Fit JAGS GLM Models for Negative Binomial

model_code = '
model
{
    ## Likelihood
    for(i in 1:N){
      Y[i] ~ dnegbin(p[i],r)
      
      p[i] <- r/(r+lambda[i]) 
      log(lambda[i]) <- mu[i]
      mu[i] <- intercept + beta*t[i]
    }

  ## Priors
  beta ~ dnorm(mu.beta,tau.beta)
  intercept ~ dnorm(mu.intercept,tau.intercept)
  r ~ dunif(0,20)
  #r ~ dgamma(0.01,0.01)


  ## Posterior Predictive Checks
  for(i in 1:N){
    Y2[i]    ~ dnegbin(p[i],r)
  }
  D[1] <- mean(Y2[])
  D[2] <- sd(Y2[])

}
'
  print(paste("JAGS GLM Models for Negative Binomial ", k,sep = " "))
  # Set up the data
  model_data = list(N = 41, t=seq(1:41),Y=X.num[,k],mu.beta=0,tau.beta=.0001,mu.intercept=0,tau.intercept=.0001  )
  # Choose the parameters to watch
  model_parameters =  c("beta", "intercept","D")
   model <- jags.model(textConnection(model_code),data = model_data,n.chains = n.chains)#Compile Model Graph
  update(model, nSamples, progress.bar="none"); # Burnin
  out.coda  <- coda.samples(model, variable.names=model_parameters,n.iter=2*nSamples) 
  plot(out.coda)
  #assess the posterior's stationarity, by looking at the Heidelberg-Welch convergence diagnostic:
  heidel.diag(out.coda) 
  # check that our chain's length is satisfactory.
  raftery.diag(out.coda)  
  
  geweke.diag(out.coda)
  if(n.chains > 1)
  {
    gelman.srf <-gelman.diag(out.coda)
    plot(gelman.srf$psrf,main = "Gelman Diagnostic")
  }

  chains.ess <- lapply(out.coda,effectiveSize)
  first.chain.ess <- chains.ess[1]
  plot(unlist(first.chain.ess), main="Effective Sample Size")
  
  # Compute the test stats for the data
  D0   <- c(   mean(X.num[,k]),   sd(X.num[,k]))
  Dnames <- c("mean Y", "sd Y")
  # Compute the test stats for the models
  chain <- out.coda[[1]]
  D1   <- cbind(chain[,"D[1]"],chain[,"D[2]"])
  pval1 <- rep(0,2)
  names(pval1)<-Dnames
  pval2 <- pval1
  for(j in 1:2){
   pval1[j] <- mean(D1[,j]>D0[j]) 
  }
  
  pander(data.frame(p.vals = pval1) ,caption=paste("Posterior Perdictive check p-values ",k,sep = " "))
  
  chain <- out.coda[[1]]

  posterior.means <- list()
  
  for( i in 1:length(colnames(chain)) )
  {  
    colname <- colnames(chain)[i]
    samples <- chain[,i]
    posterior.means[colname] <-mean(samples)
  }
    pander(data.frame(posterior.means))

### Frequentist
    
    model.nb <- glm.nb(Y~t,data=df)
    snb <- summary.glm(model.nb)
    