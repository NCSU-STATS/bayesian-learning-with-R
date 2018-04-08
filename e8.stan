  Y1pred[1,1:p] ~ dmnorm( Y1[1,1:p] ,precision[,])
  
  for(i in 2:N) {
    Y1pred[i,1:p] ~ dmnorm( theta[i,1:p] ,precision[,])
    
    Y2[i] ~ dnorm(theta[i,1],sigmaY2)
    
    theta[i,1]<-  mu2[1] + rho * Y1[i-1,1]
    theta[i,2]<-  mu2[2] + rho * Y1[i-1,2]
    theta[i,3]<-  mu2[3] + rho * Y1[i-1,3]
    theta[i,4]<-  mu2[4] + rho * Y1[i-1,4]
    theta[i,5]<-  mu2[5] + rho * Y1[i-1,5]
    theta[i,6]<-  mu2[6] + rho * Y1[i-1,6]

    # Causes a compilation error :( with the Openbugs engine - we use Y3 in the prior for Y1 imputation
    #Y3[i] ~ dnorm(thetaBar[i],sigmaY3)
    #thetaBar[i] <- 1/6 * (theta[i,2] +theta[i,2]+ theta[i,3]+ theta[i,4] +theta[i,5] +theta[i,6])
    }
  
  # Priors
  rho  ~  dunif(0,1)
  sigmaY3 ~ dnorm(0, 0.01)
  sigmaY2  ~ dnorm(0, 0.01)
    
  for(j in 1:p)
  {
    mu2[j]  ~  dnorm(0,0.01)
  }
  
  precision[1:p,1:p] ~ dwish(R[,],k)
  
  SigmaVAR[1:p,1:p] <- inverse(precision[1:p,1:p])
  
  # Missing data model for x
  for(i in 1:N){
    Y1[i,1:p]~dmnorm(Y1_mn[],Y1_prec[,])
  }
  
  # Priors for missing-data model parameters
  for(j in 1:p){
    Y1_mn[j]~dnorm(Y3[j],0.01)
  }
  Y1_prec[1:p,1:p]~dwish(R[,],k)
  
  k <- p+0.1
  for(j1 in 1:p)
  {
    for(j2 in 1:p)
    {
      R[j1,j2] <- 0.1*equals(j1,j2)
    }
  }
