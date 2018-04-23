#######################################################################
# MCMC sampling for an ill-posed model to illustrate convergence issues
#
#  The model is  Y|mu1,mu2 ~ N(mu1+mu2,1)
#  The priors are  mu1,mu2 ~ N(0,pri_sd^2)
#
#######################################################################


# Set up

Y       <- 1
pri_sd  <- 10
n.iters <- 1000



# plot the posterior p(mu1,mu2|Y) on a grid for reference

grid     <- seq(-20,20,length=100)
grid_mu1 <- matrix(grid,100,100,byrow=FALSE)
grid_mu2 <- matrix(grid,100,100,byrow=TRUE)

prior    <- dnorm(grid_mu1,0,pri_sd)*
  dnorm(grid_mu2,0,pri_sd)
like     <- dnorm(Y,grid_mu1+grid_mu2,1)
post     <- prior*like
post     <- post/sum(post)

library(fields)
par(mfrow=c(1,1))
image.plot(grid,grid,post,main="Posterior",
           xlab=expression(mu[1]),
           ylab=expression(mu[2]),
           col=gray(1-seq(0,0.5,.1)))





# Define the function that sample one mu from its full conditional

sample<-function(y,sig0){
  B  <- 1 + 1/sig0^2
  A  <- y
  mu <- rnorm(1,A/B,1/sqrt(B))
  return(mu)}




# Gibbs sampling!

# initial values
mu1 <- -10
mu2 <- -10

keep.mu     <- matrix(0,n.iters,2)
keep.mu[1,] <- c(mu1,mu2)

# The first 10 samples are plotted

for(iter in 2:10){
  
  
  #Draw mu1|mu2,y
  abline(mu2,0)
  Sys.sleep(2)
  
  mu1 <- sample(Y-mu2,pri_sd)
  points(mu1,mu2,col=4,pch=19)
  
  #Draw mu2|mu1,y
  abline(v=mu1)
  Sys.sleep(2)
  
  mu2 <- sample(Y-mu1,pri_sd)
  points(mu1,mu2,col=4,pch=19)
  
  keep.mu[iter,]<-c(mu1,mu2)
  
}


# The remaining samples are not plotted

for(iter in 11:n.iters){
  mu1            <- sample(Y-mu2,pri_sd)
  mu2            <- sample(Y-mu1,pri_sd)
  keep.mu[iter,] <- c(mu1,mu2)
}


# Plot all the samples over the true density

library(fields)
image.plot(grid,grid,post,main="Posterior",
           xlab=expression(mu[1]),
           ylab=expression(mu[2]),
           col=gray(1-seq(0,0.5,.1)))
points(keep.mu,col=4,pch=19)


par(mfrow=c(2,2))
keepers <- 5:n.iters
hist(keep.mu[keepers,1],main="mu1",breaks=50)
plot(keep.mu[keepers,1],type="l",ylab="mu1")
hist(keep.mu[keepers,2],main="mu2",breaks=50)
plot(keep.mu[keepers,2],type="l",ylab="mu2")

