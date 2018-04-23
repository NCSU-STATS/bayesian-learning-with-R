rm(list = ls())
library(rle)
setwd("c:/e/brucebcampbell-git/bayesian-learning-with-R/E3")

load("heatwaves.RData")

plot(y[1,1:365,1])
abline(h = quantile(y[1,1:365,1],.95))

###################################################################
#  HW  = 3 consecutive days above 95th quantile for that year
###################################################################
X.num <- matrix(nrow =41, ncol = 9 )
X.sev <- matrix(nrow =41, ncol = 9 )

for(i in 1:41)
{
  for(k in 1:9)
  {
    data <- y[i,1:365,k]
    data <- na.exclude(data)
    q_ik <-  quantile(data,.95)
    hot_ik <- data > q_ik
    run_lengths <- rle(hot_ik)
    df_rl <- data.frame(l = run_lengths$lengths,v = run_lengths$values)
    hot_groups <- df_rl[df_rl$v==TRUE,]
    heat_waves <- hot_groups[hot_groups$l>3,]
    num_hw <- nrow(heat_waves)
    sev_hw <- sum(heat_waves$l)
    
    X.num[i,k] <- num_hw
    X.sev[i,k] <- sev_hw
  }
    
}

image(X.num)
image(X.sev)
f<- rowSums(X.num)
plot(f)
f<- rowSums(X.sev)
plot(f)
plot(X.sev[,1])
plot(X.sev[,2])
plot(X.sev[,3])
plot(X.sev[,4])
plot(X.sev[,5])
plot(X.sev[,6])
plot(X.sev[,7])
plot(X.sev[,8])
plot(X.sev[,9])


###################################################################
#  HW  = World Meteorological Organization definition
#  daily temperature for more than five consecutive days exceeds the average temperature by 9 degrees
###################################################################
X.num <- matrix(nrow =41, ncol = 9 )
X.sev <- matrix(nrow =41, ncol = 9 )

#Daily Averages
DA <- matrix(nrow = 365,ncol = 9)
for(j in 1:365)
{
  for(k in 1:9)
  {
    data <- y[1:41,j,k]
    data <- na.exclude(data)
    DA[j,k] <- mean(data)
  }
}
plot(y[1,1:365,1]); lines(DA[,1],col='red')


AboveDA.Idx <- y
for(i in 1:41)
{
  for(k in 1:9)
  {
    for(j in 1:365)
    {
      t <- y[i,j,k]
      A <- DA[j,k]
      isHot <- t >=A+9
      AboveDA.Idx[i,j,k] <- isHot
    }
  }
}

X.num <- matrix(nrow =41, ncol = 9 )
X.sev <- matrix(nrow =41, ncol = 9 )

for(i in 1:41)
{
  for(k in 1:9)
  {
    data <- AboveDA.Idx[i,1:365,k]
    data <- na.exclude(data)
    run_lengths <- rle(as.vector(data))
    df_rl <- data.frame(l = run_lengths$lengths,v = run_lengths$values)
    hot_groups <- df_rl[df_rl$v==TRUE,]
    heat_waves <- hot_groups[hot_groups$l>5,]
    num_hw <- nrow(heat_waves)
    sev_hw <- sum(heat_waves$l)
    
    X.num[i,k] <- num_hw
    X.sev[i,k] <- sev_hw
  }
  
}

image(X.num)
image(X.sev)
f<- rowSums(X.num)
plot(f)
f<- rowSums(X.sev)
plot(f)
plot(X.sev[,1])
plot(X.sev[,2])
plot(X.sev[,3])
plot(X.sev[,4])
plot(X.sev[,5])
plot(X.sev[,6])
plot(X.sev[,7])
plot(X.sev[,8])
plot(X.sev[,9])



###############################333
#Plot the prior, likelihood, and posterior on a grid
k=1
for(i in 1:41)
{
  N      <- 1
  Y      <- X.num[i,k]
  a      <- 1
  b      <- 0.5
  grid   <- seq(0.01,10,.01)
  like   <- dpois(Y,N*grid)
  like   <- like/sum(like) #standardize
  prior  <- dgamma(grid,a,b)
  prior  <- prior/sum(prior) #standardize
  post   <- like*prior
  post   <- post/sum(post)
  ps.mean <-sum(post*grid)
  plot(grid,like,type="l",lty=2,
       xlab="lambda",ylab="Density",main=paste(Y," post mean = ",ps.mean,sep = ' '), ylim=c(0,.1))
   lines(grid,prior)
  lines(grid,post,lwd=2)
  legend("topright",c("Likelihood","Prior","Posterior"),lwd=c(1,1,2),lty=c(2,1,1),inset=0.05)
}


k=1
hist(X.num[,k])
library(fitdistrplus)    # fits distributions using maximum likelihood
library(gamlss)          # defines pdf, cdf of ZIP
# FIT DISTRIBUTION (mu = mean of poisson, sigma = P(X = 0)
fit_zip = fitdist(X.num[,k], 'ZIP', start = list(mu = 2, sigma = 0.7))
fit_zip2 <- fitdist(X.num[,k], 'nbinom', start = list(mu = 3, size = 0.1)) 
# VISUALIZE TEST AND COMPUTE GOODNESS OF FIT    
plot(fit_zip2)
gofstat(fit_zip2, print.test = T)

df <- data.frame(t=seq(1:41),Y=X.num[,k])
model.pois <- glm( Y~ t, family=poisson, df)
summary(model.pois)
sumary(model.pois)


