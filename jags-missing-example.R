rm(list = ls())
setwd("c:/e/brucebcampbell-git/bayesian-learning-with-R")
set.seed(123) # make reproducible
M = 10000 # Number of posterior samples

N = 200
# sample the IV
x = rnorm(N, 10, 5)
# regression model for DVs
y = 100 + 3*x + rnorm(N,0,10)

## Delete missing data (3 in each)
x.miss = sample(N)[1:3]
x[x.miss]= NA
y.miss = sample(N)[1:3]
y[y.miss]= NA


## Build the JAGS model
mod_str = "
model{
for(i in 1:length(y)){
# regression model on y, given x
y[i] ~ dnorm( mu[i], prec.err )
mu[i] <- intercept + slope * x[i]

# Stochastic model on x
x[i] ~ dnorm( mu.x, prec.x)
}

# priors for demonstration only
intercept ~ dnorm(0, .0000001)
slope ~ dnorm(0, .0000001)
mu.x ~ dnorm(0, .0000001)
prec.x ~ dgamma(.01, .01)
prec.err ~ dgamma(.01,.01)
}
"
for_jags = list(x=x,y=y)

library(rjags)
## Compile the model
jmod = jags.model(file = textConnection(mod_str), data = for_jags)
## Sample from the posterior
samples = coda.samples(jmod, c("intercept","slope","y","x"), n.iter = M)

## Now you're done. The rest is just for demonstration.

plot(samples[[1]][,"intercept"])
plot(samples[[1]][,"slope"])

## See how JAGS fills in missing data
first.miss.x = paste0("x[",x.miss[1],"]")
first.miss.y = paste0("x[",y.miss[1],"]")

## in x
plot(samples[[1]][,first.miss.x])


## in y
plot(samples[[1]][,first.miss.y])


## Create a scatterplot showing the "imputation"
## Error bars represent posterior standard deviations
plot(x,y, ylim = range(y, na.rm = TRUE), 
     xlim = range(x, na.rm = TRUE), pch = 19)
for(i in x.miss){
  lab = paste0("x[",i,"]")
  s = samples[[1]][,lab]
  mn = mean(s)
  sdv = sd(s)
  points(mn, y[i], pch = 19, col = "blue")
  segments(mn - sdv, y[i], mn + sdv, y[i], lwd=2, col="blue")
}

for(i in y.miss){
  lab = paste0("y[",i,"]")
  s = samples[[1]][,lab]
  mn = mean(s)
  sdv = sd(s)
  points(x[i], mn,  pch = 19, col = "red")
  segments(x[i], mn - sdv, x[i], mn + sdv, lwd=2, col="red")
}
### End