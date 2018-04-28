#HeatWave W  = 3 consecutive days above 95th quantile of daily 

rm(list = ls())
setwd("c:/e/brucebcampbell-git/bayesian-learning-with-R/E3")
load("heatwaves.RData")

#Daily - 95th quantile 
DQ <- matrix(nrow = 365,ncol = 9)
for(j in 1:365)
{
  for(k in 1:9)
  {
    data <- y[1:41,j,k]
    data <- na.exclude(data)
    DQ[j,k] <- quantile(data,.90)
  }
}
plot(y[1,1:365,1]); lines(DQ[,1],col='red', main = "daily and 90th quantile")

AboveDQ.Idx <- y
for(i in 1:41)
{
  for(k in 1:9)
  {
    for(j in 1:365)
    {
      t <- y[i,j,k]
      A <- DQ[j,k]
      isHot <- t >=A
      AboveDQ.Idx[i,j,k] <- isHot
    }
  }
}

X.num <- matrix(nrow =41, ncol = 9 )
X.sev <- matrix(nrow =41, ncol = 9 )

for(i in 1:41)
{
  for(k in 1:9)
  {
    data <- AboveDQ.Idx[i,1:365,k]
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

save(X.num,X.sev,file ="HWD1.RData")
rm(X.num,X.sev)

# HeatWave  = World Meteorological Organization definition
#daily temperature for more than five consecutive days exceeds the average temperature by 9 degrees

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

save(X.num,X.sev,file ="HWD2.RData")
rm(X.num,X.sev)
