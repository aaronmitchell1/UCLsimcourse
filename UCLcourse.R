library(foreach)
library(rsimsum)

#Set seed
set.seed(5000)

#Example: generate data for 2 groups, sample size 5000 each, 
#with lambda of 1 for both groups (equal variances)

df1pois <- data.frame(id = 1:5000,
                  dat = rpois(5000, 1))

df2pois <- data.frame(id = 1:5000,
                  dat = rpois(5000, 1))

#Histogram of Poisson data
hist(df1pois$dat)
hist(df2pois$dat)

#From normal distribution - use normally distributed data as control

df1norm <- data.frame(id = 1:5000,
                       dat = rnorm(5000, 0, 1))

df2norm <- data.frame(id = 1:5000,
                      dat = rnorm(5000, 0, 1))

#Histogram of normally distributed data
hist(df1norm$dat)
hist(df2norm$dat)

#Combine 2 datasets
#Assign a grouping variable
df1pois$grp <- 1
df2pois$grp <- 2
df1norm$grp <- 1
df2norm$grp <- 2

datpois$grp <- as.factor(datpois$grp)
datnorm$grp <- as.factor(datnorm$grp)

datpois <- rbind(df1pois, df2pois)
datpois$id <- 1:10000

datnorm <- rbind(df1norm, df2norm)
datnorm$id <- 1:10000

#Run a t-test on Poisson data assuming equal variances
tpois <- t.test(dat ~ grp, data=datpois, var.equal=TRUE)
tnorm <- t.test(dat ~ grp, data=datnorm, var.equal=TRUE)

#Extract estimate and SE from t-test on Poisson data
tstatpois <- tpois$estimate[1] - tpois$estimate[2]
tsepois <- tpois$stderr

tstatnorm <- tnorm$estimate[1] - tnorm$estimate[2]
tsenorm <- tnorm$stderr

#Function for 1 iteration - possibly do smaller nobs

sim <- function(nobs1, nobs2, lambda1, lambda2)
{
  df1pois <- data.frame(id = 1:nobs1,
                        dat = rpois(nobs1, lambda1))
  
  df2pois <- data.frame(id = 1:nobs2,
                        dat = rpois(nobs2, lambda2))
  
  df1pois$grp <- 1
  df2pois$grp <- 2
  
  datpois <- rbind(df1pois, df2pois)
  datpois$id <- 1:nobs1+nobs2
  datpois$grp <- as.factor(datpois$grp)
  
  tpois <- t.test(dat ~ grp, data=datpois, var.equal=TRUE)
  tstatpois <- tpois$estimate[1] - tpois$estimate[2]
  tsepois <- tpois$stderr
  
  res <- data.frame(tstatpois, tsepois)
}

#Function for loop

simloop <- function(iter, nobs1, nobs2, lambda1, lambda2)
           {
  
  df1pois <- data.frame(id = 1:nobs1,
                        dat = rpois(nobs1, lambda1))
  
  df2pois <- data.frame(id = 1:nobs2,
                        dat = rpois(nobs2, lambda2))
  
  df1pois$grp <- 1
  df2pois$grp <- 2
  
  datpois <- rbind(df1pois, df2pois)
  datpois$id <- 1:nobs1+nobs2
  datpois$grp <- as.factor(datpois$grp)
  
  tpois <- t.test(dat ~ grp, data=datpois, var.equal=TRUE)
  tstatpois <- tpois$estimate[1] - tpois$estimate[2]
  tsepois <- tpois$stderr
  
  res <- data.frame(i = iter, diff = tstatpois, se = tsepois)
  
  return(res)
}

#Loop results for Poisson

iter=500
nobs1=250
nobs2=250
lambda1=1
lambda2=1

for (i in 1:iter) {
  
  res <- simloop(iter=i, nobs1=nobs1, nobs2=nobs2, 
                          lambda1=lambda1, lambda2=lambda2)
  
  if(i==1){
    results<-res 
  }else{
    results<-rbind(results,res)
  }
}

#Function for normally distributed data
#For 1 iteration

simnorm1 <- function(nobs1, nobs2, mean1, sd1, mean2, sd2)
{
  df1norm <- data.frame(id = 1:nobs1,
                        dat = rnorm(nobs1, mean1, sd1))
  
  df2norm <- data.frame(id = 1:nobs2,
                        dat = rnorm(nobs2, mean2, sd2))
  
  df1norm$grp <- 1
  df2norm$grp <- 2
  
  datnorm <- rbind(df1norm, df2norm)
  datnorm$id <- 1:nobs1+nobs2
  
  datnorm$grp <- as.factor(datnorm$grp)
  
  tnorm <- t.test(dat ~ grp, data=datnorm, var.equal=TRUE)
  tstatnorm <- tnorm$estimate[1] - tnorm$estimate[2]
  tsenorm <- tnorm$stderr
  
  res <- data.frame(tstatnorm, tsenorm)
}

nobs1=250
nobs2=250
mean1=0
mean2=0
sd1=1
sd2=1

simnorm1(nobs1 = nobs1, nobs2 = nobs2, mean1 = mean1, mean2 = mean2, sd1 = sd1, sd2 = sd2)


simloopnorm <- function(iter, nobs1, nobs2, mean1, sd1, mean2, sd2)
{
  
  df1norm <- data.frame(id = 1:nobs1,
                        dat = rnorm(nobs1, mean1, sd1))
  
  df2norm <- data.frame(id = 1:nobs2,
                        dat = rnorm(nobs2, mean2, sd2))
  
  df1norm$grp <- 1
  df2norm$grp <- 2
  
  datnorm <- rbind(df1norm, df2norm)
  datnorm$id <- 1:nobs1+nobs2
  datnorm$grp <- as.factor(datnorm$grp)
  
  tnorm <- t.test(dat ~ grp, data=datnorm, var.equal=TRUE)
  tstatnorm <- tnorm$estimate[1] - tnorm$estimate[2]
  tsenorm <- tnorm$stderr
  
  res <- data.frame(i = iter, diff = tstatnorm, se = tsenorm)
  
  return(res)
}

simloop2 <- simloopnorm(iter = iter, nobs1 = nobs1, nobs2 = nobs2, mean1 = mean1, mean2 = mean2, sd1 = sd1, sd2 = sd2)

for (i in 1:iter) {
  
  resnorm <- simloopnorm(iter=i, nobs1=nobs1, nobs2=nobs2, 
                     mean1 = mean1, mean2 = mean2, sd1 = sd1, sd2 = sd2)
  
  if(i==1){
    resultsnorm<-resnorm
  }else{
    resultsnorm<-rbind(resultsnorm,resnorm)
  }
}

s1 <- simsum(data = resultsnorm, estvarname = "diff", se = "se", true = 0)


#Performance measures - use rsimsum. Empirical being smaller than model-based is 'better' - better bias properties.
MCSE value is after bias, cannot exclude that bias is 0 but it should be. SE from single rep is the true SE

s <- simsum(data = results, estvarname = "diff", se = "se", true = 0)

#Do we want to vary the group sizes? Can do this with the function we made.

#For loop with 2 DGMs



#Use foreach

#Where do we specify mean and SD?

function(mean1, mean2, sd1, sd2)

foreach(i = 1) %do% {
  
  df1norm <- data.frame(id = 1:nobs1,
                        dat = rnorm(nobs1, mean1, sd1))
  
  df2norm <- data.frame(id = 1:nobs2,
                        dat = rnorm(nobs2, mean2, sd2))
}

#We simulated data for 250 individuals in each group. First we used a Poisson distribution, with lambda as 1. We then 
#used an unpaired t-test assuming equal variances as our method. We ran the simulation for 500 iterations. In an ideal method
#the bias should be 0. The point estimate was -0.0055 with a MCSE of 0.0039. Our estimate is therefore more or less unbiased as we cannot exclude 0 as this included if we do 1.96*x
#Empirical  0.0879  and model-based standard error 0.0894  were very similar. Coverage was 0.95 which is good.
#Type 1 error (power) was also not bad

#We then simulated data for 250 individuals in each group for a normal distribution with mean 0 and sd 1. Also 500 reps.
#The bias was  0.0011 which is less than for the previous DGM. The ESE was 0.09 and MSE was also 0.09. Coverage was 
#very similar, power was also very similar. We are happy with the number of iterations as coverage was good. The 
#method was also quick.

#Diagnostics? Mean for each DGM and correlation between groups/DGMs
#Matrices to store them before the loop
meang1 <- matrix(0, iter)
meang2 <- matrix(0, iter)
cor1 <- matrix(0, iter)

meang1[i] <- mean(datpois$dat[datpois$grp=="1"])
meang2[i] <- mean(datpois$dat[datpois$grp=="2"])
cor1[i] <- cor(datpois$dat[datpois$grp=="1"], datpois$dat[datpois$grp=="2"])