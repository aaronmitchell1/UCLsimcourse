#Set seed
set.seed(5000)

#Generate data for 2 groups, sample size 5000 each, 
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

datpois <- rbind(df1pois, df2pois)
datpois$id <- 1:10000
datpois$grp <- as.factor(datpois$grp)

datnorm <- rbind(df1norm, df2norm)
datnorm$id <- 1:10000
datnorm$grp <- as.factor(datnorm$grp)

#Run a t-test on Poisson data assuming equal variances
tpois <- t.test(dat ~ grp, data=datpois, var.equal=TRUE)
tnorm <- t.test(dat ~ grp, data=datnorm, var.equal=TRUE)

#Extract estimate and SE from t-test on Poisson data
tstatpois <- tpois$estimate[1] - tpois$estimate[2]
tsepois <- tpois$stderr

tstatnorm <- tnorm$estimate[1] - tnorm$estimate[2]
tsenorm <- tnorm$stderr

#Function for 1 iteration

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

#Loop
iter=500, nobs1=5000, nobs2=5000, lambda1=1, lambda2=1   

results <- vector(mode = "list", length = iter)

for (i in 1:iter) {
  
  results[[i]] <- simloop(iter=iter, nobs1=nobs1, nobs2=nobs2, 
                            lambda1=lambda1, lambda2=lambda2)

}

#Diagnostics
#Mean for data generated for group 1 and 2
meang1 <- mean(datpois$dat[datpois$grp=="1"])
meang2 <- mean(datpois$dat[datpois$grp=="2"])

#Correlation between data in groups 1 and 2
cor1 <- cor(datpois$dat[datpois$grp=="1"], datpois$dat[datpois$grp=="2"])
  
#Do we want to vary the group sizes?

#Function for loop, 1 DGM

dgm <- 1:2