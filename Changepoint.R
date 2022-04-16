#### Bayesian Changepoint Detection ####
########################################

#### Get data ####
library(boot)
data(coal)
x <- 1851:1962
y <- tabulate(floor(coal[[1]]))
y <- y[x]

### 1. No change points
########################
a1 = 2 # prior
b1 = 1 # prior
a2 = a1 + sum(y) # posterior
b2 = b1+112 # posterior
xs <- seq(0,4,0.01) # sequence of lambdas
pd <- dgamma(xs, a2, b2) # posterior distribution
plot(xs, pd, type='l')

# Draw predictions
lambdasample <- rgamma(10000, a2, b2) # sample lambdas from gamma
pred <- matrix(0, nrow=10000, ncol=112)
for (sample in 1:10000){
  for (year in 1:112){
    indpred <- rpois(1,lambdasample[sample])
    pred[sample, year] <- indpred
  }
}

# Marginal probability
l.py <- -sum(lfactorial(y))-(2+sum(y))*log(113)+lfactorial(sum(y)+1)
py = exp(l.py)

# Plot 95% prediction interval
cp <- apply(pred,1,cumsum) # cumulative 
cp <- t(cp)
medp <- rep(0,112)
twoh <- rep(0,112)
nins <- rep(0,112)
for (year in 1:112){
  medp[year]<-quantile(cp[,year],probs = c(0.5))
  twoh[year]<-quantile(cp[,year],probs = c(0.025))
  nins[year]<-quantile(cp[,year],probs = c(0.975))
}
plot(1851:1962,medp,type='l',ylim=c(1,220))
lines(1851:1962, twoh, col='red')
lines(1851:1962, nins, col='red')
lines(1851:1962, cumsum(y), col='orange')


### 2. One change point
########################
alpha1n <- rep(0,111)
alpha2n <- rep(0,111)
beta1n <- rep(0,111)
beta2n <- rep(0,111)

# get posterior parameters for multiple positions of change point
for (t in 2:112){
  alpha1n[t-1] = 2+sum(y[1:t-1])
  beta1n[t-1] = 1+t-1
  alpha2n[t-1] = 2+sum(y[t:112])
  beta2n[t-1] = 1+(112-t)
}

# calculate probability of each year to be a changepoint
lpT <- rep(0, 111)
for (t in 1:111){
  lpT[t] = log(114-t)*(-2-sum(y[t:112]))+lgamma(sum(y[t:112])+2)+
    log(t)*(-2-sum(y[1:t-1]))+lgamma(sum(y[1:t-1])+2)
}
lpT <- lpT-max(lpT)
pT = exp(lpT)
pT = pT/sum(pT)
barplot(pT)

layout(matrix(1:9, ncol=3, nrow=3, byrow=T))

# sample lambdas using updated parameters
dens = matrix(0, nrow=2048, ncol=111)
dens2 = matrix(0, nrow=2048, ncol=111)
densll = matrix(0, nrow=128, ncol=128)
xxs <- seq(0,5,length.out=2048)
xss <- seq(0,5,length.out=128)

for (i in 1:111){
  for (j in 1:2048){
    dens[j,i] <- pT[i]*dgamma(xxs[j],alpha1n[i],beta1n[i])
    dens2[j,i] <- pT[i]*dgamma(xxs[j],alpha2n[i],beta2n[i])
  }
}
dl1<-rep(0,2048)
dl2<-rep(0,2048)
for (i in 1:2048){
  dl1[i] = sum(dens[i,])
  dl2[i] = sum(dens2[i,])
}

# marginal distributions of lambdas
plot(x=xxs, y=dl1, type='l')
plot(x=xxs, y=dl2, type='l')
z2 = dl2%*%t(dl1)
image(xxs,xxs,z2,col=mycol,useRaster = T)

for (i in 1:128){
  for (j in 1:128){
    for (t in 1:111){
      densll[i,j] = densll[i,j]+pT[t]*dgamma(xss[i],alpha1n[t],beta1n[t])*dgamma(xss[j],alpha2n[t],beta2n[t])
    }
  }
}


pred.t <- rep(0,111)
pred2 <- matrix(0, nrow=2048, ncol=112)
for (sample in 1:5){
  for (year in 1:112){
    for (t in 2:112){
      if (year<t){pred.t[t] <- rpois(1,dl1[sample])}
      else {pred.t[t] <- rpois(1,dl2[sample])}
      print(t, rpois(1,dl1[sample]))
    }
    indpred <- sum(pred.t)
    pred[sample, year] <- indpred
  }
}

cp <- apply(pred,1,cumsum) # cumulative 
cp <- t(cp)
medp <- rep(0,112)
twoh <- rep(0,112)
nins <- rep(0,112)
for (year in 1:112){
  medp[year]<-quantile(cp[,year],probs = c(0.5))
  twoh[year]<-quantile(cp[,year],probs = c(0.025))
  nins[year]<-quantile(cp[,year],probs = c(0.975))
}
plot(1851:1962,medp,type='l',ylim=c(1,220))
lines(1851:1962, twoh, col='red')
lines(1851:1962, nins, col='red')
lines(1851:1962, cumsum(y), col='orange')
