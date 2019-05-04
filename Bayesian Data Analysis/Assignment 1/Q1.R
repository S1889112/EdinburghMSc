#### Load

library(rjags)
library(gridExtra)

##### 1a (Initial) #####

y <- c(-0.566,  3.74, 5.55, -1.90, -3.54, 
        5.16 , -1.76, 4.08,  4.62,  0.732)
n <- length(y)
mu0  <- 1
prec <- 1/30
prec0 <- 4

ybar <- mean(y)


norm.post.calc <- function(y, prec, prec.h, mu.h){
  # y: Data
  # prec:   Known precision
  # prec.h: Hyperparameter for precision of mu
  # mu.h:    Hyperparameter for mean of mu
  ybar <- mean(y)
  n    <- length(y)
  
  ## Mean:
  post.mu <- (prec.h*mu.h + n*prec*ybar)/(prec.h + n*prec)
  ## Variance:
  post.var <- 1/(prec.h + n*prec)
  ## Return
  c(post.mu, post.var)
}

norm.post.calc(y, prec = prec, prec.h =  prec0, mu.h = mu0)


## ===========================================================================##
#################################### 1b: Calcs #################################
## ===========================================================================##

### New hyperparameters:
prec1  <- 1
mu1    <- 5

### Posterior hyp. of US prior

norm.post.calc(y, prec=prec, prec.h = prec1, mu.h = mu1)


### Weight estimation in JAGS
mus  <- c(mu0, mu1)
taus <- c(prec0, prec1)
pis  <- c(2/3, 1/3)


mod.1b <- "model{
  # Likelihood
  for (i in 1:n){
    y[i] ~ dnorm(mu, tau)
  }

  # Prior
  mu0 ~ dnorm(mus[1], taus[1])
  mu1 ~ dnorm(mus[2], taus[2])
  w   ~ dbinom(pis[1], 1)
  mu = w*mu0 + (1-w)*mu1

}"

data <- list(y=y, n=n, 
             mus = mus, taus = taus, pis = pis, 
             tau = 1/30)

model.1b <- jags.model(textConnection(mod.1b), n.chains = 3, data = data)
update(model.1b, 10000)
samp.1b <- coda.samples(model.1b, variable.names = c('mu', 'w'), n.iter = 20000)

## ===========================================================================##
################################# 1C: SAMPLES  #################################
## ===========================================================================##


## Hyperparameters on tau
a <- 0.01
b <- 0.01

## JAGS MODEL:

mod.1c <- "model{

## Likelihood

for (i in 1:n) {
  y[i] ~ dnorm(mu, tau)
}


## Prior on mu

mu1 ~ dnorm(mus[1], taus[1])
mu2 ~ dnorm(mus[2], taus[2])
I   ~ dbinom(pis[1], 1) # Choose distribution 1 with probability 1. 
mu = I*mu1 + (1-I)*mu2

## Prior on tau
tau ~ dgamma(a, b)

## Convert precision to variance
sig2 = 1/tau

}"

# Data to feed to JAGS

data <- list(y = y, n = n, 
             mus = mus, taus = taus, pis = pis,
             a   = a, b   = b)


# Compile and initialise model

model.1c <- jags.model(textConnection(mod.1c), n.chains = 5, data = data)

# Update: Select burn-in of 10000
update(model.1c, 10000)

# Get samples

res.1c <- coda.samples(model.1c, 
                    variable.names = c("mu", "sig2", 'I'), n.iter = 10000)


##### 1c: Posterior mean, median, lower and upper quartiles #####

chain1 <- res.1c[[1]]
summ.1c <- as.data.frame(t(rbind(apply(chain1, 2, mean), 
                   apply(chain1, 2, quantile))))
summ.1c[, c('0%', '100%')] <- NULL
colnames(summ.1c) <- c('Mean', 'Lower Quartile', 'Median', 'Upper Quartile')
rownames(summ.1c) <- c('Weight', 'Mu', 'SigmaSq')

#### Send to Image ###
png(filename = 'Q1/1cSummary.png', width=380,height=100) 
grid.table(round(summ.1c, 2)) 
dev.off() 

## ===========================================================================##
################################# 1D: UPDATED  #################################
## ===========================================================================##

## JAGS MODEL:

mod.1d <- "model{

## Likelihood

for (i in 1:n) {
y[i] ~ dnorm(mu, tau)
}


## Prior on mu

mu1 ~ dnorm(mus[1], taus[1])
mu2 ~ dnorm(mus[2], taus[2])
I   ~ dbinom(pis[1], 1) # Choose distribution 1 with probability 1. 
mu = I*mu1 + (1-I)*mu2

## Prior on tau
tau ~ dgamma(a, b)

## Convert precision to variance
sig2 = 1/tau

## PREDICTIONS
for (i in 1:5){
  ypred[i]~ dnorm(mu, tau)
}

}"

# Data to feed to JAGS

data <- list(y = y, n = n, 
             mus = mus, taus = taus, pis = pis,
             a   = a, b   = b)


# Compile and initialise model

model.1d <- jags.model(textConnection(mod.1d), n.chains = 5, data = data)

# Update: Select burn-in of 10000
update(model.1d, 20000)

# Get samples

res.1d <- coda.samples(model.1d, 
                       variable.names = c("mu", 'ypred'), 
                       n.iter = 20000)

## EXTRACT RELEVANT DATA
mus       <- res.1d[[1]][, 'mu']
ypred.mat <- as.matrix(res.1d[[1]][, -1])

## pr(mu < 1)
mean((mus < 1))
## 0.3834

## Pr(at least one in sample has negative)
neg.lc <- apply(ypred.mat, 1, FUN =function(x) sum(x<0) > 0)
prlo <-  sum(neg.lc)/length(neg.lc)
## 88%

mu.text <- intToUtf8(956)

label1 <- paste('P(', mu.text, '<1 | y)', sep = '')
label2 <- paste('P(>0 log concentrations negative | y)')

df.1d <- t(cbind(mean(mus<1), prlo))
df.1d <- signif(100*df.1d, 3)
df.1d <- apply(df.1d, 2, paste, '%', sep='')
rownames(df.1d) <- c(label1, label2)
colnames(df.1d) <- NULL
df.1d

### SAVE TABLE

png(filename = 'Q1/1dresults.png', width=300, height=40)
grid.table(df.1d)
dev.off()



## Can also be done binomially
1 - (sum(res.1d[[1]][, 'ypred[1]'] > 0)/20000)^5
## 89%
