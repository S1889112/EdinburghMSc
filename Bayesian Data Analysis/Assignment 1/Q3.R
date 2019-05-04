## Load Data

library(rjags)
library(e1071)
library(gridExtra)


data("rock")
area  <- rock$area
peri  <- rock$peri
perm  <- rock$perm
shape <- rock$shape

#### Define Functions ####

#### Plotting ####


data.plots <- function(yname, xnames, data, mains = c('')) {
  i <- 0
  for (name in xnames) {
    i = i + 1
    
    scatter.smooth(data[, yname] ~ data[, name],xlab=name, ylab = '',main=mains[i])
   # plot(data[, name], data[, yname], xlab = name, ylab = '', main = mains[i])
    #abline(lm(data[, yname] ~ data[, name]))
  }
}

#### Convergence ####

#### ESS
ESSplot <- function(sample.obj) {
  ESS <- data.frame(effectiveSize(sample.obj[[1]]))
  ESS <- data.frame(variable = row.names(ESS), ESS, row.names = NULL)
  colnames(ESS) <- c('Variable', 'ESS')
  ggplot(data = ESS, aes(x = Variable, y = ESS)) +
    geom_bar(stat = 'Identity') +
    ggtitle('Effective Sample Size for Variables')
}
#### GELMAN.DIAG
gelman.diag.df <- function(sample){
  
  a <- gelman.diag(sample)
  a <- round(a$psrf, 2)
  grid.table(a)
  
}

## ===========================================================================##
#################################### RESIDUALS##################################
## ===========================================================================##

fitted.blr <- function(resmat, X, B, sigma  = NULL, resids = FALSE){
  # resmat : Results matrix (e.g. as.matrix(samp[[1]]))
  # X      : X matrix
  # B      : Beta matrix
  # sigma  : Standard deviation (only needed for resids = TRUE)
  # resids : If true, return studentised residuals. 
  
  # Observations and number of iterations
  n <- nrow(X)
  niterf <- nrow(resmat)
  
  # Get individual vectors
  b1    <- B[, 1]
  b2    <- B[, 2]
  b3    <- B[, 3]
  b4    <- B[, 4]
  # Hat matrix
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  
  # Y = XB
  fittedvalues <- X%*% t(cbind(b1, b2, b3, b4))
  
  if (resids == TRUE) {
    
    studentisedresid=matrix(0, nrow=n, ncol=niterf)
    
    # For each posterior sample
    for(l in 1:niterf){
      # For each observation
      for(i in 1:n){
        # Residual/sigma * 
        studentisedresid[i,l]=(l.perm[i]-fittedvalues[i,l])/(sigma[l]*sqrt((1-diag(H)[i])))
      }
    }
    
    return(studentisedresid)
  } else {
    return(fittedvalues)
  }
}
  
## ===========================================================================##
#################################### SCALING ###################################
## ===========================================================================##

standardise <- function(x) {(x - mean(x))/sd(x)}

## ===========================================================================##
################################## EXPLORATORY #################################
## ===========================================================================##

#### Univariate ####


png('Q3/3aEDAHist.png', height = 400, width = 600)
par(mfrow = c(2,2))
hist(perm,  main = 'Permeability Histogram', xlab = 'Permeability')
hist(area,  main = 'Area Histogram',         xlab = 'Area')
hist(peri,  main = 'Perimeter Histogram',    xlab = 'Perimeter')
hist(shape, main = 'Shape Histogram',        xlab = 'Shape')
par(mfrow = c(1,1))
dev.off()

#### Transformations ####

l.perm       <- log(perm)
area.norm    <- standardise(area)
peri.norm    <- standardise(peri)
shape.norm   <- standardise(shape)

rock.t <- as.data.frame(cbind(area.norm, peri.norm, shape.norm,l.perm))

#### Bivariate ####

png('Q3/3aEDABiv.png', height = 400, width = 600)
par(mfrow=c(2,2))
data.plots('l.perm', c('peri.norm', 'area.norm', 'shape.norm'), data=rock.t, 
           mains = c('Permeability vs Perimeter', 'Permeability vs Area', 
                     'Permeability vs Shape'))
par(mfrow=c(1,1))
dev.off()

#### Frequentist ####

freq <- lm(l.perm ~ peri.norm + area.norm + shape.norm, data = rock.t)

#### Results:
png('Q3/3aEDAfreqcoef.png', height=102, width = 310)
grid.table(round(summary(freq)$coef, 3))
dev.off()


### Save residual plot
png('Q3/3aEDAfreqres.png', height = 300, width = 1200)
par(mfrow = c(1, 4))
plot(freq)
par(mfrow = c(1,1))
dev.off()


## ===========================================================================##
################################## REGRESSION ##################################
## ===========================================================================##

## Define hyperparameters
beta.mu.prior   <- rep(0, 4)
beta.tau.prior  <- diag(1/100000, 4)
tau.shape.prior <- 0.1
tau.rate.prior  <- 0.1


## Data defining
x     <- cbind(1, rock.t$peri.norm, rock.t$area.norm, rock.t$shape.norm)
y     <- rock.t$l.perm
n     <- nrow(x)

## Data list
data <- list(y = y, x = x, n = n, # The dataset 
             beta.mu.prior   = beta.mu.prior, 
             beta.tau.prior  = beta.tau.prior, 
             tau.shape.prior = tau.shape.prior, tau.rate.prior = tau.rate.prior) 

model_string <-   
  "model {

# Likelihood
for (i in 1:n) {
y[i] ~ dt(mu[i], tau, nu)
mu[i] = inprod(beta[], x[i, ])
}

# Prior defined on vector
beta ~ dmnorm(beta.mu.prior, beta.tau.prior)

## Prior on tau
tau ~ dgamma(tau.shape.prior, tau.rate.prior)

## Prior on nu (Fixed value now, will change later)
nu = 5

## Convert precision to variance
sigma2 <- 1/tau

}"

model <- jags.model(textConnection(model_string), 
                    data = data, n.chains = 3)

update(model, 100000)

samp <- coda.samples(model = model, variable.names = c('beta', 'sigma2'), n.iter = 250000, thin = 50)


## Result Summary
summary(samp)

## Save BGR plots
png('Q3/3bGelman.png', height = 600, width = 700)
gelman.plot(samp)
dev.off()


## Save trace plots

png('Q3/3bTrace.png', height = 600, width = 700)
par(mfrow = c(3, 2))
traceplot(samp)
dev.off()

## Save autocorr plots

png('Q3/3bAuto.png', height = 600, width = 700)
par(mfrow = c(3, 2))
autocorr.plot(samp[[1]])
dev.off()

## Effective Sample Size

effectiveSize(samp[[1]])

## Bayes-Freq comparison

# Keep estimate and p-value
freq.mat <- summary(freq)$coef[, c(1,2,4)]

# Bayes
b.mean <- apply(samp[[1]], 2, mean)[1:4]
b.sd   <- apply(samp[[1]], 2, sd)[1:4]
b.CI <- apply(samp[[1]],2,  quantile, probs = c(0.025, 0.975))[,1:4]
bayes.mat <- rbind(b.mean, b.sd, b.CI)
bayes.mat <- t(bayes.mat)

combined.mat <- round(cbind(freq.mat, bayes.mat),3)
colnames(combined.mat) <- c('Classical Est', 'Classical SE', 'p-value', 'Bayes Mean', 'Bayes SD', 'Bayes 2.5%', 'Bayes 97.5%')

## SAVE
png('Q3/3bBayesFreqComp.png', width = 600, height = 100)
grid.table(combined.mat)
dev.off()




## ===========================================================================##
################################## SENSITIVITY #################################
## ===========================================================================##

## Define hyperparameters
beta.mu.prior   <- rep(0, 4)
beta.tau.prior  <- diag(1/1000, 4)
tau.lower       <- 0
tau.upper       <- 100000
nu.prior        <- 0.1

## Data defining
x     <- cbind(1, rock.t$peri.norm, rock.t$area.norm, rock.t$shape.norm)
y     <- rock.t$l.perm
n     <- nrow(x)

## Data list
data <- list(y = y, x = x, n = n, # The dataset 
             beta.mu.prior   = beta.mu.prior, 
             beta.tau.prior  = beta.tau.prior,
             tau.lower = tau.lower, tau.upper = tau.upper, 
             nu.prior = nu.prior)

model_string <-   
  "model {

# Likelihood
for (i in 1:n) {
y[i] ~ dt(mu[i], tau, nu)
mu[i] = inprod(beta[], x[i, ])
}

# Prior defined on vector
beta ~ dmnorm(beta.mu.prior, beta.tau.prior)

## Prior on tau
tau ~ dunif(tau.lower, tau.upper)

## Prior on nu (Fixed value now, will change later)
nu ~ dgamma(nu.prior, nu.prior)

## Convert precision to variance
sigma2 <- 1/tau

}"

model <- jags.model(textConnection(model_string), 
                    data = data, n.chains = 3)

update(model, 100000)

samp.sens <- coda.samples(model = model, variable.names = c('beta', 'sigma2', 'nu'), n.iter = 250000, thin = 50)



## ===========================================================================##
######################## SENSITIVITY DIAGNOSTIC PLOTS ##########################
## ===========================================================================##

## Result Summary
summary(samp.sens)

## Save BGR plots
png('Q3/3cGelman.png', height = 600, width = 700)
gelman.plot(samp.sens)
dev.off()


## Save trace plots

png('Q3/3cTrace.png', height = 600, width = 700)
par(mfrow = c(3, 2))
traceplot(samp.sens)
dev.off()

## Save autocorr plots

png('Q3/3cAuto.png', height = 600, width = 700)
par(mfrow = c(3, 2))
autocorr.plot(samp.sens[[1]])
dev.off()

## Effective Sample Size

effectiveSize(samp.sens[[1]])


## COMPARISON TO PREVIOUS PRIOR

## PREVIOUS PRIOR: MEAN, SD, QUANTILES

previous.mean <- apply(samp[[1]], 2, mean)
previous.sd   <- apply(samp[[1]], 2, sd)
previous.qnt  <- apply(samp[[1]], 2, quantile, prob = c(0.25, 0.5, 0.75))
previous.mat  <- rbind(previous.mean, previous.sd, previous.qnt)
previous.mat  <- t(previous.mat)
colnames(previous.mat) <- c('Orig. Mean', 'Orig. Sd', '25%', '50%', '75%')
previous.mat


## NEW PRIOR
new.mean <- apply(samp.sens[[1]], 2, mean)
new.sd   <- apply(samp.sens[[1]], 2, sd)
new.qnt  <- apply(samp.sens[[1]], 2, quantile, prob = c(0.25, 0.5, 0.75))
new.mat  <- rbind(new.mean, new.sd, new.qnt)[, c(1,2,3,4,6)]
new.mat  <- t(new.mat)
colnames(new.mat) <- c('New Mean', 'New Sd', '25%', '50%', '75%')

## Comparison matrix
prior.comp.mat <- round(cbind(previous.mat, new.mat), 3)

## Save
png('Q3/3cPriorComparison.png', height = 150, width = 605)
grid.table(prior.comp.mat)
dev.off()

## ===========================================================================##
############################# SENSITIVITY (NORMAL) #############################
## ===========================================================================##

## Define hyperparameters
beta.mu.prior   <- rep(0, 4)
beta.tau.prior  <- diag(1/1000, 4)
tau.lower       <- 0
tau.upper       <- 100000


## Data defining
x     <- cbind(1, rock.t$peri.norm, rock.t$area.norm, rock.t$shape.norm)
y     <- rock.t$l.perm
n     <- nrow(x)

## Data list
data <- list(y = y, x = x, n = n, # The dataset 
             beta.mu.prior   = beta.mu.prior, 
             beta.tau.prior  = beta.tau.prior,
             tau.lower = tau.lower, tau.upper = tau.upper)

model_string <-   
  "model {

# Likelihood
for (i in 1:n) {
y[i] ~ dnorm(mu[i], tau)
mu[i] = inprod(beta[], x[i, ])
}

# Prior defined on vector
beta ~ dmnorm(beta.mu.prior, beta.tau.prior)

## Prior on tau
tau ~ dunif(tau.lower, tau.upper)

## Convert precision to variance
sigma2 <- 1/tau

}"

model <- jags.model(textConnection(model_string), 
                    data = data, n.chains = 3)

update(model, 100000)

samp.sens.norm <- coda.samples(model = model, variable.names = c('beta', 'sigma2'), n.iter = 250000, thin = 50)

##### NORMAL DISTRIBUTION COMPARISON

## PREVIOUS PRIOR: MEAN, SD, QUANTILES

previous.mean <- apply(samp[[1]], 2, mean)
previous.sd   <- apply(samp[[1]], 2, sd)
previous.qnt  <- apply(samp[[1]], 2, quantile, prob = c(0.25, 0.5, 0.75))
previous.mat  <- rbind(previous.mean, previous.sd, previous.qnt)
previous.mat  <- t(previous.mat)
colnames(previous.mat) <- c('Orig. Mean', 'Orig. Sd', '25%', '50%', '75%')
previous.mat


## NEW PRIOR
new.mean.norm <- apply(samp.sens.norm[[1]], 2, mean)
new.sd.norm   <- apply(samp.sens.norm[[1]], 2, sd)
new.qnt.norm  <- apply(samp.sens.norm[[1]], 2, quantile, prob = c(0.25, 0.5, 0.75))
new.mat.norm  <- rbind(new.mean, new.sd, new.qnt)
new.mat.norm  <- t(new.mat)
colnames(new.mat.norm) <- c('New Mean', 'New Sd', '25%', '50%', '75%')

## Comparison matrix
prior.comp.mat <- round(cbind(previous.mat, new.mat), 3)

## Save
png('Q3/3cPriorComparisonNormal.png', height = 150, width = 605)
grid.table(prior.comp.mat)
dev.off()






