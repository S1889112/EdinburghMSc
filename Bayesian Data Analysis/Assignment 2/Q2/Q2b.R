## ===========================================================================##
#################################### 2b: Mod ###################################
## ===========================================================================##


#### Extract as vectors ####

para <- cows$parasite

### Fixed ###
# Specific #
age    <- demean(cows$age)

# Environment #
temp   <- demean(cows$temp)
rain   <- demean(cows$rain)
perm   <- demean(cows$permeab)
height <- demean(cows$hight)
slop   <- demean(cows$slope)

# Random
farm <- cows$farmID


#### Data ####
## Dataset ##
n      <- nrow(cows)
J      <- max(farm)
para   <- para
age    <- age
temp   <- temp
rain   <- rain
perm   <- perm
height <- height
slop   <- slop
farm   <- farm

## Prior ##

### Same priors for each beta
beta.mu  <- 0
beta.tau <- 0.01

## Hyperpriors ##
#  Priors on the random parameter alpha
sig.alpha.ub <- 20

## DATA LIST ##

data <- list(n = n, J = J,                                         # Loop idx
             para = para, age = age, temp = temp, rain = rain,     # Covariates
             perm = perm, height = height, slop = slop, 
             farm = farm, 
             beta.mu = beta.mu, beta.tau = beta.tau,               # Priors
             sig.alpha.ub = sig.alpha.ub) # Hyperpriors


#### MODEL ####
modstr.2b <- "model{
  
  # Likelihood
  for (i in 1:n) {
     para[i] ~ dbern(p[i])
     # alpha is the random farm-specific intercept. 
     logit(p[i]) = b0 + alpha[farm[i]] + b1*age[i] + b2*temp[i] + b3*rain[i] + 
                   b4*perm[i] + b5*height[i] + b6*slop[i]
  }

  # Priors
  b0 ~ dnorm(beta.mu, beta.tau)
  b1 ~ dnorm(beta.mu, beta.tau)
  b2 ~ dnorm(beta.mu, beta.tau)
  b3 ~ dnorm(beta.mu, beta.tau)
  b4 ~ dnorm(beta.mu, beta.tau)
  b5 ~ dnorm(beta.mu, beta.tau)
  b6 ~ dnorm(beta.mu, beta.tau)
  
  for (j in 1:J){
    alpha[j] ~ dnorm(0, tau.alpha)
  }

  # Hyperpriors #
  sig.alpha ~ dunif(0, sig.alpha.ub)
  tau.alpha = pow(sig.alpha, -2)


}"

m.2b <- jags.model(textConnection(modstr.2b), data = data, n.chains = 3)


var.names <- c('b0','b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'alpha', 'mu.alpha', 'sig.alpha')



update(m.2b, 50000)


# 12m: 150000 iterations
# 8.7m: 125000?
start_time <- Sys.time()

res.2b <- coda.samples(m.2b, variable.names = var.names, n.iter = 125000, thin = 100)

end_time <- Sys.time()
end_time - start_time

## Combine ##
# From runjags
combres.2b <- combine.mcmc(res.2b)



#### CHECK HOW MUCH TO THIN ####
# only run if this is 1

runthincheck <- 0

if (runthincheck == 1){
  
  thincheck(res.2b, 'b2', 500, 50, dim(res.2b[[1]])[1])
abline(v = c(100, 200, 300, 400, 500), col = c('red', 'blue', 'green', 'purple', 'pink'))
  
} 






#### CONVERGENCE ####
gelman.diag(res.2b) # All 1, upper CI 1.02
effectiveSize(combres.2b) # Minimum of 1800

png('Q2/Q2bTrace.png', width = 1200, height = 800)
par(mfrow = c(5, 6))
traceplot(res.2b)
par(mfrow=c(1,1))
dev.off()



#### RESULTS ####

restab.2b <- results.table(combres.2b)
png('Q2/Q2bResults.png', width = 800, height = 600)
grid.table(restab.2b)
dev.off()


#### Further ####

mean(combres.2b[, 'b1'] > 0) 








