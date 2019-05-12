## Demean covariates
gulls$yellowlegged <- demean(gulls$yellowlegged)
gulls$year         <- demean(gulls$year)

#### DATA ####
## Dataset
n    <- nrow(gulls)
aud  <- gulls$audouin
year <- gulls$year

## Prior
b0.mu  <- 0
b1.mu  <- 0
b0.tau <- 0.01
b1.tau <- 0.01

## data list

data  <- list(n = n, aud = aud, year = year,
             b0.mu = b0.mu, b0.tau = b0.tau, 
             b1.mu = b1.mu, b1.tau = b1.tau)


#### MODEL ####

modstr.1b <- "model{

b0 ~ dnorm(b0.mu, b0.tau)
b1 ~ dnorm(b1.mu, b1.tau)

# likelihood


for (i in 1:n){
    aud[i] ~ dpois(mu[i])
    log(mu[i]) = b0 + b1*(year[i])
  }

}"


m1.b <- jags.model(textConnection(modstr.1b), data = data, n.chains = 3)

update(m1.b, 20000)


#### RESULTS ####
res.1b <- coda.samples(m1.b, c('b0', 'b1'), n.iter = 20000, thin = 4) ## thin to make following steps quicker

## COMBINE CHAINS 
combres.1b <- combine.mcmc(res.1b)


#### CONVERGENCE, ESS ####

png('Q1/Q1bConvergence.png', width = 1200, height = 200)

par(mfrow = c(1, 4))

traceplot(res.1b)
mcmc.dens(combres.1b)
par(mfrow=c(1,1))

dev.off()

autocorr.plot(combres.1b)
effectiveSize(combres.1b)
gelman.diag(res.1b)
gelman.plot(res.1b)

#### CONSISTENCY CHECK ####

## Change priors of b0 and b1, and then compare quantiles
## Reduced iterations for speed

modstr.1b <- modstr.1b

taus <- c(0.001, 0.01, 0.1, 1, 10)

sumlist <- list()

# Loop over index
for (tau.idx in 1:length(taus)) {

  # Extract the prior being tested
  tau.prior   <- taus[tau.idx]
  # Create label for the results to add to sumlist. of the form 'tau=0.01'
  listlabel   <- paste('tau', '=', tau.prior, sep='')
  
  # Alter data to have new priors, and create model
  data$b0.tau <- tau.prior
  data$b1.tau <- tau.prior
  m1.b.check  <- jags.model(textConnection(modstr.1b), 
                            data = data, 
                            n.chains = 1)
  # Update and extract results, create summary object from results
  update(m1.b.check, 20000)
  
  res.1b.check <- coda.samples(m1.b.check, c('b0', 'b1'), n.iter = 30000, thin = 4)
  sum.check    <- summary(res.1b.check)
  
  # Store results in sumlist
  sumlist[[listlabel]]       <- sum.check$quantiles
}

# View results
sumlist

#### REPORT RESULTS ####

## EXTRACT RESULTS AS SUMMARY ##

restab.1b <- results.table(combres.1b)

png('Q1/Q1bResults.png', width = 720, height = 70)
grid.table(restab.1b)
dev.off()



