#### DATA RESPECIFICATION ####

# Already been demeaned
yellowlegged <- gulls$yellowlegged

# Add to data list
# Add after year, so next to covariates
#data <- append(data, list(yel = yellowlegged), 3)

data$yel <- yellowlegged
# Add priors to list
data$b2.mu  <- 0
data$b2.tau <- 0.01


#### MODEL ####

modstr.1d <- "model{

# Parameter priors

b0 ~ dnorm(b0.mu, b0.tau)
b1 ~ dnorm(b1.mu, b1.tau)
b2 ~ dnorm(b2.mu, b2.tau)

# Extra variation hyperprior details
# sigma uniformly distributed
# tau is inverse sigma squared (inverse variance)

sigma.epsilon ~ dunif(0, 10)
tau.epsilon <- pow(sigma.epsilon, -2)


# likelihood
for (i in 1:n){
  aud[i] ~ dpois(mu[i])
  log(mu[i]) = b0 + b1*(year[i]) + b2*(yel[i]) + epsilon[i]
    epsilon[i] ~ dnorm(0, tau.epsilon)
  }

}"


m1.d <- jags.model(textConnection(modstr.1d), data = data, n.chains = 3)

update(m1.d, 200000)


## RUNTIME: 2000000: 13m
## RUNTIME: 1500000: 10m
## RUNTIME: 1000000: 7m
start_time <- Sys.time()
res.1d <- coda.samples(m1.d, c('b0', 'b1', 'b2', 'sigma.epsilon'), 
                       n.iter = 1400000, thin = 500)

end_time <- Sys.time()
end_time - start_time

effectiveSize(res.1d)

### Combined results
combres.1d <- combine.mcmc(res.1d)

#### CHECK HOW MUCH TO THIN ####
runthincheck <- 0

if (runthincheck == 1){
  
  thincheck(res.1d, 3, 1000, 50, dim(res.1d[[1]])[1])
  abline(v = c(100, 200, 300, 400, 500), col = c('red', 'blue', 'green', 'purple', 'pink'))
  
} 





#### CONVERGENCE ####
## Save trace and densityplots
png('Q1/Q1dConvergence.png', width = 1200, height = 300)
par(mfrow = c(2, 4))
traceplot(res.1d)
mcmc.dens(combres.1d)
par(mfrow=c(1,1))
dev.off()

### AUTOCORR, GELMAN, ESS ###
autocorr.plot(combres.1d)
effectiveSize(combres.1d)
gelman.diag(res.1d)
gelman.plot(res.1d)



## REPORT TABLE ##

restab.1d <- results.table(combres.1d)

png('Q1/Q1dResults.png', width = 750, height=110)
grid.table(restab.1d)
dev.off()




