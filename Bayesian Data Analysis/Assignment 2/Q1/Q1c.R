#### DATA RESPECIFICATION ####

data$b0.tau <- 0.01
data$b1.tau <- 0.01

#### MODEL ####

modstr.1c <- "model{


### Priors on beta
b0 ~ dnorm(b0.mu, b0.tau)
b1 ~ dnorm(b1.mu, b1.tau)

### Hyperprior information
sigma.epsilon ~ dunif(0, 10)
tau.epsilon = pow(sigma.epsilon, -2)



# likelihood
for (i in 1:n){
    aud[i] ~ dpois(mu[i])
    log(mu[i]) = b0 + b1*(year[i]) + epsilon[i]
    epsilon[i] ~ dnorm(0, tau.epsilon)
  }

}"

m1.c <- jags.model(textConnection(modstr.1c), data = data, n.chains = 3)

update(m1.c, 100000)


## 5:20 mins for 1000000 iterations, 3 chains
# Thin for ease of following steps
start_time <- Sys.time()
res.1c <- coda.samples(m1.c, 
                       c('b0', 'b1', 'sigma.epsilon'), 
                       n.iter = 1000000,
                       thin = 400)

end_time <- Sys.time()
end_time - start_time


# Combine results to 1 chain
combres.1c <- combine.mcmc(res.1c)


#### CHECK HOW MUCH TO THIN ####
# only run if this is 1

runthincheck <- 0

if (runthincheck == 1){

thincheck(res.1c, 1, 1000, 50, dim(res.1c[[1]])[1])
abline(v = c(100, 200, 300, 400, 500), col = c('red', 'blue', 'green', 'purple', 'pink'))

} 




#### CONVERGENCE, ESS ####

### TRACE, DENSITY PLOTS ###

png('Q1/Q1cConvergence.png', width = 1200, height = 270)
# 2 x 3 plot window
# 2: Trace and density
# 3: Parameters
par(mfrow = c(2, 3))

# All the trace plots
traceplot(res.1c)

# Density plot of each parameter
# Check functions for documentation
mcmc.dens(combres.1c)
par(mfrow=c(1,1))

dev.off()


### AUTOCORR, GELMAN, ESS ###
autocorr.plot(combres.1c)
effectiveSize(combres.1c)
gelman.diag(res.1c)
gelman.plot(res.1c)


#### REPORT RESULTS ####

## EXTRACT RESULTS AS SUMMARY ##
sum.1c <- summary(combres.1c)


## REPORT TABLE ##

restab.1c <- results.table(combres.1c)

png('Q1/Q1cResults.png', width = 750, height=85)
grid.table(restab.1c)
dev.off()


