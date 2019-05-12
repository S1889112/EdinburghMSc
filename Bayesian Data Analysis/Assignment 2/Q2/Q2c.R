#### TEMP REMOVED MODEL STRING ####

modstr.2c.rtemp <- "model{

  # Likelihood
  for (i in 1:n) {
para[i] ~ dbern(p[i])
# alpha is the random farm-specific intercept. 
logit(p[i]) = b0 + alpha[farm[i]] + b1*age[i] + 
             #b2*temp[i] + 
              b3*rain[i] + b4*perm[i] + b5*height[i] + b6*slop[i]
}

# Priors
b0 ~ dnorm(beta.mu, beta.tau)
b1 ~ dnorm(beta.mu, beta.tau)
#b2 ~ dnorm(beta.mu, beta.tau)
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

#### TEMP, RAIN REMOVED MODEL STRING ####

modstr.2c.rtemprain <- "model{


  # Likelihood
  for (i in 1:n) {
para[i] ~ dbern(p[i])
# alpha is the random farm-specific intercept. 
logit(p[i]) = b0 + alpha[farm[i]] + b1*age[i] + 
             #b2*temp[i] + b3*rain[i] + 
              b4*perm[i] + b5*height[i] + b6*slop[i]
}

# Priors
b0 ~ dnorm(beta.mu, beta.tau)
b1 ~ dnorm(beta.mu, beta.tau)
#b2 ~ dnorm(beta.mu, beta.tau)
#b3 ~ dnorm(beta.mu, beta.tau)
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

#### TEMP, HEIGHT REMOVED MODEL STRING ####

modstr.2c.rtempheight <- "model{


  # Likelihood
  for (i in 1:n) {
para[i] ~ dbern(p[i])
# alpha is the random farm-specific intercept. 
logit(p[i]) = b0 + alpha[farm[i]] + b1*age[i] + 
             #b2*temp[i] + 
              b3*rain[i] + b4*perm[i] + 
             #b5*height[i] + 
              b6*slop[i]
}

# Priors
b0 ~ dnorm(beta.mu, beta.tau)
b1 ~ dnorm(beta.mu, beta.tau)
#b2 ~ dnorm(beta.mu, beta.tau)
b3 ~ dnorm(beta.mu, beta.tau)
b4 ~ dnorm(beta.mu, beta.tau)
#b5 ~ dnorm(beta.mu, beta.tau)
b6 ~ dnorm(beta.mu, beta.tau)

for (j in 1:J){
  alpha[j] ~ dnorm(0, tau.alpha)
}

# Hyperpriors #
sig.alpha ~ dunif(0, sig.alpha.ub)
tau.alpha = pow(sig.alpha, -2)

}"

#### MODEL INITIALISATION ####

## Note: Should be warnings about unused variables in data

m2c.rtemp       <- jags.model(textConnection(modstr.2c.rtemp),       data = data, n.chains = 2)

m2c.rtemprain   <- jags.model(textConnection(modstr.2c.rtemprain),   data = data, n.chains = 2)

m2c.rtempheight <- jags.model(textConnection(modstr.2c.rtempheight), data = data, n.chains = 2)

#### BURN IN ####
update(m2c.rtemp,       20000)
update(m2c.rtemprain,   20000)
update(m2c.rtempheight, 20000)


#### DIC SAMPLES ####

n.iter <- 100000 ## 18 minutes

start <- Sys.time()
dic.full        <- dic.samples(m.2b,            n.iter = n.iter)
dic.rtemp       <- dic.samples(m2c.rtemp,       n.iter = n.iter)
dic.rtemprain   <- dic.samples(m2c.rtemprain,   n.iter = n.iter)
dic.rtempheight <- dic.samples(m2c.rtempheight, n.iter = n.iter)
end <- Sys.time()
end-start



dic.full.val        <- sum(dic.full$deviance)        + sum(dic.full$penalty)
dic.rtemp.val       <- sum(dic.rtemp$deviance)       + sum(dic.rtemp$penalty)
dic.rtemprain.val   <- sum(dic.rtemprain$deviance)   + sum(dic.rtemprain$penalty)
dic.rtempheight.val <- sum(dic.rtempheight$deviance) + sum(dic.rtempheight$penalty)

## Results matrix
dic.all <- cbind(dic.full.val, dic.rtemp.val, dic.rtemprain.val, dic.rtempheight.val)
colnames(dic.all) <- c('Full Model', 'Temp Removed', 'Temp, Rain Removed', 'Temp, Height Removed')
rownames(dic.all) <- c('DIC')
dic.all <- round(dic.all, 2)
dic.all <- as.data.frame(dic.all)



png('Q2/Q2cDIC.png', width = 500, height = 50)
grid.table(dic.all)
dev.off()

#### PART 2, PARAMETERS OF CHOSEN MODEL ####

## Recompile model with 3 chains
m2c.rtempheight   <- jags.model(textConnection(modstr.2c.rtempheight), n.chains = 3, data = data)

## Burn-in
update(m2c.rtempheight, 50000)

## Determine variables to keep (remove b2 and b5)
var.names <- c('b0', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'alpha', 'sig.alpha')
var.names.rtempheight <- var.names[!(var.names %in% c('b2', 'b5'))]


## Generate samples

start <- Sys.time()
res.2c <- coda.samples(m2c.rtempheight, var.names.rtempheight, n.iter = 150000, thin = 50)
end <- Sys.time()
end - start

combres.2c <- combine.mcmc(res.2c)


#### CONVERGENCE ####
## Traceplots
png('Q2/Q2cTrace.png', width = 1200, height=800)
par(mfrow = c(4, 6))
traceplot(res.2c)
par(mfrow=c(1,1))
dev.off()


effectiveSize(res.2c)
gelman.diag(res.2c)
gelman.plot(res.2c)

#### RESULTS ####

restab.2c <- results.table(combres.2c)
png('Q2/Q2cResults.png', width = 800, height = 600)
grid.table(restab.2c)
dev.off()

#### RESULTS EVALUATION ####
b6.2c <- combres.2c[, 'b6']
mean(b6.2c < 0.01 & b6.2c > -0.01)

#### RESULTS COMPARISON ####
restab.2b <- results.table(combres.2b)
restab.2b <- as.data.frame(restab.2b)
restab.2c <- as.data.frame(restab.2c)

### JOIN ###
joined <- merge(restab.2c, restab.2b, by = 0)

### Meandif

meandif <- cbind.data.frame(joined$Mean.x, joined$Mean.y)
meandif
View(meandif)

