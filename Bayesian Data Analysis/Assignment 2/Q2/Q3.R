#### MODEL STRING ####

modstr.3 <- "model{


# Likelihood
for (i in 1:n) {
para[i] ~ dnorm(p[i], para.tau)

  p[i] = b0 + b1*age[i] +  
                b3*rain[i] + 
                b4*perm[i] + 
                b6*slop[i]

}     

# Priors
b0 ~ dnorm(beta.mu, beta.tau)
b1 ~ dnorm(beta.mu, beta.tau)
b3 ~ dnorm(beta.mu, beta.tau)
b4 ~ dnorm(beta.mu, beta.tau)
b6 ~ dnorm(beta.mu, beta.tau)
para.tau ~ dgamma(0.01, 0.01)

}"

m.3 <- jags.model(textConnection(modstr.3), data = data, n.chains = 3)

### DIC ###
update(m.3, 50000)
dic.3 <- dic.samples(m.3, 150000)

dic.3


#### MODEL ####
res.3 <- coda.samples(m.3, c('b0', 'b1',  'b3', 'b4', 'b6'), n.iter = 50000, thin = 10)

combres.3 <- combine.mcmc(res.3)


#### CONVERGENCE ####

png('Q2/Q3Trace.png', width = 700, height = 400)
par(mfrow = c(2, 3))
traceplot(res.3)
par(mfrow = c(1,1))
dev.off()

#### RESULTS ####
restab.3 <- results.table(combres.3, dig = 4)

png('Q2/Q3Results.png', width = 750, height = 150)
grid.table(restab.3)
dev.off()

### COMP ###

mean(combres.3[, 'b0'] > 0)
mean(combres.3[, 'b1'] > 0)
mean(combres.3[, 'b3'] > 0)
mean(combres.3[, 'b4'] < 0)
mean(combres.3[, 'b6'] < 0)

mean(combres.2c[, 'b0'] > 0)
mean(combres.2c[, 'b1'] > 0)
mean(combres.2c[, 'b3'] > 0)
mean(combres.2c[, 'b4'] < 0)
mean(combres.2c[, 'b6'] < 0)


### PREDICTIONS ###
X  <- cbind(1, age, rain, perm, slop)
Bt <- t(as.matrix(combres.3))

lc <- X%*%Bt
pr <- apply(lc, 1, mean)
table(pr>0.5, para)



