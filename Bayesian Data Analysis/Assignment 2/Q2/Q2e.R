modstr.2e <- "model{


# Likelihood
  for (i in 1:n) {
      para[i] ~ dbern(p[i])
      # alpha is the FIXED farm-specific intercept. 
      logit(p[i]) = alpha[farm[i]] + b1*age[i] + 
      #b2*temp[i] + 
      b3*rain[i] + b4*perm[i] + 
      #b5*height[i] + 
      b6*slop[i]
  }
  
  # Priors
  b1 ~ dnorm(beta.mu, beta.tau)
  #b2 ~ dnorm(beta.mu, beta.tau)
  b3 ~ dnorm(beta.mu, beta.tau)
  b4 ~ dnorm(beta.mu, beta.tau)
  #b5 ~ dnorm(beta.mu, beta.tau)
  b6 ~ dnorm(beta.mu, beta.tau)
  
  for (j in 1:J){
    alpha[j] ~ dnorm(beta.mu, beta.tau)
  }
  
}"



mod.2e <- jags.model(textConnection(modstr.2e), data = data, n.chains = 3)

update(mod.2e, 100000)

var.names.2e <- c('b0', 'b1', 'b3', 'b4', 'b6', 'alpha')


start <- Sys.time()
res.2e <- coda.samples(mod.2e, variable.names = var.names.2e, n.iter = 400000, 
                       thin = 200)
Sys.time() - start

combres.2e <- combine.mcmc(res.2e)

#### CONVERGENCE ####
effectiveSize(res.2e)

png('Q2/Q2eTrace.png', width = 1200, height=800)
par(mfrow = c(4, 6))
traceplot(res.2e)
par(mfrow=c(1,1))
dev.off()

gelman.diag(res.2e)


#### RESULTS ####
restab.2e <- results.table(combres = combres.2e)


##### REPORT RESULTS OF PARAMETERS #####
png('Q2/Q2eResults.png', width = 800, height = 600)
grid.table(restab.2e)
dev.off()



#### FARM SPECIFIC ####

## Recall objects of interest:
# X: Same rows are used, so just copy
# No b0, so remove the first element with '-1' as column index
xt.2e.1 <- t(as.matrix(xt.2d.1[-1]))
xt.2e.6 <- t(as.matrix(xt.2d.6[-1]))

# B, alpha: Need to extract new parameters
B  <- combres.2e[, c('b1', 'b3', 'b4', 'b6')]
Bt <- t(as.matrix(B))
  
#### FARM 1
ID <- 1

# Extract farm-specific effect. 
a.2e.1 <- combres.2e[, 'alpha[1]']

probs.2e.1 <- farmprobs(xt = xt.2e.1, Bt = Bt, alpha = a.2e.1)
post.2e.1  <- results.post(1, probs.2e.1, 5)


#### FARM 6

ID <- 6

# Extract farm-specific effect. 
a.2e.6 <- combres.2e[, 'alpha[6]']

probs.2e.6 <- farmprobs(xt = xt.2e.6, Bt = Bt, alpha = a.2e.6)
post.2e.6  <- results.post(6, probs.2e.6, 5)

#### SUMMARISE RESULTS ####

## Combined posterior summary ##

Q2e.results <- rbind(post.2e.1, post.2e.6)
rownames(Q2e.results) <- paste(rownames(Q2e.results), '(Fixed)') 
Q2e.results <- Q2e.results[, c('Mean', '95% CI', 'P(Epidemic)')]

##### REPORT RESULTS #####
png('Q2/Q2eResultsFARM.png', height = 80, width = 340)
grid.table(Q2e.results)
dev.off()

## Histograms ##


png('Q2/Q2eHists.png', width = 800, height = 300)
par(mfrow = c(1, 2))
hist(probs.2e.1, main = 'Histogram of Farm 1 Proportions', xlab = 'Farm 1 Proportion')
abline(v = c(post.2e.1[, '2.5%'], post.2e.1[, '97.5%'], post.2e.1[, 'Mean']), 
       col = c('Red', 'Red', 'Blue'), lwd = 2)

hist(probs.2e.6, main = 'Histogram of Farm 6 Proportions', xlab = 'Farm 6 Proportion')
abline(v = c(post.2e.6[, '2.5%'], post.2e.6[, '97.5%'], post.2e.6[, 'Mean']), 
       col = c('Red', 'Red', 'Blue'), lwd = 2)
par(mfrow = c(1,1))
dev.off()


