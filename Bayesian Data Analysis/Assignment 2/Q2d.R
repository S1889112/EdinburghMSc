### Get common function inputs for both farms

# Data matrix and coefficients
# 1 (intercept), age, rain, permeability, slope
# age = 0 since assuming mean age (mean(age) - mean(age)) since demeaned variable
X <- cbind(1, age=0, rain, perm, slop)
B <- combres.2c[, c('b0', 'b1', 'b3', 'b4', 'b6')]
Bt <- t(as.matrix(B))

#### Farm 1 ####
ID <- 1

# Get farm 1 matrix
X.2d.1 <- X[cows$farmID == ID, ]
# Extract 1st row, since all values same for a given farm
xt.2d.1 <- t(as.matrix(X.2d.1[1, ]))

# Extract farm-specific effect. 
a.2d.1 <- combres.2c[, 'alpha[1]']

probs.2d.1 <- farmprobs(xt = xt.2d.1, Bt = Bt, alpha = a.2d.1)
post.2d.1  <- results.post(1, probs.2d.1, 3)

#### Farm 6 ####

ID <- 6

# Get farm 1 matrix
X.2d.6 <- X[cows$farmID == ID, ]
# Extract 1st row, since all values same for a given farm
xt.2d.6 <- t(as.matrix(X.2d.6[1, ]))

# Extract farm-specific effect. 
a.2d.6 <- combres.2c[, 'alpha[6]']

probs.2d.6 <- farmprobs(xt = xt.2d.6, Bt = Bt, alpha = a.2d.6)
post.2d.6  <- results.post(6, probs.2d.6, 3)

#### SUMMARISE RESULTS ####

## Combined posterior summary ##

Q2d.results <- rbind(post.2d.1, post.2d.6)
rownames(Q2d.results) <- paste(rownames(Q2d.results), '(Rand)') 
Q2d.results <- Q2d.results[, c('Mean', '95% CI', 'P(Epidemic)')]

##### REPORT RESULTS #####
png('Q2/Q2dResultsFARM.png', height = 80, width = 300)
grid.table(Q2d.results)
dev.off()

## Histograms ##


png('Q2/Q2dHists.png', width = 800, height = 300)
par(mfrow = c(1, 2))
hist(probs.2d.1, main = 'Histogram of Farm 1 Proportions', xlab = 'Farm 1 Proportion')
abline(v = c(post.2d.1[, '2.5%'], post.2d.1[, '97.5%'], post.2d.1[, 'Mean']), 
       col = c('Red', 'Red', 'Blue'), lwd = 2)

hist(probs.2d.6, main = 'Histogram of Farm 6 Proportions', xlab = 'Farm 6 Proportion')
abline(v = c(post.2d.6[, '2.5%'], post.2d.6[, '97.5%'], post.2d.6[, 'Mean']), 
       col = c('Red', 'Red', 'Blue'), lwd = 2)
par(mfrow = c(1,1))
dev.off()
