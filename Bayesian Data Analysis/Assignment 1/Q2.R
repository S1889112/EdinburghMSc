## ===========================================================================##
####################################### 2 ######################################
## ===========================================================================##

## ===========================================================================##
#################################### 2B: Calcs #################################
## ===========================================================================##
prior.alpha <- 0.001
prior.beta  <- 0.001
y           <- 3
k           <- 3/12
post.alpha <- prior.alpha + y
post.beta  <- prior.beta  + k

# Mean, sd, 95\% CI
mean.2b <- post.alpha/post.beta
sd.2b   <- sqrt(post.alpha/(post.beta^2))
sci.2b <- c(qgamma(0.025, post.alpha, post.beta), 
         qgamma(0.975, post.alpha, post.beta))

# Posterior plot

x   <- seq(0, mean.2b+4*sd.2b, 0.1)
f.x <- dgamma(x, post.alpha, rate = post.beta)
xlabel <- intToUtf8(955)
ylabel <- paste('p(', xlabel, '| y)', sep = '')
sigma  <- intToUtf8(963)



png('Q2/2bplot.png', height = 350, width = 500)
plot(x, f.x, 'l', 
     xlab = xlabel, ylab = ylabel, main = 'Posterior Density Plot')
abline(v = mean.2b        , col = 'red')
abline(v = sci.2b[1]      ,  col = 'green')
abline(v = sci.2b[2]      ,  col = 'green')
abline(v = mean.2b + sd.2b,  col = 'blue')
abline(v = mean.2b - sd.2b,  col = 'blue')

legend('topright',
       legend=c('Posterior', 'Mean','95% Confidence Interval', paste('Mean +-', sigma)),
       col=c('black','red','green', 'blue'),lty=1, lwd = c(2,2,2),
       cex = 0.8)
dev.off()

## ===========================================================================##
#################################### 2C: Calcs #################################
## ===========================================================================##

plambmt15.orig <- pgamma(15, post.alpha, post.beta, lower.tail = FALSE)

## ===========================================================================##
################################## 2D: Jeffreys ################################
## ===========================================================================##

### SETUP ###
prior.alpha.J <- 0.5
prior.beta.J  <- 0
y             <- 3
k             <- 3/12
post.alpha.J  <- prior.alpha.J + sum(y)
post.beta.J   <- prior.beta.J  + k

### Mean, sd, 95\% CI ###

mean.2d    <- post.alpha.J/post.beta.J
sd.2d      <- sqrt(post.alpha.J/(post.beta.J^2))
sci.2d     <- c(qgamma(0.025, post.alpha.J, post.beta.J), 
                qgamma(0.975, post.alpha.J, post.beta.J))

# Posterior plot

x      <- seq(0, mean.2d+4*sd.2d, 0.1)
f.x    <- dgamma(x, post.alpha.J, post.beta.J)
xlabel <- intToUtf8(955)
ylabel <- paste('p(', xlabel, '| y)', sep = '')
sigma  <- intToUtf8(963)

### SAVE PLOT ###

png('Q2/2dplot.png', height = 350, width = 500)

### 2D
plot(x, f.x, 'l', 
     xlab = xlabel, ylab = ylabel, main = 'Posterior Density Plot')
abline(v = mean.2d        ,  col = 'red')
abline(v = sci.2d[1]      ,  col = 'green')
abline(v = sci.2d[2]      ,  col = 'green')
abline(v = mean.2d + sd.2d,  col = 'blue')
abline(v = mean.2d - sd.2d,  col = 'blue')

### 2B
abline(v = mean.2b        ,  col = 'red',   lty = 3)


legend('topright',
       legend=c('Posterior', 'Mean','95% Confidence Interval', paste('Mean +-', sigma), 'Mean (2b)'),
       col=c('black','red','green', 'blue', 'red'),lty=c(1,1,1,1,3), lwd = 2,
       cex = 0.8)


dev.off()


#### 2D:C ####
plambmt15.J <- pgamma(15, post.alpha.J, post.beta.J, lower.tail = FALSE)

#### TABULAR COMPARISON
## originalr esults
orig <-rbind(mean.2b, sd.2b, sci.2b[1], sci.2b[2], plambmt15.orig)

#### Jeffreys
jeff <- rbind(mean.2d, sd.2d, sci.2d[1], sci.2d[2], plambmt15.J)

#### Overall
comp.Q2d <- cbind(orig, jeff)
lambda <- intToUtf8(955)
sigma  <- intToUtf8(963)
gteq   <- intToUtf8(8805)

rownames(comp.Q2d) <- c('Mean', 'SD', '2.5%', '97.5%', 
                   paste('P(',lambda, gteq, '15)', sep=''))
colnames(comp.Q2d) <- c('Original', 'Jeffreys')

# Multiply last row by 100 to get as percentage
comp.Q2d[5, ] <- 100*comp.Q2d[5, ]
# 4 sf for whole matrix
comp.Q2d <- signif(comp.Q2d, 4)
# Add % on the end to show percentage
comp.Q2d[5, ]  <- sapply(comp.Q2d[5,], paste, '%', sep='')
# Transpose to take up less space
comp.Q2d <- t(comp.Q2d)

png('Q2/2dComp.png', height=100, width = 300)
grid.table(comp.Q2d)
dev.off()



## ===========================================================================##
################################# 2E: POSTERIOR ################################
## ===========================================================================##

gp.nbinom <- function(ypred, kpred, post.alpha, post.beta, lower.tail = TRUE) {
  #
  # Adapts nbinom to work with gamma posterior parameters.
  #
  # ypred:       P(X <= ypred) by default
  # kpred:       Conversion parameter associated with ypred
  # post.alpha:  Posterior from previous
  # post.beta:   Posterior from previous
  # lower.tail:  P(X <= ypred) default
  
  r <- post.alpha
  p <- post.beta/(kpred + post.beta)
  
  pnbinom(ypred, size=r, prob=p, lower.tail = lower.tail)
}

### Various ys and ks (note, vary lower.tail, so not exact)

ypred.6m <- 5; kpred.6m <- 6/12 ## 6 or more  in 6m
ypred.5m <- 1; kpred.5m <- 5/12 ## 1 or fewer in 5m
ypred.1m <- 4; kpred.1m <- 1/12 ## 5 or more  in 1m. 
## 5 or more in at least one month
## define p = P(5 or more in 1m)
# (6 choose 0) * p^0 *(1-p)^6 is P(4 or less in all months)
# Do 1 - this. 



####### ORIGINAL PRIOR ######
r <- post.alpha

## 6 or more in O2
sixplus.orig <- gen.nbinom(ypred.6m, kpred.6m, 
                           post.alpha=post.alpha, post.beta = post.beta, 
                           lower.tail = FALSE)

## 1 or fewer in first 5 months: P(ypred <= 1)
oneminus.orig <- gen.nbinom(ypred.5m, kpred.5m, 
                            post.alpha=post.alpha, post.beta = post.beta, 
                            lower.tail = TRUE)

## 5 or more in last month: P(ypred >= 5) = 1- P(ypred <=4)
fiveplus.orig <- gen.nbinom(ypred.1m, kpred.1m, 
                            post.alpha = post.alpha, post.beta=post.beta, 
                            lower.tail = FALSE)

# 5 or more in at least one month
fourless.all.orig <- 1 - (1-fiveplus.orig)^6


####### JEFFREYS PRIOR ######

r <- post.alpha.J

## 6 or more in O2
sixplus.J <- gen.nbinom(ypred.6m, kpred.6m, 
                           post.alpha=post.alpha.J, post.beta = post.beta.J, 
                           lower.tail = FALSE)

## 1 or fewer in first 5 months: P(ypred <= 1)
oneminus.J <- gen.nbinom(ypred.5m, kpred.5m, 
                            post.alpha=post.alpha.J, post.beta = post.beta.J, 
                            lower.tail = TRUE)

## 5 or more in last month: P(ypred >= 5) = 1- P(ypred <=4)
fiveplus.J <- gen.nbinom(ypred.1m, kpred.1m, 
                            post.alpha = post.alpha.J, post.beta=post.beta.J, 
                            lower.tail = FALSE)

# 5 or more in at least one month
fourless.all.J <- 1 - (1-fiveplus.J)^6


### Combine results
origres <- cbind(sixplus.orig, oneminus.orig, fiveplus.orig, fourless.all.orig)
jeffres <- cbind(sixplus.J, oneminus.J, fiveplus.J, fourless.all.J)

allres <- rbind(origres, jeffres)
rownames(allres) <- c('Original', 'Jeffreys')
colnames(allres) <- c('>=6 (6m)', '<=1 (First 5m)', '>=5 (Last Month)', '>=5 (Any month)')

allres <- signif(100*allres, 4)
allres <- apply(allres, c(1,2), paste, '%', sep='')

### Write results
png('Q2/2eJeffOrig.png', height =90, width = 420)
grid.table(allres)
dev.off()





