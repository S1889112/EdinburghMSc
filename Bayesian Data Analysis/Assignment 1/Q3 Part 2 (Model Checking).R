## ===========================================================================##
########################## STUDENTISED RESIDUALS ###############################
## ===========================================================================##

# Use one chain for the results

resmat <- as.matrix(samp[[1]])

# Individual parameter vectors
b1    <- resmat[, 1]
b2    <- resmat[, 2]
b3    <- resmat[, 3]
b4    <- resmat[, 4]
sigma <- sqrt(resmat[, 5])

# beta matrix
B <- cbind(b1, b2, b3, b4)

# residuals and fitted
studentisedresid <- fitted.blr(resmat = resmat, X = x, B = B, 
                               sigma = sigma, resids = TRUE)
fittedvalues     <- fitted.blr(resmat = resmat, X = x, B = B, 
                               sigma = NULL, resids = FALSE)

## ===========================================================================##
############################ RESIDUAL ANALYSIS #################################
## ===========================================================================##

# posterior mean of studentised residuals
# Get mean of each row, so we have 48 residuals

studentisedresidm <- apply(studentisedresid, 1, mean)

## Index plot

png('Q3/3eresidindex.png', height = 350, width = 500)
plot(studentisedresidm, main = 'Residuals vs Index')
dev.off()

#QQ-plot

png('Q3/3eQQplot.png', height=350, width=500)
qqplot(x=qt(ppoints(n), df=5), 
       y=studentisedresidm, 
       main="QQ Plot (T- Distribution)",
       xlab="Theoretical Quantiles", 
       ylab= "Sample Quantiles")
qqline(studentisedresidm, distribution=function(p) qt(p, df=5), col="red", lw=2)
dev.off()



######## PREDICTIVE CHECKS
######## REPLICATE DATA

#Now do some predictive checks
#First replicate the data


niterf <- nrow(resmat)

yrep=matrix(0,nrow=n,ncol=niterf)
for(l in 1:niterf){
  for(i in 1:n){
    yrep[i,l]=rnorm(1,b1[l]*x[i,1]+b2[l]*x[i,2]+b3[l]*x[i,3]+b4[l]*x[i,4],sigma[l])
  }
}

#Compute posterior preditive distribution of important stats
yrepmin  = apply(yrep,2,min)
yrepmax  = apply(yrep,2,max)
yreplow  = apply(yrep,2,quantile, probs = c(0.25))
yrepmed  = apply(yrep,2,median)
yrepupp  = apply(yrep,2,quantile, probs = c(0.75))
ypredkrt = apply(yrep, 2, kurtosis)


png('Q3/3ePostPredChecks.png', height=500, width=700)
par(mfrow = c(2, 3))
##### Posterior histograms
hist(yrepmin, col = 'gray40', main = 'Predictive Distribution for Minimum')
  abline(v = min(l.perm), col = 'red', lwd = 2)

hist(yrepmax, col = 'gray40', main = 'Predictive Distribution for Maximum')
  abline(v = max(l.perm), col = 'red', lwd = 2)

hist(yreplow, col = 'gray40', main = 'Predictive Distribution for Lower Quartile')
  abline(v = quantile(l.perm, probs = c(0.25)), col = 'red', lwd = 2)
  
hist(yrepmed, col = 'gray40', main = 'Predictive Distribution for Median')
  abline(v = median(l.perm), col = 'red', lwd = 2)
  
hist(yrepupp, col = 'gray40', main = 'Predictive Distribution for Upper Quartile')
  abline(v = quantile(l.perm, probs = c(0.75)), col = 'red', lwd = 2)
  
hist(ypredkrt, col = 'gray40', main = 'Predictive Distribution for Kurtosis')
  abline(v = kurtosis(l.perm), col = 'red', lwd = 2)
dev.off()




