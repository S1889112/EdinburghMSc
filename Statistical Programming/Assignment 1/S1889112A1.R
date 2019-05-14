#### Question 1 ####

## ;;
## --------------------------------------------
## Q1: --
## --------------------------------------------
## ;;



## 1.1

x <- c( 2.53, 4.22 , 1.15 , 4.33 ,  7.31,  5.89,  2.74,
        8.69, 5.19 , 8.38 , 13.65, 13.23,  5.18, 14.61, 
       14.78, 14.15, 20.19, 18.09, 17.57, 21.73)
n <- length(x)

# x[1] is first element, unchanged based on smoothing formula

# Combine with numeric vector of n-1 elements for loop to add to

a.lambda.0.2 <- c(x[1], numeric(n-1))
a.lambda.0.5 <- c(x[1], numeric(n-1))

# Add smoothed values to vector

for (t in 2:n) {
  a.lambda.0.2[t] <- 0.2 * x[t] + (1 - 0.2) * a.lambda.0.2[t - 1]
  a.lambda.0.5[t] <- 0.5 * x[t] + (1 - 0.5) * a.lambda.0.5[t - 1]
}

# Plot x, and smoothed series. 


plot(x = seq(1:n), y=x, type = 'l', xlab = 'index', ylab = 'y')
lines(x = seq(1:n), y=a.lambda.0.2, col='red')
lines(x = seq(1:n), y=a.lambda.0.5, col='blue')
title(main = 'Question 1.1')

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 1.2


n <- 50
numdatasets <- 1000
exp.rate <- 1

Ymeans <- numeric(numdatasets)

# Generate  1000 datasets with 50 observations
# Then store the means of each dataset in Ymeans

set.seed(30)

for (i in 1:numdatasets) {
  expsimul  <- rexp(n, rate = exp.rate)
  Ymeans[i] <- mean(expsimul)
}

Ymeans <- sort(Ymeans)

# We know standard deviation and mean for exponential given rate parameter 
# Place these here for flexibility - if we want to change exp.rate, it is easier
# to feed through

mu.exp    <-  1 / (exp.rate)
sigma.exp <-  1 / (exp.rate ^ 2)

# Empirical density

emp.dens.data <- (sqrt(n) * (Ymeans - mu.exp)) / sigma.exp

plot(density(emp.dens.data), xlab = 'x', main = 'Question 1.2')

# N(0,1) density
# Get inputs for normal PDF
x.normdens.inputs <- seq(from = -10, to = 10, by = 0.1)

# Calculate density for each input, then plot on the same figure as 
# the empirical density

x.norm.dens <- dnorm(x = x.normdens.inputs, mean = 0, sd = 1)
lines(x = x.normdens.inputs, 
      y = x.norm.dens, 
      col = 'red')

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


#### Question 2 ####


## ;;
## --------------------------------------------
## Q2 --
## --------------------------------------------
## ;;

## 2.1

Kurtosis <- function(y) {
  # Computes kurtosis of vector y, where the numerator is the fourth central 
  # moment, and the denominator is the squared variance. Then use: 
  #
  #    (numerator)/(denominator) - 3
  # 
  # so normal distribution has kurtosis 0.
  #  
  # Args:
  #   y: The vector of length n of which to compute kurtosis
  #
  # Returns:
  #   Kurtosis of y
  
  y.mean <- mean(y)
  num <- mean((y - y.mean) ^ 4)
  denom <- var(y) ^ 2
  return(num/denom - 3)
}





## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##



## 2.2

y.Q2 <- c(-0.90,  0.47,  0.87, -5.37, -0.48,
           0.24,  0.71,  0.58, -0.54, -0.41,
           0.09,  0.32,  0.07,  1.70, -0.41,
           0.33, -0.72, -0.74, -0.35,  1.14)


# B is number of boostrap samples

n.Q2 <- length(y.Q2)
B.Q2 <- 10000

# Initialise vector  kurts, to store kurtosis for each bootstrapped sample

kurts <- numeric(B.Q2)
set.seed(36)
# B times, sample n times from vector y with replacement, calculate kurtosis 
# and store in kurts

for (sample in 1:B.Q2) {
  bs <- sample(y.Q2, n.Q2, replace=TRUE) 
  kurts[sample] <- Kurtosis(bs)
}

kurts <- sort(kurts)

# 2.5% below, and 2.5% above
paste("The confidence interval is [", round(kurts[0.025 * B.Q2], 3), ", ",
      round(kurts[0.975 * B.Q2], 3), '] to 3 decimal places.', sep='')

# [1] "The confidence interval is [-1.494, 8.824] to 3 decimal places." 


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 2.3

# Suppose we are adopting a significance level of 'a'
# where traditionally 'a' = 0.05

# If the null and alternative are as specified in the
# question, then rejecting the null is equivalent to saying
# that 0 is not in the (1-a) confidence interval. 

# Supposing we adopt the traditional 5% significance level,
# we reject the null hypothesis if 0 is in the 95% 
# confidence interval for kurtosis. 

# This logic can be seen below as code. 

if (0 > kurts[0.025*B.Q2] && 0 < kurts[0.975*B.Q2]) {
  paste("Do not reject the null hypothesis. There is not enough evidence to",
        "suggest kurtosis of y is non-zero")
} else {
  paste("Reject the null hypothesis")
}



## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

#### Question 3 ####


## ;;
## --------------------------------------------
## Q3 --
## --------------------------------------------
## ;;


## 3.1

install.packages('DAAG')
require(DAAG)

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##



## 3.2

set.seed(36)

# The confidence interval can be altered by changing the value below

confidenceinterval <- 0.95

siglev <- 1 - confidenceinterval

# Get data

data("rainforest")

# Create observation vectors

wood.myrtifolia <- rainforest$wood[rainforest$species == 'B. myrtifolia' ]
wood.acmena     <- rainforest$wood[rainforest$species == 'Acmena smithii']


# Perform bootstrap

B.Q3 <- 10000

# Specify bootstrap sample size for each species
n.myrtifolia <- length(wood.myrtifolia)
n.acmena     <- length(wood.acmena)

# Initialise vector of the difference in the means
meandif <- numeric(B.Q3)


for (i in 1:B.Q3) {
  bs.myrtifolia <- sample(wood.myrtifolia, n.myrtifolia, replace=TRUE)
  bs.acmena <- sample(wood.acmena, n.acmena, replace=TRUE)
  meandif[i] <- mean(bs.myrtifolia) - mean(bs.acmena)
}

meandif <- sort(meandif)

# The confidence interval is as below for the specified significance level. 

CI.Q3 <- c(meandif[(siglev / 2) * B.Q3], meandif[(1 - siglev / 2) * B.Q3])

# Changing the confidence interval requested will feed through here 

paste("The ", confidenceinterval * 100, "% " ,"confidence interval is ", 
      "[", round(CI.Q3[1], 3), ", ", round(CI.Q3[2], 3), "]", 
      " rounded to 3 decimal places", sep='')

# [1] "The 95% confidence interval is [-142.755, 507.759] rounded to 3 
#      decimal places"

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

#### Question 4 ####

## ;;
## --------------------------------------------
## Q4 --
## --------------------------------------------
## ;;

## 4.1


LlikPoisson <- function(b, x, y){
  # 
  # Computes log-likelihood of Poisson regression model with one independent 
  # variable
  # 
  #  Args:
  #    b: a 2-element vector where the first element is the constant term, and 
  #       the second is the coefficient on x
  #    x: The independent variable
  #    y: The dependent variable
  #    
  #
  #  Returns:  
  #    The log-likelihood of the aforementioned Poisson regression model
  
  # Error handling. Ensure b has 2 elements, and x and y are the same length
  
  if (length(b) != 2) {
    stop('b must be a vector containing 2 elements')
  }
  if (length(x) != length(y)) {
    stop('x and y must be vectors of the same length. Try using length() to',
         ' check', sep='')
  }
  
  # Define b0 (constant) and b1 (coefficient)

  b0 <- b[1]
  b1 <- b[2]
  
  # Calculate log-likelihood and return it. 
  
  llik <- sum(y * (b0 + b1 * x) - exp(b0 + b1 * x) - log(factorial(y)))
  return(llik)
}


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##



## 4.2

Q4df <- read.table(file = 'counts.csv', sep = ' ', header = TRUE)

# Perform optimisation

# Use control = list(fnscale = -1) to perform maximisation since optim defaults
# to minimisation.

b.optimised <- optim(par = c(0,0), LlikPoisson, x = Q4df$Time, 
                     y=Q4df$Count, method = 'BFGS',
                     control = list(fnscale = -1))

b.estimated  <- b.optimised$par
b0.estimated <- b.estimated[1]
b1.estimated <- b.estimated[2]

paste('(b0, b1) = (', round(b0.estimated,3),', ', round(b1.estimated,3), ')', 
      sep='')

# [1] "(b0, b1) = (3.24, -0.036)"



## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 4.3

# Recall lambda is expectation of Y. So E(Y|X) = lambda|X = exp(b0 + b1x)

pred.at.20 <- exp(b0.estimated + b1.estimated * 20)
pred.at.20

# 12.39256



## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##



## 4.4

set.seed(36)

# CI for the prediction


B.Q4 <- 10000
n.Q4 <- length(Q4df$Time)
pred.vec <- numeric(B.Q4)

for (i in 1:B.Q4)  {
  
  # Create index to sample data frame
  sampleindex <- sample(x = 1:n.Q4, size = n.Q4, replace=TRUE)
  df.indexed <- Q4df[sampleindex, ]  # indexed rows from both columns
  
  # Perform optimisation on the indexed data frame, extract parameters
  bs.opt <- optim(par = c(0,0), LlikPoisson, x = df.indexed$Time, 
                  y = df.indexed$Count, method = 'BFGS',
                  control=list(fnscale=-1))
  
  bs.params <- bs.opt$par
  
  # Make prediction as in Q4.3, and store in pred.vec vector.
  pred.vec[i] <- exp(bs.params[1] + bs.params[2]*20)
}

pred.vec <- sort(pred.vec)

paste("The 95% confidence interval for the count given t=20 is ", 
      "[",  round(pred.vec[0.025 * B.Q4], 3), 
      ", ", round(pred.vec[0.975 * B.Q4], 3), "] (3dp).", sep='')

# [1] "The 95% confidence interval for the count given t=20 is 
#      [0.226, 21.71] (3dp)."


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

#### Question 5 ####


## ;;
## --------------------------------------------
## Q5 --
## --------------------------------------------
## ;;

## 5.1

# Doing some algebra, the inverse CDF is:
# F_x.inv = sigma*sqrt(-2*log(1-u))
# where u is a uniform random variable with support (0,1)
# log(z) where z in (0,1) is negative, so square root term does not cause issue

rRayleigh <- function(n, sigma=1){
  # Returns a vector of size n sampled from the Rayleigh distribution with
  # scale parameter sigma. The default scale parameter is 1.
  # 
  # Args:
  #   n: Number of samples desired
  #   sigma: the scale parameter of the Rayleigh distribution, defaulted to 1.
  #
  # Returns:
  #   A vector with n random samples from the Rayleigh(sigma) distribution
  
  
  #Generate n standard uniform samples
  u <- runif(n, 0, 1)
  
  # Pass through inverse CDF of Rayleigh
  x <- sigma * sqrt(-2 * log(1-u))
  return(x)
}

# Function call requested

rRayleigh(n = 1000, sigma = 1)


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

## 5.b

rgev <- function(n, mu, sigma, xi, tol.xi.limit = 0.05){
  # Returns a vector of size n sampled from the GEV distribution with
  # location paramater mu, scale parameter sigma, and shape parameter xi.
  # The tolerance level is 
  # 
  #
  # Args:
  #   n:    Number of samples desired
  #   mu:    Location parameter
  #   sigma: Scale parameter
  #   xi:    Shape parameter
  #
  # Returns:
  #   A vector with n random samples from the GEV(mu, sigma, xi) distribution
  u <- runif(n, 0, 1)
  if(abs(xi) < tol.xi.limit){
    x <- -sigma * log(-log(u)) + mu
  } 
  else {
    x <- mu + sigma * ((-log(u)) ^ (-xi) - 1) / xi    
  }
  return(x)
}


# Function calls

rgev(1000, mu = 0, sigma = 1, xi = 0  )
rgev(1000, mu = 0, sigma = 1, xi = 1  )
rgev(1000, mu = 0, sigma = 1, xi= -0.5)




## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

## 5.c

# No, it is not possible to always construct rejection sampling for beta(a,b)
# given a U(0,1) proposal. Consider the following:

# Finiteness of p(x)/g(x) is required for rejection sampling.  
# Additionally, we require Mg(x) >= p(x) for all x
# Define g(x) as a U(0,1) pdf, and p(x) as a Beta(alpha, beta) pdf

# Note the mode of the beta distribution is (alpha-1)/(alpha+beta-2)
# Alpha, beta > 0 by definition of the beta.
# The support of the beta distribution is (0,1) so for the mode to exist it
# must be in that interval. This leads us to the conditions:
# alpha > 1
# beta > 1

# If alpha = beta = 1, then the mode is 0/0, and hence undefined.
# This makes sense as these parameters lead to a U(0,1) distribution, of which
# every value in the interval is a mode. 

# If the mode of the beta distribution does not exist, then there cannot exist
# an M such that Mg(x) >= p(x) if g(x) is uniform(0,1). If alpha or beta < 1
# then as x -> 0 or x-> 1, the density approaches infinity at either or both
# ends. Hence for Mg(x) >= p(x) for all x, M would need to be infinite. 

# See below for examples of beta densities where alpha < 1 and/or beta < 1

# Define x range 

x <- seq(0,1, by=0.005)

# beta(0.9, 0.9)

dbeta(0, 0.99, 0.99)
dbeta(1, 0.99, 0.99)
plot(x, dbeta(x, 0.99, 0.99))

# beta(1.5, 0.9)

dbeta(0, 1.5, 0.99)
dbeta(1, 1.5, 0.99)
plot(x, dbeta(x, 1.5, 0.99))

# beta(0.9, 1.5)

dbeta(0, 0.99, 1.5)
dbeta(1, 0.99, 1.5)
plot(x, dbeta(x, 0.99, 1.5))

# Although not an official proof, this shows that even when only one of alpha 
# or beta is near 1, the density is infinity at one end. 


rbeta.rs <- function(n, alpha, beta){
  #
  # Samples n times from a beta distribution with parameters alpha and beta This   
  # function uses a rejection sampling method, so we require alpha, beta > 1. 
  #
  # Args:
  #   n:     The number of samples desired
  #   alpha: The first shape parameter of the beta distribution
  #   beta:  The second shape parameter of the beta distribution
  #
  # Returns:
  #   A vector of size n containing random samples from the beta distribution   
  
  
  # Error handling
  # Stop function if user requests a uniform distribution (alpha=beta=1) or if
  # alpha or beta are less than 1. 
  
  if ((alpha < 1 || beta < 1) || (alpha==1 && beta==1)){
    stop('Invalid parameter values')
  }
  
  # Main function
  
  desiredsamples <- n
  numsamples <- 0
  samples <- numeric(desiredsamples)

  # M is multiplication factor that ensures Mg(x) >= p(x) for all x.
  # Find highest point in beta distribution, and find equivalent density. M*1
  # must be at least this value (1 comes from pdf of uniform(0,1))
  
  betamode <- (alpha - 1) / (alpha + beta - 2)
  M <- dbeta(betamode, alpha, beta)
  
  
  while (numsamples < desiredsamples) {
    x <- runif(1)
    u <- runif(1) #This is for the check
    if (u < dbeta(x = x, shape1 = alpha, shape2 = beta) / M) {
      numsamples = numsamples + 1
      samples[numsamples] <- x
    }
  }
  return(samples)
}

# Function call

rbeta.rs(n = 1000, alpha = 2, beta = 1)






## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


#### Question 6 ####


## ;;
## --------------------------------------------
## Q6 --
## --------------------------------------------
## ;;


## 6.1

LlikCauchy <- function(theta, y){
  # Returns the log-likelihood of the Cauchy distribution with location 
  # parameter theta and a scale parameter set to 1. 
  #
  # Args:
  #   y: Data to determine log-likelihood of, assuming a Cauchy distribution
  #   theta: The location parameter 
  #
  #
  # Returns:
  #   A scalar - the log-likelihood given the previous arguments
  
  n <- length(y)
  llik <- -n * log(pi) - sum(log(1 + (y - theta) ^ 2))
  return(llik)
}





## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

## 6.2

# Cauchy has no closed form solution, hence numerical methods required

# Define the input vector

y.Q6 <- c(0, 4, 8)

# Define score function to check for presence of multiple minima

ScoreCauchy <- function(theta, y = c(0, 4, 8)) {
  #
  # Returns the score of the Cauchy distribution with location parameter theta
  # and a scale parameter set to 1. 
  #
  # Args:
  #   theta: The scale parameter
  #   y:     Data vector to determine score of
  #   
  #
  #
  # Returns:
  #   A scalar value - the score given the above arguments. 
  
  score <- 2 * sum((y - theta)/(1 + (y - theta) ^ 2))
  return(score)
}

# Firstly, plot a graph of the Score
# Generate theta values, and generate score of each 

theta.test <- seq(-100, 100, 0.1)
score.vector <- vapply(X = theta.test, FUN= ScoreCauchy, y = y.Q6, 
                      FUN.VALUE = numeric(1))
plot(theta.test, score.vector, 'l')


# We can see there are 3 minima from the above plot. Hence we may converge to
# a local maxima when maximising the log-likelihood numerically. 
# To check our solutions, plot a graph of the log-likelihood in a similar 
# manner to how the score was plotted, using theta.test as the inputs again


llik.vector <- vapply(X = theta.test, FUN=LlikCauchy, y = y.Q6, 
                      FUN.VALUE = numeric(1))
plot(x = theta.test, y = llik.vector, 'l')


# Now perform optimisation using multiple methods, and check each against plot

# 1) BFGS

theta.bfgs <- optim(par = c(0), LlikCauchy, y = y.Q6, method = 'BFGS', 
                  control = list(fnscale = -1))
abline(v = theta.bfgs$par, col='red')

# We can see on the plot BFGS does not converge to the global maximum
# Now try Nelder-Mead (the default)

# 2) Nelder-Mead

theta.nm <- optim(par = c(0), LlikCauchy, y = y.Q6, method = 'Nelder-Mead', 
                  control = list(fnscale = -1))
abline(v = theta.nm$par, col='pink')

# Nelder-Mead converges to the same local minima as BFGS, and R now recommends
# using Brent optimisation


# 3) Brent

theta.brent <- optim(par=c(0), LlikCauchy, y = y.Q6, method = 'Brent',
                     lower = -10, upper = 10, control = list(fnscale = -1))
abline(v = theta.brent$par, col='green')

# We can see Brent optimisation converges to the global maximum. So we will
# use the parameters from Brent optimisation. 

paste('The MLE of theta given the observations is ', round(theta.brent$par,3), 
      '.', sep='')

# [1] "The MLE of theta given the observations is 4."

