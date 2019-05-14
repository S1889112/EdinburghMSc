# Remove any leftover variables before starting
rm(list = ls())

## ;;
## ----------------------------------------------------------------------------|
## Q1: --                                                                      |
## ----------------------------------------------------------------------------|
## ;;


## 1.1

# Function to return area and standard error of the integral using importance
# sampling

mc.integrate <- function(n, limits, integrand, proposal.dens,
                         proposal.rand, ...){
  # Function to calculate the Monte Carlo estimate and standard error for a 
  # definite integral using importance sampling
  #
  # Args:
  #   n:             Sample size
  #   limits:        Limits of integration, a vector e.g. c(0,1)
  #   integrand:     A function for the integrand
  #   proposal.dens: A function representing the proposal density
  #   proposal.rand: A function representing random draws from the proposal 
  #                  density
  #   ...          : Set keyword arguments for proposal density and its random
  #                  numbers
  # Returns:
  #   Returns a vector containing the MC estimate of the integral, and the MC
  #   standard error
  # 
  # Example:
  #   The following calculates the area under y = x^4 from 0 to 1, using a 
  #   N(0, 2) distribution. We know the analytical solution is 0.2. The mean 
  #   and sd parameters are passed to both dnorm, and rnorm when mc.integrate
  #   is called. 
  #
  # n.ex         <- 100000
  # limits.ex    <- c(0, 1)
  # integrand.ex <- function(x) x^4
  # 
  # set.seed(1)
  # mc.integrate(n = n.ex, limits = limits.ex, integrand = integrand.ex, 
  #              proposal.dens = dnorm, proposal.rand = rnorm, 
  #              mean = 0, sd = 2)
  # 
  # [1] 0.202350793 0.002411652
  
  # Extract individual limits of integration
  lim.lower = limits[1]
  lim.upper = limits[2]
  
  # Generate random numbers from the proposal distribution with user-set
  # parameters
  
  x <- proposal.rand(n, ...)
  
  # Evaluate the integrand at each random number, and divide it by the proposal
  # density evaluated at each random number
  
  w <- integrand(x)/proposal.dens(x, ...)
  
  # i represents the indicator function formed from bringing the limits 'down'
  # into the integrand
  
  i <- (x > lim.lower & x < lim.upper)
  
  # Compute the Monte Carlo estimates of the area (mean) and standard error. 
  
  mc_est <- mean(w * i)
  mc_var <- var(w * i)
  mc_se  <- sqrt(mc_var / n)
  # Return as vector
  return(c(mc_est, mc_se))
}


set.seed(1)
# Define function for the integrand, and the limits of integration

integrand.Q1 <- function(x) (x ^ 2) * exp(- (x - 2) ^ 2)
limits.Q1    <- c(1, 3)

# Perform integration

mc.integrate(n = 100000, limits = limits.Q1, integrand = integrand.Q1, 
             proposal.dens = dnorm, proposal.rand = rnorm, mean=2, sd=sqrt(1/2))

#     Estimate   Standard error
# [1] 6.35210597 0.01360721

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 1.2


set.seed(1)
# Define function for the integrand, and the limits of integration

integrand.Q2 <- function(y) y ^ 5 * log(y)
limits.Q2    <- c(1, 5)

# Perform integration

mc.integrate(100000, limits.Q2, integrand = integrand.Q2, proposal.dens = dunif,
             proposal.rand = runif, min=1, max=5)

#     Estimate   Standard error
# [1] 3764.7781 16.4611

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 1.3


# We can continue using importance sampling in this example. 
# We use g(x) = 1*I(x in (0,1)) as the proposal, which is a U(0,1). This is the
# support of the integrand when the limits are 'brought down'. 

set.seed(1)

# Define function for the integrand, and the limits of integration

integrand.Q3 <- function(x) (4 / (1 + x ^ 2))
limits.Q3    <- c(0, 1)


# N = 10

mc.integrate(10, limits = limits.Q3, integrand = integrand.Q3,
             proposal.dens = dunif, proposal.rand = runif,
             min = 0, max = 1)

# [1] 3.0259369 0.2261461

# ---


# N = 100

mc.integrate(100, limits = limits.Q3, integrand = integrand.Q3,
             proposal.dens = dunif, proposal.rand = runif,
             min = 0, max = 1)

# [1] 3.11920961 0.06039102

# ---

# N = 1000

mc.integrate(1000, limits = limits.Q3, integrand = integrand.Q3,
             proposal.dens = dunif, proposal.rand = runif,
             min = 0, max = 1)

# [1] 3.15102230 0.02045341

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## ;;
## ----------------------------------------------------------------------------|
## Q2: --                                                                      |
## ----------------------------------------------------------------------------|
## ;;

# Remove all variables before starting question
rm(list = ls())

## 2.1


# The null hypothesis is that the variance of the observations y1 to y10 is
# the same as the variance of the observations y11 to y20. 
# 
# The alternative is that the variances are different, so the variance
# of the second group (y11 to y20) can be higher or lower than the 
# variance of the first group - i.e. we have a 2-tailed test. 
# 
# Denote variances as s. Then we have
#
#
# H0: s1  =  s2 (or s1/s2  =  1)
# H1: s1 =/= s2 (or s1/s2 =/= 1)
#
#
# Now, we specify a significance level we wish to test at (alpha).
#
# Next, we calculate the F-statistic, which is the ratio of the sample
# variances. 
#
#
# Since we have a 2-tail test, let us put the larger sample variance as the 
# numerator. Denote the larger sample variance s1, and the smaller one s2. 
# Note, we can also do the opposite, and do a one-sided test on the lower end.
#
# F = s1/s2 ~ F(df1, df2) where df1 = n - 1, df2 = m - 1
#
# n is the number of observations in the group with the larger variance, and  
# analogously for m.
#
# 
# We can now calculate a one-sided p-value for s1 > s2.
#
# A p-value (in this case) is the probability of the test-statistic being 
# greater than the value we actually observed of the test-statistics (F_obs), 
# under the assumption of the null hypothesis being true. So we consider the
# test-statistic a random variable, F and calculate the following: 
# 
# p = Pr(F > F_obs | H0)
#
# We can calculate this using the 'pf' function in R to get a one-sided p-value.

# Since this was a one-sided test, we multiply the p-value by 2 to get the 
# 2-sided p-value, as this takes into account the fact the p-value
# considers 'extremeness' and we have just considered the greater than case. 
#
# If p < alpha, we reject the null hypothesis. Otherwise we do not reject it. 


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##



## 2.2


# Specify a significance level of 5%.

alpha <- 0.05

# input data

y <- c(0.48,  0.50, -0.86, -0.83, -0.32, -1.30, -1.42,
       1.74, -0.29, -1.31, -0.07, -1.22,  3.24, -1.97,
       1.81,  4.00,  1.87,  1.50,  6.81, -4.14)

# Get number of data points

n <- length(y)

# Split into groups, k = 10

y.group1.2ii <- y[1:10]
y.group2.2ii <- y[11:n]

# Perform F-test using var.test():

F.test.obj.Q2ii <- var.test(y.group1.2ii, y.group2.2ii)

# Get p-value

p.value.2ii <- F.test.obj.Q2ii$p.value

# Do we reject the null hypothesis?

p.value.2ii < alpha

# [1] TRUE

# The p-value is less than significance level, so we reject the null hypothesis 
# of equal variances at the 5% significance level.  


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 2.3


# k in {2, 3, ..., n-2} = {2, 3, ..., 18}

# Recall n from previous subquestion

n <- length(y)

# min.k: Minimum value k can take is 2
# max.k: Maximum value k can take is 18
# 
# As we need >= 2 observations to calculate a variance

min.k <- 2
max.k <- n - 2

# k can take any value between 2 and n-2
# Hence number of tests is number of values between 2 and n - 2

tests <- length(seq(min.k, max.k))

# Continually update minimum p-value, and store k that matches it
# Set to 1, as probabilities cannot be larger than 1. 

min.p.2iii <- 1

# Loop through each possible k

for (k in min.k:max.k){
  # Split y into 2 groups, the first k observations in group 1, the rest in
  # group 2

  y.group1.2iii <- y[1 : k]
  y.group2.2iii <- y[(k + 1) : n]
  
  # Perform the F-test between groups, store the resulting object, and get
  # the p-value
  
  F.test.obj.2iii <- var.test(y.group1.2iii, y.group2.2iii)
  p.value.2iii    <- F.test.obj.2iii$p.value
  
  # If the p-value is lower than current minimum p-value, update it, and the k
  # corresponding to this minimum p-value
  
  if (p.value.2iii < min.p.2iii){
    min.p.2iii <- p.value.2iii
    min.p.k    <- k
  }
  
}



# Use cbind to keep min.p.k discrete (aesthetic choice)

cbind(min.p.k, min.p.2iii)

#      min.p.k   min.p.2iii
# [1,]      12 0.0003491066

# k = 12 is the most likely change point, with a corresponding p-value of
# 0.00035

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 2.4


# General steps:
# 1) Begin simulation
# 2) Rearrange the y vector
# 3) Perform the F-test for each possible k-value as done in part iii
# 4) Find the minimum p-value and store it
# 5) Return to step 1, repeat until desired simulations completed

####

# Set seed for consistent results

set.seed(36)

# Recall our variables from part iii, descriptions available in iii

n     <- length(y)
min.k <- 2
max.k <- n - 2
tests <- length(seq(2, n - 2))


# Specify number of simulations desired
# Note there will be a loop within a loop, so maintain a reasonable number

sims <- 10000

# Initiate vector to store minimum p-values - one for each simulation

min.p.values.2iv <- numeric(sims)



for (simulation in 1:sims) {
  # Set default minimum p-value to update as 1, all probabilities less than 1. 
  
  p.min.2iv <- 1
  
  # Rearrange observations in y
  
  y.rearranged <- sample(x = y, size = n, replace = FALSE) 
  
  # Perform steps from part 3 on rearranged y
  
  for (k in min.k:max.k){
    
    # Split the rearranged y into 2 groups based on k. 
  
    y.group1.2iv <- y.rearranged[1:k]
    y.group2.2iv <- y.rearranged[(k + 1):n]
    
    # Perform F-test and extract p-value
    
    F.test.obj.2iv <- var.test(y.group1.2iv, y.group2.2iv)
    p.value.2iv    <- F.test.obj.2iv$p.value
    
    # Update minimum p-value if necessary
    
    if (p.value.2iv < p.min.2iv) {
      p.min.2iv <- p.value.2iv
    }
  }
  # Store minimum p-value for sampling distribution
  
  min.p.values.2iv[simulation] <- p.min.2iv
}


# Sort minimum p-values

min.p.values.2iv <- sort(min.p.values.2iv)

# Find value which 5% of minimum p-values are less than or equal to

min.p.critical <- min.p.values.2iv[0.05*sims]

# Plot density of min.p.values

plot(density(min.p.values.2iv))


# To use the graph to conclude about a change in the sequence, we must plot
# both the critical value, and the observed value at k = 12 on the density 
# graph. 

# The critical value is the value at which 5% of the minimum p-values are less
# than or equal to it. Hence if the minimum p-value from part iii is less than
# it, we reject the null of no change at the 5% significance level, as under the
# null of no change, getting this particular minimum p-value would be very 
# unlikely - i.e. have less than a 5% probability of occurring


# Draw on vertical lines representing key values 
# Red for critical, green for obtained

abline(v = min.p.critical, col = 'red')
abline(v = min.p.2iii,     col = 'green')

# From the graph, we just about see the green line is to the left of the red.
# This suggests that under the null of no change, the probability of obtaining
# less than or equal to the minimum p-value we actually got is less than 5%. 

# Hence we conclude there is evidence to suggest a change has occurred at the 
# 5% significance level at our value of k found in question 3, k = 12. 

# We can also see this without using the graph by the following condition:
# if our observed minimum p-value is less than the critical value, then we 
# reject the null hypothesis. 

min.p.2iii < min.p.critical

# [1] TRUE

# Hence we reject the null. 



## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##




## ;;
## ----------------------------------------------------------------------------|
## Q3: --                                                                      |
## ----------------------------------------------------------------------------|
## ;;


# Remove all variables before starting question
rm(list = ls())

# Prevent scientific notation in vector printout. 
options(scipen = 10)


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 3.1


# Read in author counts
counts <- as.matrix(read.csv('authorship.csv'))

# Remove first column denoting author index
counts <- counts[, 2:ncol(counts)]

# Remove names of columns and rows for cleaner printouts
counts <- unname(counts)


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 3.2


# Christie, Dickens, Martin

# Totals
# First column is author index. 
total.christie <- sum(counts[1, ])
total.dickens  <- sum(counts[2, ])
total.martin   <- sum(counts[3, ])

# MLE: Proportion of times each word was used


# Agatha Christie MLE
mle.christie <- counts[1, ] / total.christie


# [1] 0.0244736175 0.0047196850 0.0003221906 0.0029346389
# [5] 0.0223813481 0.0016091148 0.0026998888 0.0051889226
# [9] 0.0063217101 0.0054142197 0.0037620411 0.0063020163
# [13] 0.0025053140 0.0017356803 0.0035795452 0.0017351551
# [17] 0.0005713828 0.0003326939 0.0056728650 0.0026754685
# [21] 0.0076868843 0.0014539277 0.0061985582 0.0092521476
# [25] 0.0061455162 0.0028104367 0.0129359387 0.0017133607
# [29] 0.0070404025 0.0150618188 0.0019628155 0.0008077084
# [33] 0.0014670569 0.0014284570 0.0038069430 0.0033975745
# [37] 0.0062941387 0.0019058347 0.0206850554 0.0058524724
# [41] 0.0036869421 0.0013349771 0.0032226936 0.0006168098
# [45] 0.0005889759 0.0012157640 0.0037426099 0.0018499044
# [49] 0.0006498954 0.0008337042 0.0150352979 0.0395666839
# [53] 0.0008077084 0.0025507411 0.0043352620 0.0013365526
# [57] 0.0032473765 0.0249966849 0.0033017313 0.0003316436
# [61] 0.0158233125 0.0030365215 0.0047782412 0.0020087677
# [65] 0.0010947127 0.0025594064 0.0018596200 0.0066486271
# [69] 0.0026922739 0.0022787040 0.6211233081


# Charles Dickens MLE
mle.dickens <- counts[2, ] / total.dickens


# [1] 0.0219488560 0.0039018213 0.0001916143 0.0027108326
# [5] 0.0352034418 0.0017391677 0.0022457823 0.0086893033
# [9] 0.0070345713 0.0056477857 0.0029045382 0.0053798394
# [13] 0.0044024235 0.0010545322 0.0025521561 0.0016915909
# [17] 0.0007452829 0.0007298597 0.0071025382 0.0033165220
# [21] 0.0076862691 0.0014173707 0.0059029229 0.0078530493
# [25] 0.0131717186 0.0038675765 0.0170602079 0.0022421226
# [29] 0.0056287027 0.0127184318 0.0018873878 0.0010610675
# [33] 0.0020902428 0.0009640840 0.0063457532 0.0037264145
# [37] 0.0056177235 0.0020413589 0.0256846813 0.0060571554
# [41] 0.0028091231 0.0010040798 0.0031526173 0.0008440963
# [45] 0.0007552165 0.0014121425 0.0044716974 0.0018651678
# [49] 0.0015195824 0.0015823211 0.0133894740 0.0477263382
# [53] 0.0017992923 0.0017237445 0.0031607210 0.0003466311
# [57] 0.0046125980 0.0266181801 0.0025830026 0.0027056044
# [61] 0.0115752813 0.0034362482 0.0032001941 0.0034281445
# [65] 0.0038858753 0.0025667951 0.0017875288 0.0098267028
# [69] 0.0027840277 0.0023594961 0.5788513755


# George Martin MLE
mle.martin <- counts[3, ] / total.martin

# [1] 0.02267700156 0.00363238030 0.00006899812 0.00148431490
# [5] 0.02950496416 0.00070993932 0.00281751822 0.00860822811
# [9] 0.00493593154 0.00459322188 0.00243033043 0.00644704737
# [13] 0.00229233419 0.00111366385 0.00226610351 0.00197813615
# [17] 0.00156243674 0.00080573836 0.00620412837 0.00418208432
# [21] 0.00970763615 0.00128758473 0.00489715574 0.01089200882
# [25] 0.01488933785 0.00344420362 0.01022141553 0.00149001723
# [29] 0.00522903099 0.00933584464 0.00132750100 0.00063523888
# [33] 0.00209788495 0.00113590291 0.00489430458 0.00436113729
# [37] 0.00671790775 0.00146150561 0.02056771196 0.00507791940
# [41] 0.00355596916 0.00185211479 0.00205226636 0.00087302578
# [45] 0.00056110867 0.00101216249 0.00329537296 0.00161432789
# [49] 0.00046587986 0.00192168314 0.00905300937 0.05860335262
# [53] 0.00302166142 0.00132864146 0.00233966348 0.00029937200
# [57] 0.00253753412 0.02266559691 0.00249476669 0.00058106680
# [61] 0.01257818599 0.00412962295 0.00233510162 0.00359987706
# [65] 0.00026914969 0.00190742733 0.00355710963 0.00755614936
# [69] 0.00401500624 0.00379261560 0.60814542751


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 3.3


# Read data
unknown.text <- read.csv('unknowntext.csv', header = FALSE)

# Convert to vector, as only one row
unknown.text <- as.vector(as.matrix(unknown.text))



## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 3.4


# Normalise unknown text, given that there are 10000 words. 

unknown.text <- unknown.text / 10000

# Get test statistic for each author

# Agatha Christie
test.stat.christie <- sum((unknown.text - mle.christie) ^ 2)
test.stat.christie
# [1] 0.001080229

# Charles Dickens
test.stat.dickens <- sum((unknown.text - mle.dickens) ^ 2)
test.stat.dickens
# [1] 0.001112411


# George Martin
test.stat.martin <- sum((unknown.text - mle.martin) ^ 2)
test.stat.marti
# [1] 0.0000530682


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

## 3.5


multi.crit <- function(S, size, probvec, setseed = TRUE, seed = 1){
  # Simulates S multinomial random samples with 'size' observations each, using
  # probabilities given by probvec, and calculates the L2 distance between each 
  # normalised sample and probvec. Returns the value 'gamma' which is 
  # above 95% of the distances. I.e. find the value gamma such that
  # Pr(distance < gamma | probvec) = 0.95
  #
  # Args:
  #   S:       Number of multinomial samples
  #   size:    Number of observations to be split in each category for a given
  #            sample
  #   probvec: The probabilities with which to generate each multinomial sample
  #   setseed: Boolean stating whether or not to set a seed
  #   seed:    The seed to set, if setseed = TRUE. 
  #   
  # Returns:
  #   gamma:   The value that 95% of calculated distances are below
  
  
  # If user wants to specify a seed, then set the seed. 
  if (setseed == TRUE){
    set.seed(seed)
  }
  
  # Create a vector to store the test statistics, i.e. the distances
  test.stats <- numeric(S)
  
  # We want S samples for the distance
  for (i in 1:S) {
    
    # Data sampled from a particular probability vector, probvec
    
    multis <- rmultinom(1, size, prob = probvec)
    
    # Normalised to get in probability form, sum(multis) = 1.
    
    multis <- multis / size
    
    # Compute test statistic (distance) for the simulated sample to the 'true'
    # distribution (true: what it was sampled from)
    
    test.stat <- sum((multis - probvec) ^ 2)
    
    # Store in test.stats, to get sampling distribution for distance
    
    test.stats[i] <- test.stat
  }
  
  # Order the sampling distribution vector to get 95 percentile
  test.stats <- sort(test.stats)
  
  # Find the distance which 95% of test statistics are below
  gamma <- test.stats[0.95*S]
  
  return(gamma)
}


# Hence, the gammas (critical values at 5%) are as follows

# Christie

gamma.christie <- multi.crit(100000, 10000, mle.christie, setseed = TRUE)
gamma.dickens  <- multi.crit(100000, 10000, mle.dickens,  setseed = TRUE)
gamma.martin   <- multi.crit(100000, 10000, mle.martin,   setseed = TRUE)


# The distance for each author such that type 1 error is 5%. 

gamma.christie
# [1] 0.0001315494
gamma.dickens
# [1] 0.000138911
gamma.martin
# [1] 0.0001362562


## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##


## 3.6


# Reject if test.stat.author > gamma.author


test.stat.christie > gamma.christie
# [1] TRUE

test.stat.dickens > gamma.dickens
# [1] TRUE

test.stat.martin > gamma.martin
# [1] FALSE

# We reject the null for both Agatha Christie and Charles Dickens, concluding
# neither of them wrote the text. 
# We do not reject the null for George RR Martin, so we conclude he is the
# author of the unknown text. 

## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##
## --------------------------------------------------------------------------##

# END