#------------------------------------#
############## PREAMBLE ##############
#------------------------------------# 

load(file = 'databp.Rdata')
databp$missing <- ifelse(databp$R==0, 1, 0)
n <- nrow(databp)


#------------------------------------#
############### PART A ###############
#------------------------------------#

# Create df with only observed data

databp.cca <- databp[databp$missing == 0, ]


# Calculate statistics
mean.cca             <- mean(databp.cca$recovtime)
se.cca               <- sd  (databp.cca$recovtime)/sqrt(n)
cor.recov.dose.cca   <- cor (databp.cca$recovtime, exp(databp.cca$logdose))
cor.recov.bloodp.cca <- cor (databp.cca$recovtime, databp.cca$bloodp )


#### Results Part A ####

c(mean.cca, se.cca, cor.recov.dose.cca, cor.recov.bloodp.cca)



#------------------------------------#
############### PART B ###############
#------------------------------------#

# Already have complete case mean, which is the mean to impute
# Impute into new df
df.MI <- databp
df.MI$recovtime <- ifelse(df.MI$missing == 1, mean.cca, df.MI$recovtime)

# Calculate statistics


mean.MI             <- mean(df.MI$recovtime)
se.MI               <- sd  (df.MI$recovtime)/sqrt(n)
cor.recov.dose.MI   <- cor (df.MI$recovtime, exp(df.MI$logdose))
cor.recov.bloodp.MI <- cor (df.MI$recovtime, df.MI$bloodp)



#### Results Part B ####
c(mean.MI, se.MI, cor.recov.dose.MI, cor.recov.bloodp.MI)




#------------------------------------#
############### PART C ###############
#------------------------------------#

# Create new df
df.RI <- databp


# Fit model (lm ignores missing values), and generate predictions
mod            <- lm(recovtime ~ bloodp + logdose, data = df.RI)
predictions.RI <- predict(mod, df.RI)

# Impute predictions to missing data
# If data missing, use prediction, otherwise use true value
df.RI$recovtime <- ifelse(df.RI$missing==1, predictions.RI, df.RI$recovtime)

# Calculate statistics


mean.RI             <- mean(df.RI$recovtime)
se.RI               <- sd  (df.RI$recovtime)/sqrt(n)
cor.recov.dose.RI   <- cor (df.RI$recovtime, exp(df.RI$logdose))
cor.recov.bloodp.RI <- cor (df.RI$recovtime, df.RI$bloodp)




#### Results Part C ####
c(mean.RI, se.RI, cor.recov.dose.RI, cor.recov.bloodp.RI)



#------------------------------------#
############### PART D ###############
#------------------------------------#

set.seed(36)

# Already have regression results from part C
# Just need to add noise onto predictions
# Noise normally distributed, with mean 0 and variance of regression residuals


# SRI is just an extension of RI, so use df.RI as basis for new df
# Get noise scale parameter
df.SRI   <- df.RI
noise.sd <- summary(mod)$sigma

# Add normal error if data was missing
df.SRI$recovtime <- ifelse(df.SRI$missing==1, 
                           df.SRI$recovtime + rnorm(1, 0, noise.sd),
                           df.SRI$recovtime)


# Calculate statistics

mean.SRI             <- mean(df.SRI$recovtime)
se.SRI               <- sd  (df.SRI$recovtime)/sqrt(n)
cor.recov.dose.SRI   <- cor (df.SRI$recovtime, exp(df.SRI$logdose))
cor.recov.bloodp.SRI <- cor (df.SRI$recovtime, df.SRI$bloodp)

#### Results Part D ####
c(mean.SRI, se.SRI, cor.recov.dose.SRI, cor.recov.bloodp.SRI)

# Linear regression assumption of normally distributed errors
jpeg('LaTeX/qqplot.jpg')
qqnorm(rstandard(mod))
qqline(rstandard(mod))
dev.off()


#------------------------------------#
############### PART E ###############
#------------------------------------#


# Distance function
distance <- function(x,y) sqrt((x-y)^2)


# Predict for every person in dataset. Find smallest squared prediction 
# difference. Use that person's TRUE value

# New df

df.HD <- databp

# Regression model recycled from RI section

mod.HD <- mod

# Predict for each item in data, and store in the dataframe for validity checks

df.HD$predictions        <- predict.lm(mod.HD, newdata = df.HD)
df.HD$hotdeckpredictions <- numeric(n)


# Loop through rows

for (person in 1:n){
  # If data not missing, just use observed value
  if (df.HD[person, 'R'] == 1) {
    df.HD[person, 'hotdeckpredictions'] <- df.HD[person, 'recovtime']
  }
  
  else {
    # Get missing person's predicted value, and calculate distance to
    # all others' predicted value
    predval   <- df.HD[person, 'predictions']
    distances <- distance(predval, df.HD$predictions)
    
    
    # Find index of smallest distance that doesn't belong to the current or any 
    # other missing person
    mindist <- min(distances[distances>0 & df.HD$missing==0])
    minindex <- which(distances == mindist)
    
    
    # Use that index's true value as the prediction for the missing person
    df.HD[person, 'hotdeckpredictions'] <- df.HD[minindex, 'recovtime']
    
    
    # Create min index column for manual inspection of process. 
    df.HD[person, 'minindex'] <- minindex
  }
}

# View(df.HD)
# Sort by predictions in view to see method has used nearest prediction for
# the true value

# Perform imputation
df.HD$recovtime <- df.HD$hotdeckpredictions


# Calculate statistics

mean.HD             <- mean(df.HD$recovtime)
se.HD               <- sd  (df.HD$recovtime)/sqrt(n)
cor.recov.dose.HD   <- cor (df.HD$recovtime, exp(df.HD$logdose))
cor.recov.bloodp.HD <- cor (df.HD$recovtime, df.HD$bloodp)

#### Results Part  ####
c(mean.HD, se.HD, cor.recov.dose.HD, cor.recov.bloodp.HD)










