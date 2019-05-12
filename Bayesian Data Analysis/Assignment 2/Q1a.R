#### Load packages and data

require(rjags)
require(runjags)
require(gridExtra)


gulls <- read.csv('Q1/Data/gulls_data.csv')


## ===========================================================================##
#################################### 1a: EDA ###################################
## ===========================================================================##

year.raw <- gulls$year




png('Q1/Q1a.png', width = 1200, height = 250)
par(mfrow = c(1, 4))

## Univariate plot of Audouin
boxplot(gulls$audouin, main = 'Boxplot of Audouin', pch = 16, cex = 1.5)


## Plot Audouin gulls per year

plot(gulls$year, gulls$audouin, xlab = 'Year', ylab = 'Audouin', 
     main = 'Audouin abundance over time', pch = 'x')

abline(lm(audouin~year, data = gulls))
abline(lm(audouin~year, data = gulls[gulls$audouin < 525, ]), col = 'red')
abline(lm(audouin~year, data = gulls[gulls$audouin < 475, ]), col = 'green')


# Label years 1995 to 1997 for both time series plots
# Done by slicing the data to only include these years for the text addition

years <- c(1995, 1996, 1997)

text(x      = gulls$year[gulls$year%in%years], 
     y      = gulls$audouin[gulls$year%in%years], 
     labels = year.raw[gulls$year%in%years])



## Plot yellowlegged gulls against year

plot(gulls$year, gulls$yellowlegged, xlab = 'Year', ylab = 'Yellowlegged', 
     main = 'Yellowlegged abundance over time')

abline(lm(yellowlegged~year, data = gulls))



text(x      = gulls$year[gulls$year%in%years], 
     y      = gulls$yellowlegged[gulls$year%in%years], 
     labels = year.raw[gulls$year%in%years])


## Plot yellowlegged against Audouin

plot(gulls$yellowlegged, gulls$audouin, xlab = 'Yellowlegged', ylab = 'Audouin', 
     main = 'Yellowlegged vs Audouin')
abline(lm(audouin~yellowlegged, data = gulls))
abline(lm(audouin~yellowlegged, data = gulls[gulls$audouin < 525, ]), col='red')
abline(lm(audouin~yellowlegged, data = gulls[gulls$audouin < 475, ]), col='green')

text(x      = gulls$yellowlegged[gulls$year%in%years], 
     y      = gulls$audouin[gulls$year%in%years], 
     labels = year.raw[gulls$year%in%years])



par(mfrow = c(1,1))
dev.off()

#### Get correlations 

## All data

cor(gulls$audouin, gulls$year)
cor(gulls$audouin, gulls$yellowlegged)

#### Remove outliers for audouin

sort(gulls$audouin)

# [1]   3  20  21  25  28  30  41  50  59  60  62  70  79
# [14]  80 100 120 170 187 201 225 275 300 430 476 525 625

slice <- function(boundary) {gulls$audouin < boundary}


upp <- 525
cor(gulls$audouin[slice(upp)], gulls$yellowlegged[slice(upp)])
cor(gulls$audouin[slice(upp)], gulls$year[slice(upp)])

upp <- 625
cor(gulls$audouin[slice(upp)], gulls$yellowlegged[slice(upp)])
cor(gulls$audouin[slice(upp)], gulls$year[slice(upp)])



