### Funcs from Q1 ###

demean <- function(x) {x - mean(x)}


#### Load ####

require(rjags)
require(runjags)
require(gridExtra)
require(corrplot)
require(dplyr)


cows <- read.csv('Q2/Data/cows.csv')

## ===========================================================================##
#################################### 2a: EDA ###################################
## ===========================================================================##

# -- Correlation between variables of farm environment

### To avoid inflation of correlation, get unique
env.names <- c('temp', 'rain', 'permeab', 'hight', 'slope')
env.df <- cows[, env.names]
env.df.unique <- distinct(env.df)

png('Q2/Q2aEDA.png', width = 930, height = 600)
par(mfrow = c(3, 3))
## Correlation Plot ##
corrplot(corr = cor(env.df.unique), type = 'full',method = 'number', order = 'hclust')

# -- Farm barplot data

barplot.2('farmID', xlab = 'Farm ID', srt = 90)

# -- Age barplot data

barplot.2('age', xlab = 'Age', srt = 90)

# -- Boxplots
# Remove cowID, farmID, and parasite for boxplot variables
var.names <- colnames(cows)[!(colnames(cows) %in% c('cowID', 'farmID', 'parasite'))]
boxplot.mw(cows, 'parasite', var.names = var.names)
par(mfrow = c(1,1))
dev.off()


