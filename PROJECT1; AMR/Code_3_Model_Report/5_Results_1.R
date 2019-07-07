if (!exists('data.cc')){
  source('Code_1_SETUP/2_2_1_EDA_Bivariate_ClassGr.R')
}

##============================================================================##
####                            LOAD MODEL                                  ####
##============================================================================##

if (!exists('mc41')){
  mc41 <- readRDS('Models/MC_4_1_Post.rds')
}

##============================================================================##
####                            RESULTS FUNCTION                            ####
##============================================================================##

keystats <- function(x){
  resvec <- c(mean(x), sd(x), quantile(x, probs=c(0.025, 0.05, 0.95, 0.975)))
  names(resvec) <- c('Mean', 'SD', '2.5%', '5%', '95%', '97.5%')
  resvec
}


##============================================================================##
####                            GET AS DATAFRAME                            ####
##============================================================================##

keystats <- function(x){
  resvec        <- c(mean(x), sd(x), 
                     quantile(x, probs = c(0.025, 0.05, 0.95, 0.975)),
                     mean(x>0), 
                     mean(x<0))
  names(resvec) <- c('Mean', 'SD', '2.5%', '5%', '95%', '97.5%', 'Pr(>0)', 'Pr(<0)')
  resvec
}

mc41.df  <- data.frame(mc41)
mc41.df$b_l_DDDp1kOBD.RouteNonMInvasive.Combined <- mc41.df$b_l_DDDp1kOBD  + mc41.df$b_l_DDDp1kOBD.RouteNonMInvasive 
mc41.fit <- mc41$fit

resmat <- round(t(apply(mc41.df, 2, keystats)), 3)


##============================================================================##
####                              ROW NAMES                                 ####
##============================================================================##

## colnames(mc41.df) == rownames(resmat)

allnames       <- rownames(resmat)
allnames       <- allnames[allnames != 'lp__']
betanames.main <- allnames[grep('b_[^d]', allnames)]
betanames.disc <- allnames[grep('b_d', allnames)]
betanames.ddd  <- betanames.main[grep('b_[^AIR]', betanames.main)]
sdnames        <- allnames[grep('sd_', allnames)]
rnames         <- allnames[grep('r_', allnames)]

##============================================================================##
####                              RESULTS                                   ####
##============================================================================##


### Population effects 

xtable(resmat[betanames.main, ], 
       caption = 'Results Table (Population)', 
       label = 'fig::5_Results_DDD', 
       align = rep('l', ncol(resmat) + 1))

### DIfference table

difftab <- data.frame(InterceptDiff = diff(resmat[allnames[grep('b_[I]', allnames)], 1]),
                      `DDD/Difference Ratio` = 0.239/diff(resmat[allnames[grep('b_[I]', allnames)], 1])
)

### differences in intercepts:

interceptdiff <- diff(resmat[allnames[grep('b_[I]', allnames)], 1])

## Percentage of difference accounted for by unit increase in DDD. 
ddd.difference.ratio <- 0.239/diff(resmat[allnames[grep('b_[I]', allnames)], 1])

## log DDD change to next group
difference.ddd.ratio <- 1/(0.239/diff(resmat[allnames[grep('b_[I]', allnames)], 1]))

## Transition values

transition.ratios <- exp(1/(0.239/diff(resmat[allnames[grep('b_[I]', allnames)], 1])))



difftab <- data.frame(InterceptDiff = interceptdiff, 
                      `log DDD/Difference Ratio` = ddd.difference.ratio, 
                      `Difference/log DDD Ratio` = difference.ddd.ratio, 
                      `Transition Ratio` = transition.ratios)

xtable(difftab, 
       caption = 'Group Transition Table', 
       label = 'tab::5_Transition_DDD', 
       align = rep('l', ncol(difftab) + 1))


##============================================================================##
####                              PLOTS                                     ####
##============================================================================##
plotnames <- betanames.main[grep('b_[^AI]', betanames.main)]


axnames <- c(expression(beta[lDDD]), 
             expression(beta[NonInvasive]),
             expression(beta[lDDD*NonInvasive]), 
             expression(beta[lDDD*NonInvasive] + beta[lDDD]))


areas.ddd <- mcmc_areas(mc41.df[, plotnames], prob = 0.9) +
  ggtitle('Density and Symmetric 90% Area Plots of DDD and Route Parameters') +
  scale_y_discrete(name = '', 
                   breaks = plotnames, 
                   labels = axnames)


##============================================================================##
####                            SAVE PLOTS                                  ####
##============================================================================##

ggsave('Figures/5_1_Areas_DDD.png', plot = areas.ddd, width = 10, height = 8)

