##============================================================================##
####                            LOAD REQUISITE DATA                         ####
##============================================================================##
if (!exists('routeddd')) {
  source('Code_1_SETUP/2_5_1_EDA_Route.R')
}

##============================================================================##
####                            MODEL DESCRIPTION                           ####
##============================================================================##


##============================================================================##
####                            SET THE PRIOR                               ####
##============================================================================##

## Exponential 1, standard deviation of 1 
## Remember, everything on the '1' scale. 
## Also, numeric covariates have standard deviation 1. 

PRIOR.mp.1 <- c(set_prior('normal(0,3)', class = 'Intercept'), 
                set_prior('normal(0,3)', class = 'b'),
                set_prior('exponential(1)', class = 'sd'), 
                set_prior('normal(0,3)', class = 'b', dpar = 'disc'))




##============================================================================##
####                            FIT THE MODEL                               ####
##============================================================================##

start <- Sys.time()

mp.1.pr <- brm(bf(Proportion_Gr ~ AvgAge_sc + l_DDDp1kOBD*Route + (1|Class_Gr)) +
                 lf(disc ~ 0 + Class_Gr, cmc = FALSE),
               family = cumulative('logit', 'log', 'flexible'),
               data = data.cc,
               prior = PRIOR.mp.1,
               sample_prior = 'only',
               chains = 1,
               cores = 4,
               warmup = 0,
               iter = 10000,
               seed = SEED,
               control = list(adapt_delta = 0.1))

#beepr::beep(1)

end <- Sys.time()
end - start

beepr::beep(1)

##============================================================================##
####                            SAVE THE MODEL                              ####
##============================================================================##


saveRDS(mp.1.pr, 'Models/MP_1_Prior.rds')










