##============================================================================##
####                            LOAD REQUISITE DATA                         ####
##============================================================================##
if (!exists('routeddd')) {
  source('Code_1_SETUP/2_5_1_EDA_Route.R')
}

##============================================================================##
####                            MODEL DESCRIPTION                           ####
##============================================================================##

## mc.1

## Full model, should overfit. 
##============================================================================##
####                            SET THE PRIOR                               ####
##============================================================================##

## Exponential 1, standard deviation of 1 
## Remember, everything on the '1' scale. 
## Also, numeric covariates have standard deviation 1. 

PRIOR.mc.1 <- c(set_prior('normal(0,3)', class = 'Intercept'), 
                set_prior('normal(0,3)', class = 'b'),
                set_prior('normal(0,3)', class = 'b', dpar = 'disc'))




##============================================================================##
####                            FIT THE MODEL                               ####
##============================================================================##

start <- Sys.time()

mc.1.post <- brm(bf(Proportion_Gr ~ AvgAge_sc + l_DDDp1kOBD*Route*Name) +
                   lf(disc ~ 0 + Class_Gr, cmc = FALSE),
                 family = cumulative('logit', 'log', 'flexible'),
                 data = data.cc,
                 prior = PRIOR.mc.1,
                 chains = 3,
                 cores = 4,
                 warmup = 1000,
                 iter = 4000,
                 seed = SEED,
                 control = list(adapt_delta = 0.9))

#beepr::beep(1)

end <- Sys.time()
end - start

beepr::beep(1)

##============================================================================##
####                            SAVE THE MODEL                              ####
##============================================================================##


saveRDS(mc.1.post, 'Models/MC_1_Post.rds')










