##============================================================================##
####                            LOAD REQUISITE DATA                         ####
##============================================================================##
if (!exists('routeddd')) {
  source('Code_1_SETUP/2_5_1_EDA_Route.R')
}

##============================================================================##
####                            MODEL DESCRIPTION                           ####
##============================================================================##

## mc.2: Random Slopes

# Usual model, but with random slopes on everything. 
# 
# 

##============================================================================##
####                            SET THE PRIOR                               ####
##============================================================================##

## Exponential 1, standard deviation of 1 
## Remember, everything on the '1' scale. 
## Also, numeric covariates have standard deviation 1. 

PRIOR.mc.3.2 <- c(set_prior('normal(0,3)', class = 'Intercept'), 
                  set_prior('exponential(1)', class = 'sd'),
                  set_prior('normal(0,3)', class = 'b', dpar = 'disc'))



##============================================================================##
####                            FIT THE MODEL                               ####
##============================================================================##

start <- Sys.time()

mc.3.2.post <- brm(bf(Proportion_Gr ~ AvgAge_sc + (1 + l_DDDp1kOBD*Route|Class)) +
                     lf(disc ~ 0 + Class_Gr, cmc = FALSE),
                   family = cumulative('logit', 'log', 'flexible'),
                   data = data.cc,
                   prior = PRIOR.mc.3.2,
                   chains = 3,
                   cores = 4,
                   warmup = 2000,
                   iter = 5000,
                   seed = SEED,
                   control = list(adapt_delta = 0.99))

#beepr::beep(1)

end <- Sys.time()
end - start

beepr::beep(1)

##============================================================================##
####                            SAVE THE MODEL                              ####
##============================================================================##


saveRDS(mc.3.2.post, 'Models/MC_3_2_Post.rds')










