set.seed(SEED)

## 11 minutes for loo
## Results are cnosistent


start <- Sys.time()

##============================================================================##
####                            LOAD MODELS                                 ####
##============================================================================##

mp1  <- readRDS('Models/MP_1_Post.rds')
mc1  <- readRDS('Models/MC_1_Post.rds')
mc2  <- readRDS('Models/MC_2_Post.rds')
mc31 <- readRDS('Models/MC_3_1_Post.rds')
mc32 <- readRDS('Models/MC_3_2_Post.rds')
mc33 <- readRDS('Models/MC_3_3_Post.rds')
mc34 <- readRDS('Models/MC_3_4_Post.rds')
mc41 <- readRDS('Models/MC_4_1_Post.rds')
mc42 <- readRDS('Models/MC_4_2_Post.rds')
mc43 <- readRDS('Models/MC_4_3_Post.rds')
mc44 <- readRDS('Models/MC_4_4_Post.rds')

##============================================================================##
####                            ADD LOO TO ALL MODELS                       ####
##============================================================================##

mp1  <- add_loo(mp1)
mc1  <- add_loo(mc1)
mc2  <- add_loo(mc2)
mc31 <- add_loo(mc31)
mc32 <- add_loo(mc32)
mc33 <- add_loo(mc33)
mc34 <- add_loo(mc34)
mc41 <- add_loo(mc41)
mc42 <- add_loo(mc42)
mc43 <- add_loo(mc43)
mc44 <- add_loo(mc44)

end <- Sys.time()
end - start
beepr::beep(1)

##============================================================================##
####                            MODEL COMPARISON                            ####
##============================================================================##

## Name as the random intercept is best
## Doesn't matter much about random vs fixed slopes. 

lootab  <- loo_compare(mp1, 
                       mc1, 
                       mc2, 
                       mc31, mc32, mc33, mc34, 
                       mc41, mc42, mc43, mc44)
lootab <- round(as.table(lootab), 2)


lootab

##============================================================================##
####                            PRINT AS LATEX                              ####
##============================================================================##

require(xtable)


xtable(lootab, caption = 'Model Comparison', label = 'fig::4_Model_Comp', 
       align = '|lllllllll|')

       
       caption = 'Model Comparison', label = 'fig::4_Model_Comp', 
               align = 'l')








