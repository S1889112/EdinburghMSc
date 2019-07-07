source('Code_3_Model_Report/5_Results_1.R')
### Load models
 

##============================================================================##
####                            FIXED EFFECTS MODELS                        ####
##============================================================================##

mp1 <- readRDS('Models/MP_1_Post.rds')
mc31 <- readRDS('Models/MC_3_1_Post.rds')
mc41 <- readRDS('Models/MC_4_1_Post.rds')


## pr >0
mp1names <- parnames(mp1)
mc31names <- parnames(mc31)
mc41names <- parnames(mc41)

mp1names <- mp1names[grep('DDD', mp1names)]
mc31names <- mc31names[grep('DDD', mc31names)]
mc41names <- mc41names[grep('DDD', mc41names)]

### Pr>0 ###

t(apply(as.matrix(mp1)[,mp1names], 2, keystats))
t(apply(as.matrix(mc31)[,mc31names], 2, keystats))
t(apply(as.matrix(mc41)[,mc41names], 2, keystats))


##============================================================================##
####                            OTHER MODELS                                ####
##============================================================================##

dddparget <- function(x) {
  x.colnames <- colnames(x)
  x.dddnames <- x.colnames[grep('DDD', x.colnames)]
  x.dddnames <- x.dddnames[grep('^[br].', x.dddnames)]
  x[, x.dddnames]
}


mc2  <- readRDS('Models/MC_2_Post.rds')
mc32 <- readRDS('Models/MC_3_2_Post.rds')
mc33 <- readRDS('Models/MC_3_3_Post.rds')
mc34 <- readRDS('Models/MC_3_4_Post.rds')
mc42 <- readRDS('Models/MC_4_2_Post.rds')
mc43 <- readRDS('Models/MC_4_3_Post.rds')
mc44 <- readRDS('Models/MC_4_4_Post.rds')

mc2.m  <- dddparget(as.matrix(mc2))
mc32.m <- dddparget(as.matrix(mc32))
mc33.m <- dddparget(as.matrix(mc33))
mc34.m <- dddparget(as.matrix(mc34))
mc42.m <- dddparget(as.matrix(mc42))
mc43.m <- dddparget(as.matrix(mc43))
mc44.m <- dddparget(as.matrix(mc44))


mcmc_intervals(mc2)


## get ddd params from df

posprob.mc2  <- apply(mc2.m, 2, keystats)
posprob.mc32 <- apply(mc32.m, 2, keystats)
posprob.mc33 <- apply(mc33.m, 2, keystats)
#posprob.mc34 <- apply(mc34.m, 2, keystats)
posprob.mc42 <- apply(mc42.m, 2, keystats)
#posprob.mc43 <- apply(mc43.m, 2, keystats)
#posprob.mc44 <- apply(mc44.m, 2, keystats)

mean(mc34.m>0)
mean(mc43.m>0)
mean(mc44.m>0)












