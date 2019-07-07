## 6.5 minutes to run

tictoc::tic()

if (!exists('routeddd')){
  source('Code_1_SETUP/2_5_1_EDA_Route.R')
}


set.seed(SEED);
##============================================================================##
####                            LOAD ALL MODELS                             ####
##============================================================================##

mp.1.post <- readRDS('Models/MP_1_Post.rds')


##============================================================================##
####                            RELEVANT DAATA                              ####
##============================================================================##

y           <- as.numeric(data.cc$Proportion_Gr)
yrep        <- posterior_predict(mp.1.post, nsamples = 1000)
yreplin     <- t(posterior_linpred(mp.1.post, nsamples = 500))
group.class <- data.cc$Class_Gr
group.prop  <- data.cc$Proportion_Gr


color_scheme_set('viridis')

##============================================================================##
####                              PREDICTIVE PLOTS                          ####
##============================================================================##

THEMESIZE <- theme(axis.title = element_text(size = 20), 
                   axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16), 
                   plot.title = element_text(size = 21))

GUIDE <- guides(color = guide_legend(label.theme = element_text(size = 17),
                                     keywidth = 1.4,
                                     keyheight = 1.4))

##============================================================================##
####                                BAR CLASS                               ####
##============================================================================##

mp1.barclass <- ppc_bars_grouped(y, yrep, group = group.class, prob = 0.95, 
                                 facet_args = list(scales = 'free')) +
  ggtitle('Frequency of Proportion Groups by Class Group') +
  scale_x_continuous(breaks = seq(1, 7, 1), labels = seq(1,7,1)) +
  xlab('Proportion Group') +
  THEMESIZE +
  GUIDE

ggsave('Figures/3_1_Primary_PostPred_BarClass.png', mp1.barclass, width = 11, 
       height = 7)


##============================================================================##
####                                ECDF                                    ####
##============================================================================##


mp1.ecdf <- ppc_ecdf_overlay(y, yrep, discrete = TRUE) +
  xlab('Proportion Group') +
  ylab('Cumulative Probability') +
  ggtitle('ECDF') +
  scale_x_continuous(breaks = seq(1, 7, 1), 
                     labels = seq(1, 7, 1)) +
  THEMESIZE +
  GUIDE

mp1.ecdf

ggsave('Figures/3_1_Primary_PostPred_ECDF.png', mp1.ecdf, width=10,height=6)



##============================================================================##
####                            TEST STATISTICS                             ####
##============================================================================##

TESTSTATTHEME <- theme(axis.title = element_text(size = 14), 
                       axis.text.x = element_text(size = 11),
                       axis.text.y = element_text(size = 11), 
                       plot.title = element_text(size = 16))


mp1.zeros <- ppc_stat_grouped(y, yrep, stat = 'zeros', group = group.class) + 
  xlab('Number of 0s') +
  ggtitle('0s by Class Group') +
  TESTSTATTHEME

mp1.ones <- ppc_stat_grouped(y, yrep, stat = 'ones', group = group.class) + 
  xlab('Number of 1s') +
  ggtitle('1s by Class Group') +
  TESTSTATTHEME

mp1.halves <- ppc_stat_grouped(y, yrep, stat = 'halves', group = group.class) + 
  xlab('Number of 0.5s') +
  ggtitle('0.5s by Class Group') +
  TESTSTATTHEME


ggsave('Figures/3_1_Primary_PostPred_Stats.png', 
       plot_grid(plot_grid(mp1.halves, mp1.ones, nrow = 1), mp1.zeros, nrow = 2), 
       height = 14, 
       width = 9)

##============================================================================##
####                        PREDICTION PLOT                                 ####
##============================================================================##


mp1.barpred <- ppc_bars_grouped(y, yrep, group = group.prop, 
                                facet_args = list(scales = 'free')) +
  ggtitle('Distribution of Counts by True Value') +
  THEMESIZE +
  xlab('Proportion Group')
  

ggsave('Figures/3_1_Primary_PostPred_BarPred.png', mp1.barpred, width = 14, 
       height = 8)


##============================================================================##
####                              LINEAR PREDICTOR PLOTS                    ####
##============================================================================##

# as.vector goes columnwise
# each column of yreplin denotes a simulated dataset. 
# mymat <- matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
# 
# mymat
# 
# as.vector(mymat)



veclinpred <- as.vector(yreplin)
linpreddf  <- data.frame(Proportion_Gr = rep(y, ncol(yreplin)), 
                         LinearPred = veclinpred,
                         Class_Gr = rep(group.class, ncol(yreplin)))


## average 

mp1.linpredplot <- ggplot(linpreddf) +
  aes(x = as.numeric(Proportion_Gr), y = LinearPred) +
  stat_summary(fun.y = 'mean', geom = 'bar') +
  ggtitle('Average of Linear Component by True Value') +
  xlab('Proportion Group') +
  ylab('Linear Predictor') +
  scale_x_continuous(breaks = seq(1, 7, 1),
                     labels = seq(1, 7, 1)) +
  THEMESIZE
  


ggsave('Figures/3_1_Primary_Linpred.png', mp1.linpredplot, width = 10, height = 6)





##============================================================================##
####                                   END                                  ####
##============================================================================##


# beepr::beep(1)
tictoc::toc()








