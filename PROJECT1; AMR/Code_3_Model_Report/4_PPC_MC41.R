tictoc::tic()

if (!exists('routeddd')){
  source('Code_1_SETUP/2_5_1_EDA_Route.R')
}


set.seed(SEED);
##============================================================================##
####                            LOAD ALL MODELS                             ####
##============================================================================##


mc41 <- readRDS('Models/MC_4_1_Post.rds')


##============================================================================##
####                            THEME SIZE                                  ####
##============================================================================##



##============================================================================##
####                            RELEVANT DAATA                              ####
##============================================================================##
y <- as.numeric(data.cc$Proportion_Gr)
yrep <- posterior_predict(mc41, nsamples = 1000)
yreplin <- t(posterior_linpred(mc41, nsamples = 1000))
group.name <- data.cc$Name
group.prop  <- data.cc$Proportion_Gr


color_scheme_set('viridis')

##============================================================================##
####                              THEME & GUIDE                             ####
##============================================================================##
THEMESIZE <- theme(strip.text.x = element_text(size = 8),
                   axis.title = element_text(size = 16), 
                   axis.text.x = element_text(size = 11),
                   axis.text.y = element_text(size = 11), 
                   plot.title = element_text(size = 18))

GUIDE <- guides(color = guide_legend(label.theme = element_text(size = 17),
                                     keywidth = 1.4,
                                     keyheight = 1.4))



##============================================================================##
####                              PREDICTIVE PLOTS                          ####
##============================================================================##

mc41.barclass <- ppc_bars_grouped(y, yrep, group = group.name, prob = 0.95, 
                                 facet_args = list(scales = 'free')) +
  ggtitle('Frequency of Proportion Groups by Name') +
  THEMESIZE +
  GUIDE +
  scale_x_continuous(breaks = seq(1,7,1), 
                     labels = seq(1,7,1))
  

ggsave('Figures/4_MC41_BarClass.png', mc41.barclass, width = 14, height = 9)



##============================================================================##
####                                 ECDF                                   ####
##============================================================================##

mc41.ecdf <- ppc_ecdf_overlay(y, yrep, discrete = TRUE) +
  xlab('Proportion Group') +
  ylab('Cumulative Probability') +
  ggtitle('ECDF') +
  scale_x_continuous(breaks = seq(1, 7, 1), 
                     labels = seq(1, 7, 1)) +
  guides(color = guide_legend(nrow = 1, 
                              title.theme = element_text(size = 14), 
                              label.theme = element_text(size = 14), 
                              keywidth = 1.2, 
                              keyheight = 1.2)) +
  theme(axis.text.x  = element_text(size = 14), 
        axis.text.y  = element_text(size = 14), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(size = 18))
  


ggsave('Figures/4_MC41_ECDF.png', mc41.ecdf,  width = 10, height = 6)

##============================================================================##
####                                                                        ####
##============================================================================##


mc41.zeros <- ppc_stat_grouped(y, yrep, stat = 'zeros', group = group.name) + 
  xlab('Number of 0s') +
  ggtitle('0s by Name')+
  THEMESIZE


mc41.ones <- ppc_stat_grouped(y, yrep, stat = 'ones', group = group.name) + 
  xlab('Number of 1s') +
  ggtitle('1s by Name')+
  THEMESIZE 

mc41.halves <- ppc_stat_grouped(y, yrep, stat = 'halves', group = group.name) + 
  xlab('Number of 0.5s') +
  ggtitle('0.5s by Name')+
  THEMESIZE



ggsave('Figures/4_MC41_Zeros.png', mc41.zeros, width = 12, height = 10)
ggsave('Figures/4_MC41_Ones.png', mc41.ones, width = 12, height = 10)
ggsave('Figures/4_MC41_Halves.png', mc41.halves, width = 12, height = 10)









##============================================================================##
####                                BAR PREDICTIONS                         ####
##============================================================================##

mc41.barpred <- ppc_bars_grouped(y, yrep, group = group.prop, 
                                facet_args = list(scales = 'free')) +
  ggtitle('Distribution of Counts by True Value')+
  theme(strip.text.x = element_text(size = 8)) +
  xlab('Proportion Group') +
  THEMESIZE

## Bar pred
ggsave('Figures/4_MC41_BarPred.png', mc41.barpred, width = 10, height = 6)


##============================================================================##
####                            LINEAR PREDICTOR PLOT                       ####
##============================================================================##

veclinpred <- as.vector(yreplin)
linpreddf  <- data.frame(Proportion_Gr = rep(y, ncol(yreplin)), 
                         LinearPred = veclinpred,
                         Name = rep(group.name, ncol(yreplin)))
## average 

mc41.linpredplot <- ggplot(linpreddf) +
  aes(x = as.numeric(Proportion_Gr), y = LinearPred) +
  stat_summary(fun.y = 'mean', geom = 'bar') +
  ggtitle('Average of Linear Component by True Value') +
  xlab('Proportion Group') +
  ylab('Linear Predictor')+
  scale_x_continuous(breaks = seq(1, 7, 1),
                     labels = seq(1, 7, 1)) +
  THEMESIZE +
  theme(axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16))

## Linear Predictor
ggsave('Figures/4_MC41_LinPred.png', mc41.linpredplot, width = 10, height = 6)

##============================================================================##
####                              SAVE PREDICTIVES                          ####
##============================================================================##










# beepr::beep(1)
tictoc::toc()







