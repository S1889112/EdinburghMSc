tictoc::tic()

if (!exists('routeddd')){
  source('Code_1_SETUP/2_5_1_EDA_Route.R')
}


set.seed(SEED);
##============================================================================##
####                            LOAD MODEL                                  ####
##============================================================================##
if (!exists('mp.1.pr')){
  mp.1.pr <- readRDS('Models/MP_1_Prior.rds')
}
##============================================================================##
####                            FUNCTION                                    ####
##============================================================================##

priorpred <- function(y, yrep) {
  THEMESIZE <-   theme(axis.text.x = element_text(size = 16),
                       axis.text.y = element_text(size = 16), 
                       axis.title.x = element_text(size = 18), 
                       axis.title.y = element_text(size = 18),
                       plot.title = element_text(size = 22))
  
  
  ecdfplot <- ppc_ecdf_overlay(y, yrep, discrete = TRUE) +
    xlab('Proportion Group') +
    ylab('Cumulative Probability') +
    ggtitle('ECDF') +
    scale_x_continuous(breaks = seq(1, 7, 1), 
                       labels = seq(1, 7, 1)) + THEMESIZE
  
  zerohist <- ppc_stat(y, yrep, stat = 'zeros')  + xlab('Number of 0s') + 
    THEMESIZE
  
  oneshist <- ppc_stat(y, yrep, stat = 'ones')   + xlab('Number of 1s') + 
    THEMESIZE
  halfhist <- ppc_stat(y, yrep, stat = 'halves') + xlab('Number of 0.5s') + 
    THEMESIZE
  
  list(zerohist = zerohist, oneshist = oneshist, halfhist = halfhist, 
       ecdfplot = ecdfplot)
}



##============================================================================##
####                              PRIOR PRED PLOT                           ####
##============================================================================##
color_scheme_set('viridis')

y    <- as.numeric(data.cc$Proportion_Gr)
yrep <- posterior_predict(mp.1.pr, nsamples = 500)


GUIDE <- guides(color = guide_legend(label.theme = element_text(size = 17),
                                    keywidth = 1.4,
                                    keyheight = 1.4))

# mp1.plotlist[[4]] + GUIDE



mp1.plotlist <- priorpred(y = y, yrep = yrep)

mp1.plotgrid <- plot_grid(plot_grid(mp1.plotlist[[1]], # + THEMECONSTANT, 
                                    mp1.plotlist[[2]], # + THEMECONSTANT, 
                                    mp1.plotlist[[3]], # + THEMECONSTANT, 
                                    nrow = 3), 
                          mp1.plotlist[[4]] + GUIDE)


ggsave('Figures/3_1_Primary_PriPred.png', 
       plot = mp1.plotgrid, 
       width = 11, 
       height = 8)
file.show('Figures/3_1_Primary_PriPred.png')

beepr::beep(1)
tictoc::toc()


