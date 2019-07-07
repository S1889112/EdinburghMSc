color_scheme_set('viridis')

mc1  <- readRDS('Models/MC_1_Post.rds')


##============================================================================##
####                              DEFINE VARIABLES                          ####
##============================================================================##
y <- as.numeric(data.cc$Proportion_Gr)
yrep <- posterior_predict(mc1, nsamples = 500)
yreplin <- t(posterior_linpred(mc1, nsamples = 1000))
group.class <- data.cc$Class_Gr
group.pred <- data.cc$Proportion_Gr

##============================================================================##
####                              AESTHETICS                                ####
##============================================================================##


THEMESIZE <- theme(axis.title = element_text(size = 20), 
                   axis.text.x = element_text(size = 16),
                   axis.text.y = element_text(size = 16), 
                   plot.title = element_text(size = 21))

GUIDE <- guides(color = guide_legend(label.theme = element_text(size = 17),
                                     keywidth = 1.4,
                                     keyheight = 1.4))



##============================================================================##
####                              PREDICTIVE PLOTS                          ####
##============================================================================##
## Pred Plot

overfit.predplot <- ppc_bars_grouped(y, yrep, group = group.pred, 
                                 facet_args =list(scales = 'free'), 
                                 prob = 0.95) + 
  ggtitle('Count Distribution for Overfit Model') +
  THEMESIZE +
  GUIDE

## Linpred Plot

veclinpred <- as.vector(yreplin)
linpreddf  <- data.frame(Proportion_Gr = rep(y, ncol(yreplin)), 
                         LinearPred = veclinpred,
                         Class_Gr = rep(group.class, ncol(yreplin)))
## average 

overfit.linpredplot <- ggplot(linpreddf) +
  aes(x = as.numeric(Proportion_Gr), y = LinearPred) +
  stat_summary(fun.y = 'mean', geom = 'bar') +
  ggtitle('Average of Linear Component by True Value (Overfit)') +
  xlab('Proportion Group') +
  ylab('Linear Predictor') +
  scale_x_continuous(breaks = seq(1, 7, 1),
                     labels = seq(1, 7, 1)) +
  THEMESIZE





ggsave('Figures/3_2_Overfit_Preds.png', plot = overfit.predplot, width = 14, height = 8)
ggsave('Figures/3_2_Overfit_LinPreds.png', plot = overfit.linpredplot, width = 10, height = 8)

