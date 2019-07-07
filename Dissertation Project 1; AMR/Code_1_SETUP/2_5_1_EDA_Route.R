if (!exists('ageddd')){
  source('Code_1_SETUP/2_4_1_EDA_Age.R')
}

routeprop <- ggplot(data.cc[Class_Gr !='A']) +
  aes(x = l_DDDp1kOBD, y = Proportion, color = Route) +
  stat_smooth(method = 'lm', formula = y~x, fullrange=TRUE) +
  xlab('Log DDD/1000 OBD') +
  ylab('Proportion') +
  ggtitle('Proportion vs Log DDD by Route') +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 16), 
        axis.title.x = element_text(size = 16),
        plot.title = element_text(size = 18)) +
  guides(color = guide_legend(title.theme = element_text(size = 14), 
                              label.theme = element_text(size = 14), 
                              keywidth = 1.3, 
                              keyheight = 1.3))

routeprop



##============================================================================##
####                            SAVE PLOTS                                  ####
##============================================================================##


ggsave('Figures/2_5_1_EDA_RouteProp.png', plot = routeprop, width = 8, height = 5)




