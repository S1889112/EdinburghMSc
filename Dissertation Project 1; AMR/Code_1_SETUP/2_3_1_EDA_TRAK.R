if (!exists('%+na%')){
  source('Code_1_SETUP/2_2_1_EDA_Bivariate_ClassGr.R')
}




##============================================================================##
####                            INSPECT TRAK                                ####
##============================================================================##



t.trakage <- data.cc[, .(MA = mean(AvgAge), .N), by = TRAK][order(MA)]
Ntrakage <- t.trakage[, N]

trakage <- ggplot(data.cc) +
  aes(x = reorder(TRAK, AvgAge), y = AvgAge) +
  stat_summary(fun.y    = 'mean', geom = 'point') +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', fun.args = list(mult=2)) +
  annotate(geom = 'text', 
           x = t.trakage$TRAK, 
           y = 45, 
           label = Ntrakage, 
           size = 4) +
  xlab('TRAK') +
  ylab('Average Age (\u00b1 2 se)') +
  ggtitle('Mean Age (\u00b1 2 se) by TRAK') +
  theme(axis.text.x = element_text(size = 14, angle = 90), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(size = 18)
        ) 

trakage



##============================================================================##
####                            SAVE PLOTS                                  ####
##============================================================================##

ggsave('Figures/2_3_1_EDA_TRAKAge.png', plot = trakage, width = 10, height=6)


  
##============================================================================##
####                            COMMENTS                                    ####
##============================================================================##

## TRAK super correlated with age. TRAK is pointless complexity. 

trakagemod <- lm(AvgAge_sc ~ TRAK, data = data.cc)
trakagemodsum <- summary(trakagemod)

trakagemodsum



##============================================================================##
####                            COMMENTS                                    ####
##============================================================================##
RUNEXTRA <- 0


if (RUNEXTRA == 1){
  
  trakddd <- ggplot(data.cc) +
  aes(x = reorder(TRAK, l_DDDp1kOBD), y = l_DDDp1kOBD) +
  stat_summary(fun.y    = 'mean', geom = 'point') +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar') +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('TRAK') +
  ylab('Log DDD/1000 OBD') +
  ggtitle('Log DDD/1000 OBD by TRAK')


trakddd


trakdddprop <- ggplot(data.cc) +
  aes(x = l_DDDp1kOBD, y = Proportion, color = TRAK) +
  geom_point(size = 0.5) +
  #  facet_wrap(~TRAK, scales = 'free') +
  geom_smooth(method = 'lm', se = FALSE) +
  guides(color = FALSE) +
  xlab('Log DDD/1000 OBD') +
  ylab('Proportion') +
  ggtitle('Relation between log DDD and Proportion Split by TRAK')



### Class and TRAK



trakclassdata <- merge(data.cc[, .N, by = c('TRAK', 'Class_Gr')], 
                       data.cc[, .(NTrak = .N), by = 'TRAK']) %>%
  mutate(Fraction = N / NTrak)

trakclass.bar.gr <- ggplot(trakclassdata) +
  aes(x = TRAK, y = Fraction, fill = Class_Gr) +
  geom_bar(stat = 'summary', fun.y = 'sum', width = 0.8) +
  guides(fill = guide_legend(title = 'Class Group', 
                             title.theme = element_text(size = 8),
                             label.theme = element_text(size = 8),
                             keywidth = 0.8, 
                             keyheight = 0.8, 
                             nrow = 1)) +
  annotate(
    geom = 'text',
    x = trakclassdata$TRAK,
    y = 0.5,
    label = trakclassdata$NTrak, 
    size = 3
  ) +
  coord_flip()+
  xlab('TRAK') +
  ggtitle('Class Distribution by TRAK')


trakclass.bar.gr

TRAKPLOTS <- list(trakage = trakage, 
                  trakddd = trakddd, 
                  trakdddprop = trakdddprop, 
                  trakclass.bar.gr = trakclass.bar.gr)


trakgrid <- plot_grid(plotlist = TRAKPLOTS)



} else{
  paste('EXTRA PLOTS NOT RUN')
}

