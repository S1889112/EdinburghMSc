if (!exists('data.cc') | !exists('t.week')){
  source('Code_1_SETUP/1_DataLoad.R')
}

##============================================================================##
####                      VERY BASIC OVERVIEW                               ####
##============================================================================##


summary(data.cc)

## Found a 0 value in DDD

sum(data.cc$DDD.Total == 0)
sum(data.cc$DDDp1kOBD == 0)

## --- one each, is it same?
if (sum(data.cc$DDD.Total ==0) > 0){
  (data.cc[DDD.Total == 0] == data.cc[DDDp1kOBD == 0])
  
  ## alll true, so yes. We remove this. 
  
  data.cc <- data.cc[DDDp1kOBD != 0]
  print('run')
}

##============================================================================##
####                        UNIVARIATE                                      ####
##============================================================================##
histmaker <- function(dt, group, bins = NULL) {
  ## Average over scott, FD, Sturges for bins.
  if (is.null(bins)) {
    bins <- mean(nclass.scott(dt[[group]]),
                 nclass.FD(dt[[group]]),
                 nclass.Sturges(dt[[group]]))
    
  } else{
    bins <- bins
  }
  
  ## bandwith calculated with nrd0, with some extra smoothing for aesthetics
  bw <- 1.1 * density(dt[[group]])$bw
  
  
  ## Create combined histogram density plot.
  ## Do a
  ggplot(dt) +
    aes_string(x = group) +
    geom_histogram(aes(y = ..density..), bins = bins) +
    geom_density(alpha = 0.1, bw = bw) +
    ggtitle(paste('Histogram of ', group, sep = '')) +
    theme(plot.title = element_text(size = 12))
}




##============================================================================##
####                        UNIVARIATE                                      ####
##============================================================================##

### Week
weekplot <- ggplot(data = data.cc) +
  aes(x = Week, y = Proportion, group = 1) +
  stat_summary(fun.y = 'mean', geom = 'line') +
  stat_summary(fun.y = 'mean', geom = 'point') +
  stat_summary(fun.y = 'sd', geom = 'point', color = 'red') +
  geom_hline(yintercept = mean(data.cc[,Proportion])) +
  ggtitle('Mean Proportion by Week') +
  labs(x = 'Week', y = 'Mean Proportion') +
  coord_cartesian(ylim = c(0.2, 0.4)) +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 9))

weekplot

### Route
t.route <- mytabulate(data.cc, 'Route')  
t.route$SDProportion <- paste('SD = ', round(t.route$SDProportion, 2))


routeplot <- ggplot(t.route) +
  aes(x = Route, y = MeanProportion) +
  stat_summary(fun.y = 'identity', geom = 'bar') +
  geom_text(label = t.route[['N']], vjust = -0.5) +
  facet_wrap(~SDProportion, scales = 'free') +
  ylim(c(0, 0.4)) +
  ylab('Proportion') +
  ggtitle('Resistance Proportion by Route') +
  theme(plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10), 
        axis.text = element_text(size = 9))
  
routeplot

### TRAK
t.trak <- t.trak[order(MeanProportion)]


trakplot <- ggplot(data.cc) +
  aes(x = reorder(TRAK, Proportion), y = Proportion) +
  stat_summary(fun.y = 'mean', geom = 'point') +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', size = 0.3) +
  stat_summary(fun.y = 'sd', geom = 'point', color = 'red', size = 1) +
  annotate('text', x = t.trak$TRAK, y = 0, label = t.trak$N, size = 2.3) +
  xlab('TRAK') +
  ggtitle('Resistance Proportion and Standard Deviation by TRAK') +
  theme(axis.text.x = element_text(angle = 90, size = 9), 
        axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10))

trakplot

### CLASS

ypos <- ifelse(t.class$MeanProportion==0, 0.05, 
        ifelse(t.class$Class == 'BL - Monobacatam', 0.38, 0.01))

classplot <- ggplot(data.cc) +
  aes(x = reorder(Class, Proportion), y = Proportion) +
  stat_summary(fun.y = 'mean', geom = 'point') +
  stat_summary(fun.data = 'mean_se', 
               geom = 'errorbar', 
               size = 0.3) +
  stat_summary(fun.y = 'sd', geom = 'point', color = 'red', size = 1) +
  annotate('text', x = t.class$Class, y = ypos, label = t.class$N, size = 3) +
  xlab('Class') +
  ggtitle('Resistance Proportion and Standard Deviation by Class') +
  theme(axis.text.x = element_text(angle = 90, size = 9), 
        axis.text.y = element_text(size = 9),
        plot.title = element_text(size = 12), 
        axis.title = element_text(size = 10)) +
  expand_limits(y = c(0, NA))

classplot



#### Plot to grid. 




##-------------------------------CONTINUOUS-----------------------------------##


ageplot <- histmaker(data.cc, 'AvgAge') + 
  xlab('Average Age') +
  ggtitle(paste('Histogram of Average Age, sd = ', round(sd(data.cc$AvgAge),2)))

dddplot <- histmaker(data.cc, 'DDDp1kOBD') +
  xlab('DDD per 1000 OBD') +
  ggtitle(paste('Histogram of DDD per 1000 OBD, sd = ', round(sd(data.cc$DDDp1kOBD), 2)))



### Clearly some more peaked
prophist <- histmaker(data.cc, 'Proportion')

propbar <- ggplot(data.cc) + 
  aes(x = Proportion) + 
  geom_bar() +
  ggtitle('Frequencies for Individual Proportions') +
  theme(plot.title = element_text(size = 12), 
        axis.text = element_text(size = 10))












##============================================================================##
####                      INSTANT TRANSFORMATIONS                           ####
##============================================================================##

### Log DDD to reduce the skew
### center and scale age to help with convergence

data.cc[, `:=`(l_DDDp1kOBD = log(DDDp1kOBD), 
            AvgAge_sc = standardise(AvgAge))]



##-----------------------------GROUP THE DDD----------------------------------##

## BASED OFF N, NOT THE PROPORTION

ddgrouper <- function(x){
  ifelse(x < 30, '[0, 30)', 
         ifelse(x < 55, '[30, 55)', 
                ifelse(x < 90, '[55, 90)', 
                       ifelse(x < 180, '[90, 180)', '[180, \U221E)'))))
  
}

data.cc[, `:=`(DDDp1kOBD_Gr = ddgrouper(DDDp1kOBD))]

## now create an ordered factor


ord <- unique(data.cc[order(DDDp1kOBD), DDDp1kOBD_Gr])
data.cc[, `:=`(DDDp1kOBD_Gr = factor(DDDp1kOBD_Gr, 
                                     levels = ord, 
                                     ordered = TRUE))]


#### Recreate plot to show the N ####

dddgroupfreq <- ggplot(data.cc) +
  aes(x = DDDp1kOBD_Gr) +
  geom_bar() +
  geom_hline(yintercept = c(450, 500, 550)) +
  xlab('Grouped DDD/1000 OBD') +
  ylab('Count') +
  ggtitle('Grouped DDD/1000 OBD Counts') +
  scale_y_continuous(breaks = seq(0, 650, 50), labels = seq(0, 650, 50))
dddgroupfreq

#dddgroupfreq


##-------------------------------PROPORTION-----------------------------------##

propgrouper <- function(x) {
  
  ifelse(x == 0, '0',
  ifelse(x <= 0.25, '(0, 0.25]', 
  ifelse(x < 0.5, '(0.25, 0.5)', 
  ifelse(x == 0.5, '0.5', 
  ifelse(x < 0.75, '(0.5, 0.75)', 
  ifelse(x < 1, '(0.75, 1)', '1'))))))
}

data.cc[, `:=`(Proportion_Gr = propgrouper(Proportion))]

propord <- unique(data.cc[order(Proportion), Proportion_Gr])
data.cc[, `:=`(Proportion_Gr = factor(Proportion_Gr, 
                                      levels = propord, 
                                      ordered = TRUE))]




##============================================================================##
####                      RENEWED VISUALISATIONS                            ####
##============================================================================##

##----------------------------PROPORTION--------------------------------------##
propgroup <- ggplot(data.cc) +
  aes(x = Proportion_Gr) +
  geom_bar() +
  xlab('Proportion Grouping') +
  ylab('Count') +
  ggtitle('Proportion Group Frequency') +
  theme(axis.text.x = element_text(size = 10), 
        plot.title = element_text(size = 12))

propgroup




##----------------------------log DDD-----------------------------------------##

hist.ldd <- histmaker(data.cc, 'l_DDDp1kOBD', bins = 40) +
  xlab('log DDD per 1000 OBD')  +
  ggtitle(paste('Histogram of log DDD per 1000 OBD, sd = ', 
                round(sd(data.cc$l_DDDp1kOBD), 2)))


hist.agesc <- histmaker(data.cc, 'AvgAge_sc') +
  xlab('Average Age (standardised)') +
  ggtitle(paste('Histogram of Scaled Average Age, sd = ', 
                round(sd(data.cc$l_DDDp1kOBD))))




##============================================================================##
####                            SAVE PLOTS                                  ####
##============================================================================##
THEMEADJUST <- theme(plot.title = element_text(size = 20), 
                     axis.text = element_text(size = 15), 
                     axis.title = element_text(size = 18))

THEMEADJUST.PROP <- theme(axis.text.x = element_text(size = 17, angle = 45, hjust = 1))

PLOTS.factgrid <- plot_grid(weekplot, routeplot, trakplot, classplot)
PLOTS.contgrid <- plot_grid(ageplot, dddplot, hist.agesc, hist.ldd)
PLOTS.propgrid <- plot_grid(prophist + THEMEADJUST, 
                            propbar  + THEMEADJUST, 
                            propgroup + THEMEADJUST + THEMEADJUST.PROP, 
                            nrow = 1, 
                            align = 'h')



ggsave('Figures/2_1_EDA_Cont.png',      PLOTS.contgrid, width = 14, height = 7)
ggsave('Figures/2_1_EDA_Prop.png',      PLOTS.propgrid, width = 16, height = 8)
ggsave('Figures/2_1_EDA_DDD_Log.png',   hist.ldd,       width = 12, height = 8)
ggsave('Figures/2_1_EDA_Fact_Grid.png', PLOTS.factgrid, width = 12, height = 7)
ggsave('Figures/2_1_EDA_DDD_Group.png', dddgroupfreq,   width = 12, height = 8)










################################################################################
# REMOVE <- c('dddgroupfreq', 'ord', 'PLOTS.contgrid', 'PLOTS.propgrid', 
#             'classplot', 'ypos', 'PLOTS.fact.grid', 'weekplot', 'trakplot', 
#             'routeplot', 'prophist', 'propbar', 'dddplot', 'ageplot')

# rm(list = REMOVE)




#rm(list=ls())









