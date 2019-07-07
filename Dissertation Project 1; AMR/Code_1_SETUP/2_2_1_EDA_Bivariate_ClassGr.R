if (!exists('hist.ldd')){
  source('Code_1_SETUP/2_1_EDA_Univariate_V2.R')
}



## Notation: Plots are stored in PLOTS.<indicator>

##============================================================================##
####                                GROUP CLASSES                           ####
##============================================================================##


classgrouper <- function(x){
  ## Grouping is based on the graph. 
  CLASSES <- t.class[order(MeanProportion)][['Class']]
  GroupA <- CLASSES[1:4]
  GroupB <- CLASSES[5:7]
  GroupC <- CLASSES[8:9]
  GroupD <- CLASSES[10:14]
  GroupE <- CLASSES[15]
  GroupF <- CLASSES[16]
  GroupG <- CLASSES[17:18]
  ifelse(x %in% GroupA, 'A', 
  ifelse(x %in% GroupB, 'B', 
  ifelse(x %in% GroupC, 'C', 
  ifelse(x %in% GroupD, 'D', 
  ifelse(x %in% GroupE, 'E', 
  ifelse(x %in% GroupF, 'F', 
  ifelse(x %in% GroupG, 'G','NA')))))))
}

data.cc[, `:=`(Class_Gr = factor(classgrouper(Class)))]



##============================================================================##
####                                  BIVARIATE                             ####
##============================================================================##


### Looks like no reason to include the route

#----------------------------------class and week------------------------------#

cw.plot <- ggplot(data.cc[Class_Gr != 'A', ]) +
  aes(x = as.numeric(Week), y = Proportion, color = Class_Gr) +
  stat_smooth(method = 'lm', formula = y ~ x , se = TRUE) +
  guides(color = guide_legend(title = 'Class Group', 
                              nrow = 1, 
                              title.theme = element_text(size = 14), 
                              label.theme = element_text(size = 14), 
                              keywidth = 1.2, 
                              keyheight = 1.2)) +
  ggtitle('Proportion Over Time by Class Group') +
  xlab('Week') +
  ylab('Mean Proportion') +
  scale_x_continuous(labels = seq(0, 12, 2), breaks = seq(0, 12, 2)) +
  theme(axis.text.x  = element_text(size = 14), 
        axis.text.y  = element_text(size = 14), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(size = 18))



#----------------------------------class and DDD-------------------------------#



### DDD Grouped ###

## Set up data for stacked bar. 



cdslope <- ggplot(data.cc[Class_Gr!='A',]) +
  aes(x = l_DDDp1kOBD, y = Proportion, color = Class_Gr) +
  stat_smooth(method = 'lm', formula = y~x, se = TRUE) +
  xlab('Log DDD/1000 OBD') +
  ggtitle('Log DDD Slope by Class Group') +
  guides(color = guide_legend(title = 'Class Group', 
                              nrow = 1, 
                              title.theme = element_text(size = 14), 
                              label.theme = element_text(size = 14), 
                              keywidth = 1.2, 
                              keyheight = 1.2)) +
  theme(axis.text.x  = element_text(size = 14), 
        axis.text.y  = element_text(size = 14), 
        axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(size = 18))





##============================================================================##
####                  STRUCTURE AND SAVE PLOTS                              ####
##============================================================================##


ggsave('Figures/2_2_EDA_ClassWeek.png',    plot = cw.plot,      width = 10, height = 6)
ggsave('Figures/2_2_EDA_ClassDDDSlope.png', plot = cdslope, width = 10, height = 6)



##============================================================================##
####                            CONVERT WEEK BACK TO NUMERIC                ####
##============================================================================##


data.cc[, `:=`(Week = as.numeric(Week))]





##============================================================================##
##                                                                            ##
##                                                                            ##
####                                EXTRA                                   ####
##                                                                            ##
##                                                                            ##
##============================================================================##
RUNEXTRA <- 0

if(RUNEXTRA==1){
  

###-----------------------JOINT RELATIONSHIP BETWEEN FACTORS-----------------###

#----------------------------------class and route-----------------------------#


## This plot is good, conveys all information! 
## x-axis shows if there is even the presence of invasiveness. 

cr.data <- data.cc[Class_Gr != 'A', 
                   .(N = .N, MP = mean(Proportion)), 
                   by = .(Class_Gr, Route)]

cr.data <- cr.data[order(Class_Gr, Route)]

cr.bars <- ggplot(cr.data) +
  aes(x = Class_Gr,y = MP, fill = Route) +
  geom_bar(stat = 'identity', 
           position ='dodge') +
  theme(strip.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 6), 
        axis.text.x = element_text(size = 6), 
        plot.title = element_text(size = 8)) +
  labs(x = 'Route', y = 'Mean Proportion', title = '') +
  annotate(geom = 'text', 
           x = seq(0.75, 6.5, by = 0.5), 
           y = cr.data$MP + 0.05, 
           label = cr.data$N) +
  expand_limits(y=c(0.05, NA)) +
  ggtitle('Resistance by Class and Route with Counts')



## NA Addition

# https://stackoverflow.com/questions/13106645/using-in-data-table-to-sum-the-values-of-two-columns-in-r-ignoring-nas

`%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}

cr.data <- cr.data %>%
  dcast(., 'Class_Gr ~ Route', value.var = c('MP', 'N')) %>%
  mutate(
    MP_dif = abs(MP_Invasive - `MP_Non-Invasive`),
    N_Total  = N_Invasive %+na% `N_Non-Invasive`
  ) %>%
  tidyr::replace_na(., list(MP_dif = 0))


adjs <- if_else(cr.data$MP_dif > 0, -0.6, 
                if_else(cr.data$MP_dif < 0, 1.4, -0.4))


cr.dif <- ggplot(cr.data) +
  aes(x = Class_Gr, y = MP_dif) +
  geom_bar(stat = 'identity', position = 'dodge') +
  geom_text(label = cr.data[['N_Total']], vjust = adjs, size = 3) +
  ggtitle(paste('Difference in Mean Proportion for Invasive vs', 
                'Non-Invasive by Class (Absolute value)')) +
  theme(plot.title = element_text(size = 8)) +
  xlab('Class Grouping') +
  ylab('Mean Proportion Difference (|Invasive - Non-Invasive|)') +
  coord_cartesian(ylim = c(0, 0.05))

cr.dif

PLOTS.CR <- list(cr.bars = cr.bars, 
                 cr.dif  = cr.dif)


#----------------------------------class and TRAK------------------------------#

PLOTLIST.CT <- list()

for (classname in sort(unique(data.cc$Class_Gr))){
  
  if (classname == 'A') {
    next
  }
  subdata <- data.cc[Class_Gr == classname]
  
  t.classtrak <- mytabulate(subdata, 'TRAK')
  t.classtrak <- t.classtrak[order(MeanProportion)]
  
  ypos.sub <- ifelse(t.classtrak$MeanProportion == 0, 0.05, 0)
  
  
  plt <- ggplot(subdata) +
    aes(x = reorder(TRAK, Proportion), y = Proportion) +
    stat_summary(fun.y = 'mean', geom = 'point', size = 0.9) +
    stat_summary(fun.data = 'mean_se', geom = 'errorbar', size = 0.3)  +
    annotate(geom = 'text', x = t.classtrak$TRAK, 
             y = ypos.sub, 
             label = t.classtrak$N, 
             size = 2) +
    xlab('TRAK') +
    ylab('Mean Proportion') +
    ggtitle(classname) +
    theme(axis.title = element_text(size = 7), 
          axis.text  = element_text(size = 6),
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(size = 7), 
          plot.margin = unit(c(0,0,0,0), 'cm'))
  PLOTLIST.CT[[classname]] <- plt
}

ctgrid <- plot_grid(plotlist = PLOTLIST.CT)

title <- ggdraw() + draw_label("Proportion by TRAK for each Class Group")

ct.plot <- plot_grid(title, ctgrid, ncol = 1, rel_heights = c(0.1, 1))


ct.plot


#-----------------------------------class and age------------------------------#

ca.pnt <- ggplot(data.cc) +
  aes(x = Class_Gr, y = AvgAge) +
  stat_summary(fun.y = mean, geom='point') +
  stat_summary(fun.data = mean_se, geom='errorbar', 
               fun.args = list(mult = 1)) +
  xlab('Class Grouping') +
  ylab('Average Age \u00B1 2 standard errors') +
  ggtitle('Average Age (\u00B1 2 standard errors) by Class') +
  theme(axis.text = element_text(size = 10), 
        plot.title = element_text(size = 12))

ca.pnt

## Some age differences by class, but generally a lot of overlap.  

ca.dens <- ggplot(data.cc) +
  aes(x = AvgAge, color = Class_Gr) +
  geom_density(size = 0.8) +
  xlab('Average Age') +
  guides(color=guide_legend(title="Class Group", 
                            label.theme = element_text(size = 8))) +
  ggtitle('Average Age Densities by Class')

ca.dens



PLOTS.CA <- list(ca.pnt  = ca.pnt, 
                 ca.dens = ca.dens)

## GROUPED CLASS ##
## Automatic relationship, this is how they were grouped. 

## GROUPED DDD ##

dddgroup <- ggplot(data.cc) +
  aes(x = DDDp1kOBD_Gr, y = Proportion, group = 1) +
  stat_summary(fun.y = 'mean', geom = 'point') +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', 
               fun.args = list(mult = 2)) +
  xlab('DDD Group') +
  ylab('Proportion (\u00B1 2 standard errors)') +
  ggtitle('Mean Proportion vs Grouped DDD')


dddgroup


dddclass <- ggplot(data.cc[Class_Gr != 'A']) +
  aes(x = as.numeric(DDDp1kOBD_Gr), y = Proportion, color = Class_Gr) +
  stat_summary(fun.y = 'mean', geom = 'point') +
  stat_smooth(method = 'lm', se = FALSE) +
  expand_limits(y = c(0.05, NA)) +
  guides(color = guide_legend(title = 'Class Group', 
                              label.theme = element_text(size = 8))) +
  xlab('DDD Group') +
  ggtitle('Proportion vs DDD Group by Class Group') +
  theme(strip.text.x = element_text(size = 9)) +
  expand_limits(x = c(1, 5))

dddclass


dddslopes <- ggplot(data.cc) +
  aes(x = l_DDDp1kOBD, y = Proportion, color = DDDp1kOBD_Gr) +
  stat_smooth(method = 'lm', formula = y ~ x, se = TRUE) +
  xlab('Log DDD/1000 OBD')  +
  guides(color=guide_legend(title="DDD Group", 
                            label.theme = element_text(size = 8), 
                            nrow = 1)) +
  ggtitle('Log DDD Slope by DDD Group')


#
#  facet_wrap(~DDDp1kOBD_Gr, scales = 'free') 

dddslopes


PLOTS.DD <- list(dddgroup = dddgroup, 
                 dddclass = dddclass, 
                 dddslopes = dddslopes)


### DDD Raw ###

cd.pnt <- ggplot(data.cc) +
  aes(x = Class_Gr, y = l_DDDp1kOBD) +
  stat_summary(fun.y = mean, geom='point') +
  stat_summary(fun.data = mean_se, geom='errorbar', 
               fun.args = list(mult = 2)) +
  xlab('Class Grouping') +
  ylab('Log DDD/1000 OBD \u00B1 2 standard errors') +
  ggtitle('Log DDD/1000 OBD) \u00B1 2 standard errors by Class )') +
  theme(axis.text = element_text(size = 10))



### can be used for stacked bar. 
dddgdata <- merge(data.cc[, .N, by = c('DDDp1kOBD_Gr', 'Class_Gr')], 
                  data.cc[, .(NClass = .N), by = 'Class_Gr']) %>%
  mutate(Fraction = N / NClass)






classdddprop <- ggplot(data.cc[Class_Gr != 'A']) +
  aes(x = l_DDDp1kOBD, y = Proportion, color = DDDp1kOBD_Gr) +
  stat_smooth(method = 'lm' ) +
  guides(color = guide_legend(title = 'DDD Group', 
                              label.theme = element_text(size = 8))) +
  xlab('Log DDD/1000 OBD') +
  facet_wrap(~Class_Gr, scales = 'free')  +
  ggtitle('Log DDD Slope by Class Group and DDD Group')

classdddprop


} else{
  
  paste("NO EXTRA PLOTS CREATED")
  
}

