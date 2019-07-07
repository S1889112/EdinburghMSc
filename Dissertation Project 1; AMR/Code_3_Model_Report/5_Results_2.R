if (!exists('areas.ddd')){
  source('Code_3_Model_Report/5_Results_1.R')
}

##============================================================================##
####                            NAME EFFECTS                                ####
##============================================================================##
rnames.present <- gsub('.Intercept.','',rnames) 
rnames.present <- gsub('r_Name.','',rnames.present)
rnames.present <- gsub('\\.', '/', rnames.present)


intervals.names <- mcmc_intervals(mc41.df[, rev(rnames)], prob = 0.5, prob_outer = 0.9) +
  scale_y_discrete(name = 'Drug Name', 
                   breaks = rev(rnames), 
                   labels = rev(rnames.present)) +
  ggtitle('50% and 90% Intervals for Varying Intercept on Drug') +
  geom_vline(xintercept = c(-3, 3))



ggsave('Figures/5_1_Intervals_Name.png', 
       plot = intervals.names, 
       width = 10, height = 10)




