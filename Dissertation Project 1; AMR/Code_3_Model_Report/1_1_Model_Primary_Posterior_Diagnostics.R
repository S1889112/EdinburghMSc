##============================================================================##
####                            LOAD REQUISITE DATA                         ####
##============================================================================##

if (!exists('routeddd')) {
  source('Code_1_SETUP/2_5_1_EDA_Route.R')
}

##============================================================================##
####                            LOAD MODEL                                  ####
##============================================================================##

mp.1.post <- readRDS('Models/MP_1_Post.rds')

##============================================================================##
####                            DIAGNOSTICS                                 ####
##============================================================================##

color_scheme_set('viridis')

mp1.rhatplot <- mcmc_rhat_hist(rhat(mp.1.post), binwidth = 0.0005) +
  theme(axis.text.x = element_text(size = 12))
mp1.neffrat  <- mcmc_neff(neff_ratio(mp.1.post)) + 
  scale_x_continuous(breaks = seq(0, 1, 0.05), 
                     labels = seq(0, 1, 0.05)) +
  theme(axis.text.x = element_text(size = 12))


color_scheme_set('viridis')

mp1.traceplots <- mcmc_trace(mp.1.post,
                             regex_pars = c('b_', 'sd_', 'r_'),) +
  theme(strip.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8))


##============================================================================##
####                            SAVE PLOTS                                  ####
##============================================================================##

mp1.grid <- plot_grid(plot_grid(mp1.rhatplot, mp1.neffrat, nrow = 1),
                      mp1.traceplots,
                      nrow = 2)



ggsave('Figures/3_1_Primary_Convergence.png',
       plot = mp1.grid,
       width = 15, 
       height = 10)

