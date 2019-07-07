if (!exists('areas.ddd')){
  source('Code_3_Model_Report/5_Results_1.R')
}


##============================================================================##
####                            CLASS GROUP HETERO                          ####
##============================================================================##
disctosd <- function(x){1/exp(x)}


discdf <- as.matrix(mc41.df[, betanames.disc])
sddf   <- apply(discdf, 2, disctosd)
sdtab <- round(t(apply(sddf, 2, keystats))[,c(1:2, 3, 6)], 2)

sdnames.disc <- c("$\\sigma_B$",
                  "$\\sigma_C$",
                  "$\\sigma_D$",
                  "$\\sigma_E$",
                  "$\\sigma_F$",
                  "$\\sigma_G$")

rownames(sdtab) <- sdnames.disc
print(xtable(sdtab, 
             caption = 'Class Group Heterogeneity', 
             label = 'tab::5_Heterogeneity', 
             align = rep('l', ncol(sdtab) + 1)),
      sanitize.rownames.function =  identity)





