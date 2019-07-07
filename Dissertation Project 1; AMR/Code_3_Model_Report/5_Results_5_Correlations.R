if (!exists('keystats')){
  source('Code_3_Model_Report/5_Results_1.R')
  
}

mc32 <- readRDS('Models/MC_3_2_Post.rds')



mc32.df <- data.frame(mc32)
mc32.names <- colnames(mc32.df)
mc32.cornames <- mc32.names[grep('cor', mc32.names)]


mc32.df <- mc32.df[,mc32.cornames] 
mc32.df <- as.matrix(mc32.df)

resmat <- t(apply(mc32.df, 2, keystats))
resmat <- 
  
  
cbind(resmat, resmat[,'97.5%'] - resmat[,'2.5%'])



