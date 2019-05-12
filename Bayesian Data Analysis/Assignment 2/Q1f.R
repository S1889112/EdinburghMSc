#### DIC ####

# No need to update, already burned in. 

## 16m


start <- Sys.time()
dic.1b <-dic.samples(model = m1.b, n.iter = 20000, type='pD')
dic.1c <- dic.samples(model = m1.c, n.iter = 1000000, type='pD')
dic.1d <- dic.samples(model = m1.d, n.iter = 1400000, type='pD')
end <- Sys.time()
runtime <- end-start
runtime


c1 <- sum(dic.1b$deviance) + sum(dic.1b$penalty)
c2 <- sum(dic.1c$deviance) + sum(dic.1c$penalty)
c3 <- sum(dic.1d$deviance) + sum(dic.1d$penalty)

dic.mat <- cbind(c1, c2, c3)
colnames(dic.mat) <- c('Model 1B', 'Model 1C', 'Model 1D')
rownames(dic.mat) <- 'DIC'
dic.mat <- round(dic.mat, 2)
dic.df <- as.data.frame(dic.mat)

png('Q1/Q1fDIC.png', width = 250, height = 50)
grid.table(dic.df)
dev.off()



