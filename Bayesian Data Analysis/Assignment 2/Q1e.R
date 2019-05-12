sampler.1e.2 <- function(X, B, eps){
  # X: nxp matrix
  # B: pxS matrix
  # eps: 1xS matrix, each row same
  
  S  <- ncol(B)
  X  <- cbind(X, 1)
  Be <- rbind(B, eps)
  
  ## Linear combination and sampling
  lc   <- X%*%Be
  mu   <- exp(lc)
  samp <- t(apply(mu, 1, FUN=rpois, n = S))
  
  
  ## Summary statistics
  means <- apply(samp, 1, mean)
  lower <- apply(samp, 1, quantile, probs = 0.05)
  upper <- apply(samp, 1, quantile, probs = 0.95)
  
  ## Results reporting
  ret.mat <- cbind(means, lower, upper)
  colnames(ret.mat) <- c('mean', 'lower', 'upper')
  ret.df <- as.data.frame(ret.mat)
  
  
  return(ret.df)
}

#### PLOTTER.1E ####
plotter.1e <- function(ret.df, cols=c('blue', 'red', 'red'), main = '', cex = 1){
  
  upperlim <- max(ret.df$upper)
  ## BASE PLOT
  plot(x = year.raw, y = aud, ylim = c(0, upperlim), 
       main = main, xlab = 'Year', ylab = '',
       cex = cex, cex.main = cex, cex.axis = cex, cex.lab = cex)
  
  ## MEAN PLOT
  lines(x = year.raw, y =ret.df$mean,   col = cols[1], pch = 'x', cex = cex)
  lines(x = year.raw, y =ret.df$lower,  col = cols[2], lwd = 2, lty = 2)
  lines(x = year.raw, y =ret.df$upper,  col = cols[3], lwd = 2, lty = 2)
}






#### MODEL 1B ####

Xb <- cbind(1, gulls[, 'year'])
Bb <- t(as.matrix(combres.1b))
epsb <- 0

ret.dfb <- sampler.1e.2(X = Xb, B=Bb, eps = 0)


#### MODEL 1C ####
Sc <- nrow(combres.1c)


Xc  <- cbind(1, gulls[, 'year'])
Bc  <- t(as.matrix(combres.1c[, c('b0', 'b1')]))
eps <- rbind(rnorm(Sc, 0, sd = combres.1c[, 'sigma.epsilon']), NULL)

ret.dfc <- sampler.1e.2(Xc, Bc, eps)  


#### MODEL 1D ####

Sd <- nrow(combres.1d)

Xd   <- as.matrix(cbind(1, gulls[, c('year', 'yellowlegged')]))
Bd   <- t(as.matrix(combres.1d[, c('b0', 'b1', 'b2')]))
epsd <- rbind(rnorm(Sd, 0, sd = combres.1d[, 'sigma.epsilon']), NULL)

ret.dfd <- sampler.1e.2(Xd, Bd, epsd)  

#### PLOTS ####

cex <- 2

png('Q1/Q1eFinalPlots.png', width = 1200, height = 500)
par(mfrow = c(1,3), 
    mar = c(5, 4,4,2)+ 1)
plotter.1e(ret.df = ret.dfb, main = 'Audouin ~ Year', cex = cex)
title(ylab = 'Audouin Abundance', cex.lab = 1.7)
plotter.1e(ret.df = ret.dfc, main = 'Audouin ~  Year + variance', cex = cex)
plotter.1e(ret.df = ret.dfd, main = 'Audouin ~ Year + Yellowlegged + variance', cex = cex)
par(mfrow=c(1,1))
dev.off()




#### REPORT FIGURES ####

# Capitalise first letter of a string
capitalise <- function(x) {paste(toupper(substr(x,1,1)), substr(x,2,nchar(x)),sep='')}

df1 <- cbind.data.frame(ret.dfb, ret.dfc, ret.dfd)

model.ind <- paste(c(rep('1b', 3), rep('1c', 3), rep('1d', 3)))

df1.names <- colnames(df1)
df1.names <- paste(df1.names, model.ind)
colnames(df1) <- capitalise(df1.names)
df1 <- as.data.frame(round(as.matrix(df1), 0))
rownames(df1) <- year.raw

png('Q1/Q1eResults.png', width = 650, height = 600)
grid.table(df1)
dev.off()






