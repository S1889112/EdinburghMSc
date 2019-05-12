#### DEMEAN ####

demean <- function(x) {x - mean(x)}




#### DENSITY PLOTTER ####


mcmc.dens <- function(combined.results) {
  #
  # combined results is of class "mcmc". 
  # Output you would get from mcmc.combine(coda.samples(...))
  #
  
  # Dataframe conversion
  mcmc.df <- as.data.frame(combined.results)
  
  # Parameter names
  param.names <- colnames(mcmc.df)
  
  for (name in param.names){
    # Parameter vector
    param.samples <- mcmc.df[, name]
    main = paste("Density of", name)
    plot(density(param.samples), main = main)
  }
}


#### THINNING CHECKER ####

thincheck <- function(results.obj, var.idx, thinhigh, thinby, chainsize){
  # var.idx: Specify variable to observe ESS for
  # chainsize: Size of each individual chain in results.obj
  # 
  # 
  # 
  # 
  # 
  
  # Thinning intervals to consider, slice to remove 0
  thins <- seq(from = 0, to = thinhigh, by = thinby)
  thins <- thins[2:length(thins)]
  
  # Vector to store effective sample sizes corresponding to
  # various thinning intervals
  ess   <- numeric(length(thins))
  
  # i: Index of vector add to
  i     <- 1
  
  for (thin in thins) {
    # Create indices to take from posterior sample
    thin.vec <- seq(from=1, to=chainsize, by = thin)
    # Get thinned sample by slicing posterior sample using
    # above indices
    tempsamp <- results.obj[[1]][, var.idx][thin.vec]
    # Multiply by 3 to get ESS of combined chain
    ess[i]   <- 3*effectiveSize(tempsamp)
    i = i + 1
  }
  plot(thins, ess)
}


#### RESULTS TABLE ####

results.table <- function(combres, dig = 3){
  sum.res <- summary(combres)
  quants.res <- sum.res$quantiles
  stats.res  <- sum.res$statistics
  rep.res    <- cbind(quants.res, stats.res)
  rep.res    <- round(rep.res, dig)
  
  CI.95      <- paste('(', rep.res[, '2.5%'], ',', rep.res[, '97.5%'], ')', sep='')
  CI.95.width <- round(rep.res[, '97.5%'] - rep.res[, '2.5%'], dig)
  
  rep.res <- cbind(rep.res, CI.95)
  colnames(rep.res) <- replace(colnames(rep.res), length(colnames(rep.res)), '95% CI')    
  
  rep.res <- cbind(rep.res, CI.95.width)
  colnames(rep.res) <- replace(colnames(rep.res), length(colnames(rep.res)), '95% CI Width')
  return(rep.res)
}


##### QUESTION 2 EXCLUSIVE #####

#### BARPLOTS FOR EDA ####

barplot.2 <- function(height.name, xlab, col = c('Green', 'Red'), srt = 90){
  ### Create barplot table
  t.height    <- table(cows$parasite, cows[, height.name])
  t.height.pc <- prop.table(t.height, margin = 2)
  t.height.pc <- round(t.height.pc, 2)
  # Get sample size to put at top of barplot
  sample.size <- apply(t.height, MARGIN = 2, sum)
  
  ### Create barplot ###
  
  main <- paste('Parasite Proportion by', Hmisc::capitalize(xlab))

  mybar <- barplot(t.height.pc, main = main, 
                   xlab = xlab, col = col, 
                   ylim = c(0, 1.2), las = 2)
  
  ### Label with sample size
  text(mybar, y = 1.1, label = paste('n=', sample.size, sep = ''), srt = srt)
  
}

#### Mann-Whitney U-statistic with boxplot ####

boxplot.mw <- function(df, y.name, var.names){
  # y must be categorical, with 0 and 1
  
  ## Split dataset into 2 for readability
  df.0 <- df[df[, y.name] == 0, ]
  df.1 <- df[df[, y.name] == 1, ]
  
  for (i in 1:length(var.names)) {
    
    var.name <- var.names[i]
    ## Different vectors
    zero.vec <- df.0[, var.name]
    one.vec  <- df.1[, var.name]
    ## Mann-Whitney
    MW.p <- wilcox.test(zero.vec, one.vec)$p.value
    MW.p <- round(MW.p, 3)
    
    ## Boxplot
    main    <- paste(var.name, '~', y.name)
    sub     <- paste('Mann-Whitney p-value for median difference:', MW.p)
    xlab    <- Hmisc::capitalize(y.name)
    formula <- as.formula(main)
    boxplot(formula = formula, data = df, xlab = xlab, main = main, sub = sub)
  }
}






#### FUNCTIONS FOR QUESTION 2D AND E ####

### EXTRACT POSTERIOR SAMPLES ###

farmprobs <- function(xt, Bt, alpha){
  # xt: row of data of class matrix: Dimensions 1 x k (k parameters)
  # Bt: Matrix of coefficient samples: Dimensions k x S (S MCMC samples)
  # alpha: row of samples of farm-specific intercept, class matrix: Dimensions 1 x S

  # Add 1 on for random intercept influence to Xt, and add alpha to Bt. 
  xt.a <- cbind(xt, 1)
  Bt.a <- rbind(Bt, alpha)

  ## Linear combination
  lc.farm <- xt.a%*%Bt.a

  ## Probability, plogis is the inverse logit function
  pr.farm <- plogis(lc.farm)
  
  ## Return probability vector
  return(pr.farm)
}


### SUMMARISE POSTERIOR SAMPLES ###

results.post  <- function(farmID, probvec, digits=2) {
  ### Get mean, CI, probability of epidemic
  mean.farm   <- mean(probvec)
  ci.farm     <- as.vector(quantile(probvec, probs = c(0.02,0.975)))
  pr.epi.farm <- mean(probvec > 0.2)
  
  # Get confidence interval as a string
  ci.farm.string <- paste('(', round(ci.farm[1], digits=digits), ', ', 
                          round(ci.farm[2], digits=digits), ')', sep='')
  
  # Concatenate all results
  res <- round(cbind(ci.farm[1], ci.farm[2], pr.epi.farm, mean.farm), digits=digits)
  res <- cbind(res, ci.farm.string)
  colnames(res) <- c('2.5%', '97.5%', 'P(Epidemic)', 'Mean', '95% CI')
  rownames(res) <- paste('Farm', farmID)
  return(res)
}


