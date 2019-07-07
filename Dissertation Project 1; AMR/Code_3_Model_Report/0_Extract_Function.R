mod <- mc.1.post
df <- data.frame(mod)

## names
getnames <- function(df, pattern) {
  varnames <- parnames(df)
  return(varnames[grep(pattern, varnames)])
}

prob.mt0 <- function(x) {mean(x>0)}

## posterior samples
getprobs <- function(df, pattern){
  ## Get relevant column names
  varnames <- getnames(df, pattern)
  
  ## reduced df
  df <- df[, varnames]
  
  ## Get probability > 0
  probs <- apply(df, 2, prob.mt0)
  probs
}
