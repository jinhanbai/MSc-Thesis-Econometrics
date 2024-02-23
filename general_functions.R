# Calculates Integrated Squared Error (ISE) based on true quantiles based on prob and df
# @preds: vector of quantile predictions
calcISE <- function(preds, prob, df, sigma){
  preds = as.numeric(preds)
  trueQ = as.numeric(ggdist::qstudent_t(p=prob, df=df, mu=0, sigma=sigma))
  return(mean((preds-trueQ)^2))
}


calcISRE <- function(true, preds){
  true = as.numeric(true)
  preds = as.numeric(preds)
  sre = ((true/preds)-1)^2
  return(mean(sre))
}
