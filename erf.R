####### This file contains the functions for the adapted erf method

#'erf_fit: fits a quantile regression forest
#'@X_train: predictors training data
#'@Y_train: target variable training data
#'@q: intermediate probability level 
#'@nodeSize: minimum node size tuning parameter for GRF 
#'@trees: number of trees of the forest 
#'@return: list containing fitted GRF object, out-of-bag intermediate quantile predictions,
#'out-of-bag predictions similarity weights
erf_fit <- function(X_train, Y_train, q, nodeSize, trees){
  #fit GRF quantile forest to training data
  fit_q_grf = grf::quantile_forest(X_train, Y_train, 
                                   num.trees=trees,
                                   quantiles=q,
                                   min.node.size = nodeSize,
                                   honesty=TRUE)
  #predict Out-of-bag intermediate quantile
  pred_oob = as.matrix(predict(fit_q_grf)$predictions)
  
  #get out-of-bag weights
  weights_oob = as.matrix(grf::get_forest_weights(fit_q_grf))
  
  return(list('grf_fit'=fit_q_grf, 'pred_oob'=pred_oob, 'weights_oob'=weights_oob))
}

#'erf_pred: predicts extreme value index and conditional high quantile
#'@X_train: predictors training data
#'@Y_train: target variable training data
#'@X_test: predictors test data
#'@p: final probability level (scalar)
#'@q: intermediate probability level (scalar)
#'@fit: fitted quantile regression forest object 
#'(list containing fitted GRF object, out-of-bag intermediate quantile predictions,
#'out-of-bag predictions similarity weights)
#'@lambda: penalty parameter for penalized deviance  (scalar)
#'@return: list containing excess ratio (indices), predicted extreme value indices,
#'similarity weights for test observations, 
#'intermediate quantile predictions for test observations,
#'and maximum likelihood estimator for gamma
erf_pred <- function(X_train, Y_train,
                     X_test, p, q, fit, lambda, App){

  
  grf_fit = fit$grf_fit
  pred_oob = fit$pred_oob

  #define excess ratios based on out-of-bag intermediate quantile predictions
  xsRat = Y_train/pred_oob > 1 #TRUE for indices that exceed predicted quantile
  Z = Y_train[xsRat == TRUE]/pred_oob[xsRat == TRUE] #excess ratios
  
  
  #prediction weights for new test observations 
  weights = as.matrix(grf::get_forest_weights(grf_fit, newdata=X_test))
  #filter weights on training observations for which ratio > 1
  if(App==FALSE){
    weights = as.matrix(weights[, xsRat])
  }
  if(App==TRUE){
    weights = t(as.matrix(weights[,xsRat]))
  }
  
  #ML estimate for extreme value index based on excess ratios
  gamma_ml = mean(log(Z)) 
  
  #for every test observation prediction intermediate quantile
  pred_test = as.matrix(predict(grf_fit, newdata=X_test)$predictions)
  
  #for every test observation prediction extreme value index 
  pred_evi = as.matrix(apply(weights, MARGIN=1, 
                              FUN=optimizer, init=gamma_ml, Z=Z, q=q, lambda=lambda))
  #prediction conditional VaRp
  pred_varp = pred_test*((1-q)/(1-p))^pred_evi
  
  return(list('excess_rat'=Z, 'excess_rat_ind'=xsRat,
              'pred_evi'=pred_evi, 'weight_test'=weights,
              'pred_varq'=pred_test, 'ml_est'=gamma_ml, 'pred_varp'=pred_varp))
}


#'pen_dev: function that evaluates penalized deviance for a test observation
#'@gamma: extreme value index
#'@data: exceedances based on out-of-bag predictions (dataframe/matrices)
#'@weights: similarity weights for a test data observation based on exceedances (vector)
#'@prob: intermediate probability level q (scalar)
#'@lambda: penalty parameter (scalar)
#'@gammaprior: 'simple' model for gammma, i.e ML estimator (scalar)
#'@return: value for penalized deviance
pen_dev <- function(gamma, Z, weights, prob, lambda, gammaprior){
  #vector of contribution of each Zi
  dev_cont = log(gamma)+((1/gamma)+1)*log(Z) 
  
  #sum of vector containing weight_i * contribution Zi
  L = sum(weights*dev_cont) 
  res = (1/(1-prob))*L+lambda*(gamma-gammaprior)^2
  return(res)
}

#'optimizer: function that minimzes the penalized deviance for one test observation
#'@init: initial value for parameter to be optimized (scalar)
#'@weight_i: weight corresponding to validation observation i (vector)
#'@Z: exceedance ratios based of Out-of-Bag predictions  (dataframe/matrix)
#'@lambda: penalty parameter (scalar)
#'@gammaprior: 'simple' model for gammma, ML estimator for gamma in our case (scalar)
#'@return: optimal value for gamma for test observation i (SCALAR)
optimizer <- function(init, Z, weights_i, q, lambda){
  #minimize for gamma using a limited-memory modification of the BFGS quasi-Newton method
  res <- stats::optim(
    par=init,
    fn = pen_dev,
    Z=Z,
    weights=weights_i,
    prob=q,
    lambda=lambda,
    gammaprior=init,
    lower = 10e-6,
    upper = Inf,
    method = 'L-BFGS-B'
  )$par
  return(as.matrix(res))
}