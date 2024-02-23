#' emf_fit: fits quantile forest and subsequently regression forest
#' @X_train: predictors training data
#' @Y_train: target variable training data
#' @q: intermediate probability level 
#' @nodeSize1: minimum node size used for quantile regression forest 
#' @nodeSize2: minimum node size used for extreme value index regression forest 
#' @tree1: number of trees for quantile regression forest 
#' @tree2: number of trees for regression forest
#' @return: list containing fitted forests and excess ratios 
emf_fit <- function(X_train, Y_train, q, nodeSize1, nodeSize2, tree1, tree2){
  
  #fit quantile forest
  fit_q_grf = quantile_forest(X_train, Y_train, 
                              num.trees=tree1,
                              quantiles=q,
                              min.node.size = nodeSize1,
                              honesty=TRUE)
  
  ##predict on out of bag training samples and define exceedances for EVI estimation
  pred_q_grf = predict(fit_q_grf)$predictions
  
  #define excess ratios based on out-of-bag predictions 
  xsRat = Y_train/pred_q_grf > 1 #TRUE for indices that exceed predicted quantile
  Z = Y_train[xsRat == TRUE]/pred_q_grf[xsRat == TRUE] #excess ratios
  logZ = log(Z)
  
  #fit regression forest estimating conditional mean of log exceedance ratios
  fit_reg_grf = regression_forest(X_train[xsRat==TRUE, ], logZ,
                                  num.trees=tree2,
                                  min.node.size=nodeSize2,
                                  honesty=TRUE)
  
  return(list('fit_qrf'=fit_q_grf, 'fit_reg'=fit_reg_grf, 'excess_rat'=Z,
              'excess_rat_ind'=xsRat))
}

#'emf_pred: function to predict intermediate VaRq, extreme value index
#'and finally VaRp on the new data
#'@X_test: predictors test data
#'@p: high probability level 
#'@q: intermediate probability level 
#'@fit: list containing fitted forests and excess ratios 
#'@return: list containing predictions for VaRq, VaRp and extreme value index
emf_pred <- function(X_test, p, q, fit){
  
  fit_qrf = fit$fit_qrf
  fit_reg = fit$fit_reg
  
  #for every validation observation prediction intermediate quantile
  test_pred_qrf = as.matrix(predict(fit_qrf, newdata = X_test)$predictions)
  
  #for every validation observation prediction extreme value index 
  test_pred_reg = as.matrix(predict(fit_reg, newdata=X_test)$predictions)
  
  #prediction conditional VaRp
  pred_varp = test_pred_qrf*((1-q)/(1-p))^test_pred_reg
  
  return(list('pred_varq'=test_pred_qrf, 'pred_varp'=pred_varp, 
              'pred_evi'=test_pred_reg))
}

