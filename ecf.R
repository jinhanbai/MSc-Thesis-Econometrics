#'ecf_reg_fit: fits regression forest for estimating conditional extreme value index
#'@X_train: predictors training data
#'@Y_train: target variable training data
#'@u: intermediate threshold 
#'@nodeSize1: minimum node size tuning parameter for regression forest 
#'@trees1: number of trees for regression forest 
#'@return: list containing fitted forest and excess ratios 
ecf_reg_fit <- function(X_train, Y_train, u, nodeSize1, trees1){

  u = ifelse(u==0, 1e-6, u)#in case u==0, set u=1e-6 to avoid divsion by 0

  #define excess ratios above threshold u
  xsRat = Y_train/u > 1 #TRUE for indices that exceed u
  Z = Y_train[xsRat]/u #excess ratios
  logZ = log(Z) #log of excess ratios
  
  #fit regression forest estimating conditional mean of log exceedance ratios
  fit_reg = randomForest::randomForest(X_train[xsRat==TRUE, ], logZ,
                                       ntree=trees1,
                                       nodsize=nodeSize1)
  return(list('fit_reg'=fit_reg, 'excess_rat'=Z, 'excess_rat_ind'=xsRat))
}

#'ecf_class_fit: fits classification forest and calibrates probabilities
#'@X_train: predictors training data
#'@Y_train: target variable training data
#'@u: intermediate threshold
#'@nodeSize2: minimum node size tuning parameter for classification forest 
#'@trees2: number of trees for classification forest 
#'@return: list containing fitted forest, out-of-bag classification scores,
#'maximum likelihood betas, and out-of-bag calibrated probabilities
ecf_class_fit <- function(X_train, Y_train, u, nodeSize2, trees2){
  
  xsRat = Y_train/u > 1 #TRUE for indices that exceed u
  Z = Y_train[xsRat]/u #excess ratios
  V = as.factor(xsRat)
  
  #fit classification model
  fit_class = randomForest::randomForest(X_train, V,
                                         ntree=trees2,
                                         nodesize=nodeSize2)
  #classification scores for training data
  scores = predict(fit_class, type='prob')[,2] 
  #browser()
  #extract ML parameters for probability calibration
  betas_init = c(0,0) #initialize beta0=0 and beta1=0
  betas_ml = min_ll(betas_init, V, scores)
  
  #out-of-bag predictions for calibrated probability
  pred_g = prob(betas_ml, scores) ##redundant
  
  return(list('fit_class'=fit_class, 'scores_train'=scores, 
              'betas_ml'=betas_ml, 'pred_g'=pred_g))
}

#'ecf_pred: predicts extreme value index, g and finds conditional high quantile
#'and correspondingly the VaRp.
#'@X_test: predictors test data
#'@p: final probability level
#'@u: intermediate threshold 
#'@ecf_reg: list containing fitted forest and excess ratios 
#'@ecf_class: list containing fitted forest, out-of-bag classification scores,
#'maximum likelihood betas, and out-of-bag calibrated probabilities
#'@return: list containing predicted extreme value indices, classification scores,
#'g and conditional VaRp

ecf_pred <- function(X_test, p, u, ecf_reg, ecf_class){
  
  u = ifelse(u==0, 1e-6, u) #in case u==0, set u=1e-6 to avoid division by 0
  ecf_reg_fit = ecf_reg$fit_reg
  ecf_class_fit = ecf_class$fit_class
  betas_opt = ecf_class$betas_ml
  
  
  #predict extreme value indices on new test data
  pred_evi_new = predict(ecf_reg_fit, newdata=X_test)
  
  #predict classification scores on new testdata
  scores_new = predict(ecf_class_fit, newdata=X_test, type='prob')[,2]
  
  #calibrate probabilities
  g = prob(betas_opt, scores_new)
  
  #predict conditional VaRp
  pred_varp = u*(g/(1-p))^pred_evi_new
  
  return(list('pred_evi'=pred_evi_new, 'scores'=scores_new,
              'g'=g, 'pred_varp'=pred_varp))
}

#'prob: evaluates the sigmoid function for P(V=1|X=x)
#'@betas: regression parameters 
#'@fx: classification scores 
#'@return: values vector for P(V=0|X=x)
prob <- function(betas, fx){
  res = 1/(1+exp(betas[1]*fx+betas[2]))
  res = ifelse(res==1, 0.999999, res)
  res = ifelse(res==0, 0.000001, res)
  return(res)
}

#'neg_ll: evaluates negative loglikelihood for probability calibration
#'@betas: regression parameters 
#'@V: binary variable, 1 if exceedance and 0 if not 
#'@fx: classification scores 
#'@return: value for P(V=0|X=x)
neg_ll <- function(betas, V, fx){
  p = prob(betas, fx)
  return(-sum(V*log(p)+(1-V)*log(1-p)))
}
#'min_ll: minimizes negative log-likelood for beta0 and beta1.
#'Note this is done on the training set
#'@beta_init: beta initial values 
#'@V: indicator for exceedances 
#'@fx: classification scores 
#'@return: ML estimates for betas based on numerical optimization 
min_ll <- function(betas_init, V, fx){
  #initialization
  betas_init = betas_init
  V = as.integer(V)-1 #subtract 1 to make binary vector
  fx = fx
  res <- stats::optim(
    par=betas_init,
    fn = neg_ll,
    V=V,
    fx=fx,
#    lower = -10e6,
#    upper = 10e6,
    method = 'Nelder-Mead'
  )$par
  names(res) = c('beta0','beta1')
  return(res)
} 
