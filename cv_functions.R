#'cv_methods: function to find optimal hyperparameters all methods
#'@data: dataframe containing training data (data.frame)
#'@q: intermediate probability level (scalar)
#'@pars_k1: parameters to be tried for minimum nodesize quantile forest, default ={20,80, 200} (vector)
#'@pars_k2: parameters to be tried for minimum nodesize regression forest, default = {10, 20,40} (vector)
#'@lambda: parameters to be tried for penalty parameter ERF, default = {0, 0.01, 0.001} (vector)
#'@pars_k_reg_ecf: parameters to be tried for ecf regression forest, default={10, 20, 40} (vector)
#'@pars_k_class_ecf: parameters to be tried for ecf classification forest, default={20,80,200} (vector)
#'@trees: number of trees of
#'@K: number of folds (scalar)
#'@R: number of repetitions (scalar)
#'@return: list containing tuning results and best set of parameters
cv_methods <- function(data, 
                       q, 
                       pars_k1=c(20,80,200), 
                       pars_k2=c(10,20,40),
                       lambda=c(0,0.01,0.001),
                       pars_k_reg_ecf=c(10, 20, 40),
                       pars_k_class_ecf=c(20, 80, 200),
                       trees=100,
                       K=5,
                       R=5){
  
  #find all combinations of parameters for two methods
  res_emf = expand.grid('nodeSize1'=pars_k1, 'nodeSize2'=pars_k2 )
  res_erf = expand.grid('nodeSize'=pars_k1, 'lambda'=lambda)
  
  res_ecf_reg = data.frame('nodeSize_reg' = pars_k_reg_ecf)
  res_ecf_class = data.frame('nodeSize_class'=pars_k_class_ecf)
  
  res_emf$cv = rep(0, times=nrow(res_emf))
  res_erf$cv = rep(0, times=nrow(res_erf))
  res_ecf_reg$cv = rep(0, times=nrow(res_ecf_reg))
  res_ecf_class$cv = rep(0, times=nrow(res_ecf_class))
  
  for(rep in 1:R){
    #for every rep create a new 5 fold partition
    datalist = cv_dataSplit(data, K) #random partition into 5 folds
    
    #for every fold
    for(split in 1:K){
      dat = datalist[[split]]
      X1 = data.frame(dat$X1)
      Y1 = dat$Y1

      X2 = data.frame(dat$X2)
      Y2 = dat$Y2

      
      ###############tuning procedure for proposed and adapted ERF#############
      for(k1 in 1:length(pars_k1)){

        #fit GRF quantile forest and obtain intermediate quantile estimates
        fit_quantile = grf::quantile_forest(X1, Y1,
                                            quantiles=q,
                                            num.trees = trees,
                                            min.node.size = pars_k1[k1],
                                            honesty = TRUE)
        #oob estimates intermediate quantile (training set)
        pred_grf_train = as.matrix(predict(fit_quantile)$predictions)
        
        #intermediate quantile estimates on validation set
        pred_grf_val = as.matrix(predict(fit_quantile, newdata=X2)$predictions)
        
        #excess ratios based on oob estimates
        xsRat = Y1/pred_grf_train > 1
        Z = Y1[xsRat==TRUE]/pred_grf_train[xsRat==TRUE] #excess ratios
        logZ = log(Z)
        
        #excess ratios for validation set
        xsRat_val = Y2/pred_grf_val > 1 #TRUE for indices that exceed predicted quantile
        Z_hat = Y2[xsRat_val==TRUE]/pred_grf_val[xsRat_val==TRUE] #excess ratios
        
        #weights for validation set
        weights = as.matrix(grf::get_forest_weights(fit_quantile, newdata=X2))
        weights = weights[, xsRat] #weights corresponding to Z in training data
        #Ml estimator for gamma
        gamma_ml = mean(logZ)
        
        #tune regression forest for proposed method
        for(k2 in 1:length(pars_k2)){

          #fit regression forest estimating conditional mean of log exceedance ratios
          fit_reg_grf = grf::regression_forest(X1[xsRat==TRUE, ], logZ,
                                          num.trees=trees,
                                          min.node.size=pars_k2[k2],
                                          honesty=TRUE)
          
          #predict gamma on validation set
          gamma_hat = as.matrix(predict(fit_reg_grf, newdata=X2)$predictions)
          gamma_hat = gamma_hat[xsRat_val==TRUE]
          #evaluate contribution to total cv score
          row = (res_emf$nodeSize1 == pars_k1[k1]) & (res_emf$nodeSize2 == pars_k2[k2])
          res_emf$cv[row] = res_emf$cv[row] + cv_err_cont(gamma_hat, Z_hat)
        }
        #tune lambda for ERF-predict
        for(l in 1:length(lambda)){
          
          #predict gamma on validation set 
          gamma_hat = as.matrix(apply(weights, MARGIN=1, 
                                     FUN=optimizer, init=gamma_ml, Z=Z, q=q, lambda=lambda[l]))
          gamma_hat = gamma_hat[xsRat_val==TRUE] #filter on exceedances in validation set
          #evaluate contribution to total cv score
          row = (res_erf$nodeSize==pars_k1[k1]) & (res_erf$lambda==lambda[l])
          res_erf$cv[row] = res_erf$cv[row] + cv_err_cont(gamma_hat, Z_hat)
        }
      }
      
      ###############tuning procedure for ecf method#########################
      u = sort(Y1, decreasing = FALSE)[ceiling(q*length(Y1))] #fix threshold
      #excesses training set
      xsRat_train = Y1/u > 1 
      Z_train = Y1[xsRat_train==TRUE]/u
      #excesses validation set
      xsRat_val = Y2/u > 1 
      Z_val = Y2[xsRat_val==TRUE]/u
      class_val = as.integer(xsRat_val)
      #tuning for ecf regression forest
      for(k1 in 1:length(pars_k_reg_ecf)){
        reg = ecf_reg_fit(X1, Y1, u, pars_k_reg_ecf[k1], trees)
        fit_reg = reg$fit_reg
        
        #predict gamma on validation set
        gamma_hat = as.matrix(predict(fit_reg, newdata=X2))
        gamma_hat = gamma_hat[xsRat_val==TRUE]
        
        #evaluate contribution to total cv score
        row = (res_ecf_reg$nodeSize == pars_k_reg_ecf[k1]) 
        res_ecf_reg$cv[row] = res_ecf_reg$cv[row] + cv_err_cont(gamma_hat, Z_val)
      
      }
      
      #tuning for classification forest
      for(k2 in 1:length(pars_k_class_ecf)){
        class = ecf_class_fit(X1, Y1, u, pars_k_class_ecf[k2], trees)
        fit_class = class$fit_class
        betas_ml = class$betas_ml
        
        #predict classification scores on validation set
        scores_val = predict(fit_class, newdata=X2, type='prob')[,2]
        #calibrated predicted probs on validation set
        prob_val = prob(betas_ml, scores_val)
        
        #evaluate contribution to total cv score
        row = (res_ecf_class$nodeSize == pars_k_class_ecf[k2]) 
        res_ecf_class$cv[row] = res_ecf_class$cv[row] + cv_brier(prob_val, class_val)
      }
    }
  }
  res_emf$cv = (res_emf$cv)/R #divide by R
  res_erf$cv = (res_erf$cv)/R
  res_ecf_reg$cv = (res_ecf_reg$cv)/R
  res_ecf_class$cv = (res_ecf_class$cv)/R
  
  best_emf = res_emf[which.min(res_emf$cv),]
  best_erf = res_erf[which.min(res_erf$cv), ]
  best_ecf_reg = res_ecf_reg[which.min(res_ecf_reg$cv),]
  best_ecf_class = res_ecf_class[which.min(res_ecf_class$cv),]
  
  return(list('EMF'=res_emf, 'ERF'=res_erf, 'ecfReg'=res_ecf_reg,
              'ecfClass'=res_ecf_class,'bestemf'=best_emf,
              'bestERF'=best_erf, 'bestecfReg'=best_ecf_reg,
              'bestecfClass'=best_ecf_class))
}


#'cv_dataSplit: splits data into K folds repeated R times
#'@data: dataframe containing (X,Y) (data.frame)
#'@K: number of folds (scalar)
#'@return: list containing datalists where each datalist is a list containing: 
#'X1, Y1, X2, Y2 (matrices/data.frames)
cv_dataSplit <- function(data, K){
  #initialization
  K = K
  n = nrow(data)
  train_frac = 1-1/K
  res = list()
  #get subsets of indices
  subsets = cvTools::cvFolds(n, K=K, R=K, type='random')$subsets
  for(i in 1:K){
    shuf = data[subsets[,i], ]
    Y = shuf$Y
    X = shuf[!(colnames(shuf) %in% 'Y')]
    X1 = X[1:(train_frac*n), ]
    Y1 = Y[1:(train_frac*n)]
    X_test = X[(train_frac*n+1):n,]
    Y_test = Y[(train_frac*n+1):n]
    t = list('X1'= X1, 'Y1'=Y1,
             'X2'=X_test, 'Y2'=Y_test)
    res = append(res, list(t))
  }
  return(res)
}

#'cv_err_cont: calculates contribution to CV error of the predictions made
#' on a validation set, where the error is the cumulative Pareto deviance
#' for all validation sets (ONLY APPLICABLE TO emf & ADAPTED ERF)
#'@gamma: extreme value indices (vector)
#'@Z: excess ratios (vector) 
#'@return: sum of contributions to CV error of one fold
cv_err_cont <- function(gamma, Z){
  l = log(gamma)+((1/gamma)+1)*log(Z) #negative loglikelihood of pareto
  return(sum(l))
}

#'cv_brier: computes the Brier score
#'@probPred: calibrated predicted probabilities validation set (vector)
#'@trueClass: true observed class label
#'@return: Brier score
cv_brier <- function(probPred, trueClass){
  return(mean((probPred-trueClass)^2))
}
