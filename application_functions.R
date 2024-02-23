### Fixed window
fixed_window <- function(train, test, cv_res, p, q, u){
  print('Window: Fixed')
  #initialize parameters
  p=p
  q=q
  u=u
  #get optimal tuning parameters
  best_emf_k1 = cv_res$bestemf$nodeSize1
  best_emf_k2 = cv_res$bestemf$nodeSize2
  best_erf_k = cv_res$bestERF$nodeSize
  best_erf_lambda = cv_res$bestERF$lambda
  best_ecf_k1 = cv_res$bestecfReg$nodeSize_reg
  best_ecf_k2 = cv_res$bestecfClass$nodeSize_class
  
  #fit and test data frames
  curr_data = train
  
  X_data = subset(curr_data, select=-Y)
  Y_data = curr_data$Y
  
  #initialize predictions dataframe
  res = data.frame('emf_predictions'=rep(NA, nrow(test)),
                   'erf_predictions'=rep(NA, nrow(test)),
                   'ecf_predictions'=rep(NA, nrow(test)),
                   'varHS_predictions'=rep(NA, nrow(test)),
                   'true values'=test$Y)
  
  ##fit methods on current dataframe with best parameters
  #fit emf
  emffit = emf_fit(X_data, Y_data, q, best_emf_k1, best_emf_k2, 500, 500)
  #fit erf
  erffit = erf_fit(X_data, Y_data, q, best_erf_k, 500)
  #fit ecf regression forest
  ecf_regfit = ecf_reg_fit(X_data, Y_data, u, best_ecf_k1, 500)
  #fit ecf classification forest
  ecf_classfit = ecf_class_fit(X_data, Y_data, u, best_ecf_k2, 500)
  
  for(i in 1:nrow(test)){
    print(paste0('Current iteration:', 
                 as.character(i),
                 '/', 
                 as.character(nrow(test))))
    
    X_new = subset(test[i,], select=-Y)
    
    ##predict on new observation i
    #predict emf and store
    res$emf_predictions[i] = emf_pred(X_test=X_new, p=p, q=q, fit=emffit)$pred_varp
    #predict erf
    res$erf_predictions[i] = erf_pred(X_train=X_data, Y_train=Y_data, X_test=X_new, p=p, q=q,
                                      fit=erffit,
                                      lambda=best_erf_lambda, 
                                      App=TRUE)$pred_varp
    #predict ecf
    res$ecf_predictions[i] = ecf_pred(X_test=X_new, p=p, u=u, 
                                      ecf_reg=ecf_regfit,
                                      ecf_class=ecf_classfit)$pred_varp
    #predict var HS
    res$varHS_predictions[i] = sort(curr_data$Y, decreasing = FALSE)[ceiling(p*length(curr_data$Y))]
    
  }
  return(list('CV_results'=cv_res, 'predictions'=res))
}

### Expanding window
expanding_window <- function(train, test, cv_res, p, q, u){
  print('Window: Expanding')
  #initialize parameters
  p=p
  q=q
  u=u
  #get optimal tuning parameters
  best_emf_k1 = cv_res$bestemf$nodeSize1
  best_emf_k2 = cv_res$bestemf$nodeSize2
  best_erf_k = cv_res$bestERF$nodeSize
  best_erf_lambda = cv_res$bestERF$lambda
  best_ecf_k1 = cv_res$bestecfReg$nodeSize_reg
  best_ecf_k2 = cv_res$bestecfClass$nodeSize_class
  
  #fitting data
  curr_data = train
  
  
  #initialize predictions dataframe
  res = data.frame('emf_predictions'=rep(NA, nrow(test)),
                   'erf_predictions'=rep(NA, nrow(test)),
                   'ecf_predictions'=rep(NA, nrow(test)),
                   'varHS_predictions'=rep(NA, nrow(test)),
                   'true values'=test$Y)
  
  for(i in 1:nrow(test)){
    print(paste0('Current iteration:', 
                 as.character(i),
                 '/', 
                 as.character(nrow(test))))
    Y_data = curr_data$Y
    X_data = subset(curr_data, select=-Y)
    X_new = subset(test[i,], select=-Y)
    
    ##fit methods on current dataframe with best parameters
    #fit emf
    emffit = emf_fit(X_train=X_data, Y_train=Y_data, q, best_emf_k1, best_emf_k2, 500, 500)
    #fit erf
    erffit = erf_fit(X_train=X_data, Y_train=Y_data, q, best_erf_k, 500)
    #fit ecf regression forest
    ecf_regfit = ecf_reg_fit(X_data, Y_data, u, best_ecf_k1, 500)
    #fit ecf classification forest
    ecf_classfit = ecf_class_fit(X_data, Y_data, u, best_ecf_k2, 500)
    
    ##predict on new observation i
    #predict emf and store
    res$emf_predictions[i] = emf_pred(X_test=X_new, p=p, q=q, fit=emffit)$pred_varp
    #predict erf
    res$erf_predictions[i] = erf_pred(X_train=X_data, Y_train=Y_data, X_test=X_new, p=p, q=q,
                                      fit=erffit,
                                      lambda=best_erf_lambda, 
                                      App=TRUE)$pred_varp
    #predict ecf
    res$ecf_predictions[i] = ecf_pred(X_test=X_new, p=p, u=u, 
                                      ecf_reg=ecf_regfit,
                                      ecf_class=ecf_classfit)$pred_varp
    #predict var HS
    res$varHS_predictions[i] = sort(curr_data$Y, decreasing = FALSE)[ceiling(p*length(curr_data$Y))]
    ##bind observation i to current dataframe
    curr_data = rbind(curr_data, test[i,])
  }
  return(list('CV_results'=cv_res, 'predictions'=res))
}

### Rolling Window
rolling_window <- function(train, test, cv_res, p, q, u){
  print('Window: Rolling')
  #initialize parameters
  p=p
  q=q
  u=u
  #get optimal tuning parameters
  best_emf_k1 = cv_res$bestemf$nodeSize1
  best_emf_k2 = cv_res$bestemf$nodeSize2
  best_erf_k = cv_res$bestERF$nodeSize
  best_erf_lambda = cv_res$bestERF$lambda
  best_ecf_k1 = cv_res$bestecfReg$nodeSize_reg
  best_ecf_k2 = cv_res$bestecfClass$nodeSize_class
  
  #fitting data
  curr_data = train
  
  
  #initialize predictions dataframe
  res = data.frame('emf_predictions'=rep(NA, nrow(test)),
                   'erf_predictions'=rep(NA, nrow(test)),
                   'ecf_predictions'=rep(NA, nrow(test)),
                   'varHS_predictions'=rep(NA, nrow(test)),
                   'true values'=test$Y)
  
  for(i in 1:nrow(test)){
    print(paste0('Current iteration:', 
                 as.character(i),
                 '/', 
                 as.character(nrow(test))))
    Y_data = curr_data$Y
    X_data = subset(curr_data, select=-Y)
    X_new = subset(test[i,], select=-Y)
    
    ##fit methods on current dataframe with best parameters
    #fit emf
    emffit = emf_fit(X_train=X_data, Y_train=Y_data, q, best_emf_k1, best_emf_k2, 500, 500)
    #fit erf
    erffit = erf_fit(X_train=X_data, Y_train=Y_data, q, best_erf_k, 500)
    #fit ecf regression forest
    ecf_regfit = ecf_reg_fit(X_data, Y_data, u, best_ecf_k1, 500)
    #fit ecf classification forest
    ecf_classfit = ecf_class_fit(X_data, Y_data, u, best_ecf_k2, 500)
    
    ##predict on new observation i
    #predict emf and store
    res$emf_predictions[i] = emf_pred(X_test=X_new, p=p, q=q, fit=emffit)$pred_varp
    #predict erf
    res$erf_predictions[i] = erf_pred(X_train=X_data, Y_train=Y_data, X_test=X_new, p=p, q=q,
                                      fit=erffit,
                                      lambda=best_erf_lambda, 
                                      App=TRUE)$pred_varp
    #predict ecf
    res$ecf_predictions[i] = ecf_pred(X_test=X_new, p=p, u=u, 
                                      ecf_reg=ecf_regfit,
                                      ecf_class=ecf_classfit)$pred_varp
    #predict var HS
    res$varHS_predictions[i] = sort(curr_data$Y, decreasing = FALSE)[ceiling(p*length(curr_data$Y))]
    ##update fitting window
    size = nrow(curr_data)
    binded = rbind(curr_data, test[i,])[]
    curr_data = binded[-1,]
  }
  return(list('CV_results'=cv_res, 'predictions'=res))
}

### Run application
run_app <- function(p, q, u, train, test){
  print(paste0('Initialization time: ', as.character(Sys.time())))
  print(paste0('p, q: ', as.character(p), ', ', as.character(q)))
  
  cv = cv_methods(data=train_Data, q=q, R=5, K=5)
  
  print(paste0('CV done at: ', as.character(Sys.time())))
  
  
  res_fixed = fixed_window(train, test, cv, p, q, u)
  #res_expanding = expanding_window(train, test, cv, p, q, u)
  res_rolling = rolling_window(train, test, cv, p, q, u)
  
  write.csv(res_fixed$predictions, file=paste0(datapath,
                                               '/Results/Application/',
                                               '/Fixed_preds_p',
                                               as.character(p),
                                               '_q',
                                               as.character(q),
                                               '.csv'),
            row.names = FALSE)
  #write.csv(res_expanding$predictions, file=paste0(datapath,
  #                                                 '/Results/Application/',
  #                                                 '/Expanding_preds_p',
  #                                                 as.character(p),
  #                                                 '_q',
  #                                                 as.character(q),
  #                                                 '.csv'),
  #          row.names = FALSE)
  
  write.csv(res_rolling$predictions, file=paste0(datapath,
                                                 '/Results/Application/',
                                                 '/Rolling_preds_p',
                                                 as.character(p),
                                                 '_q',
                                                 as.character(q),
                                                 '.csv'),
            row.names = FALSE)
}

ucTest <- function (true, pred, p){
  
  true = true[-1]
  pred = pred[-1]
  
  true = as.vector(true)
  pred = as.vector(pred)
  T_tilde = length(true)
  V = sum(true>pred)
  pihat = V/T_tilde
  num = ((1-p)^V)*p^(T_tilde-V)
  denom = ((1-pihat)^(T_tilde-V))*(pihat^V)
  #browser()
  if((num==0) & (denom!=0)){
    LR = 0
  }
  else if((denom==0)&(num!=0)){
    LR = Inf
  }
  else if((denom==0)&(num==0)){
    stop('LR numerator and denominator are 0')
  }
  else{
    LR = -2*log(num/denom) 
  }
  p_value = 1 - pchisq(LR, 1)
  return(list('LR'=LR, 'p_value' = p_value))
}

indTest <- function (true, pred, p){
  true = as.vector(true)
  pred = as.vector(pred)
  T_tilde = length(true)
  I = true>pred
  n00 = 0
  n01 = 0
  n10 = 0
  n11 = 0
  for (i in 2: T_tilde){
    if(I[i] == FALSE){
      if(I[i-1]==FALSE){
        n00 = n00+1
      } 
      else{
        n10 = n10+1
      }
    } 
    else{
      if(I[i-1]==FALSE){
        n01 = n01+1
      }
      else{
        n11 = n11+1
      }
    }
  }
  #browser()
  pi00 = n00/(n00 + n01)
  if(is.nan(pi00)){
    pi00 = Inf
  }
  pi10 = n10/(n11 + n10)
  if(is.nan(pi10)){
    pi10 = Inf
  }
  pi01 = n01/(n00 + n01)
  if(is.nan(pi01)){
    pi01 = Inf
  }
  pi11 = n11/(n10 + n11)
  if(is.nan(pi11)){
    pi11 = Inf
  }
  
  pi2hat = (n01+n11)/(n00+n10+n01+n11)
  num = ((1-pi2hat)^(n00+n10))*((pi2hat)^(n01+n11))
  denom = (pi00^n00)*(pi01^n01)*(pi10^n10)*(pi11^n11)
  
  if((num==0) & (denom!=0)){
    LR = 0
  }
  else if((denom==0)&(num!=0)){
    LR = Inf
  }
  else if((denom==0)&(num==0)){
    stop('LR numerator and denominator are 0')
  }
  else{
    LR = -2*log(num/denom) 
  }
  p_value = 1 - pchisq(LR, 1)
  return(list('LR'=LR, 'p_value' = p_value))
}
