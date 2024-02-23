#'run_sim: runs simulation for a given combination of input parameters
#'@m: Number of simulation repetitions (scalar)
#'@p: high confidence level (scalar)
#'@q: intermediate confidence level (scalar)
#'@d: predictor space dimension (scalar)
#'@dgp: which dgp to consider 
#'@pars_k1: parameters to be tried for minimum nodesize quantile forest
#'@pars_k2: parameters to be tried for minimum nodesize regression forest
#'@lambda: parameters to be tried for penalty parameter ERF
#'@pars_k_reg_ecf: parameters to be tried for ecf regression forest
#'@pars_k_class_ecf: parameters to be tried for ecf classification forest

run_sim <- function(m=50, 
                    p=c(0.95, 0.99, 0.995), 
                    q, 
                    d,
                    dgp,
                    pars_k1=c(20,80,200), 
                    pars_k2=c(10,20,40),lambda=c(0,0.01,0.001),
                    pars_k_reg_ecf=c(10, 20, 40),
                    pars_k_class_ecf=c(20, 80, 200)){
  
  print(paste0('Time of initialization: ', Sys.time()))
  
  trainData = read.csv(file=paste0(getwd(),'/','DGPs/','Train_',dgp, '_dim_',as.character(d),'.csv'))
  testData = read.csv(file=paste0(getwd(),'/','DGPs/','Test_',dgp, '_dim_',as.character(d),'.csv'))
  X_train = trainData[!(colnames(trainData) %in% 'Y_train')]
  Y_train = trainData$Y_train
  X_test = testData[!(colnames(testData) %in% 'Y_test')]
  Y_test = testData$Y_test
  
  
  
  #find true distribution parameters
  truedist = switch(dgp,
                    'A'=pars_a(X_test),
                    'B'=pars_b(X_test),
                    'C'=pars_c(X_test)
  )
  preds_emf = list()
  preds_erf = list()
  preds_ecf = list()
  
  #initialize list of result matrices for every method to store predictions
  #format: row1 = true value, rows 2-m+1 = predictions 
  
  for(i in p){
    true_quantile = as.numeric(ggdist::qstudent_t(p=i, df=truedist$df, mu=0, sigma=truedist$sigma))
    res = data.frame(matrix(NA, nrow=m, ncol=length(true_quantile)))
    res = data.frame(rbind(true_quantile, res))
    
    preds_emf = append(preds_emf, list(res))
    preds_erf = append(preds_erf, list(res))
    preds_ecf = append(preds_ecf, list(res))
  }
  #threshold for ecf method
  u = sort(Y_train, decreasing = FALSE)[ceiling(q*length(Y_train))]
  
  for(i in 1:m){
    t1 = Sys.time()
    
    #cross-validation to find best parameters
    cv = cv_methods(trainData, q=q, pars_k1=pars_k1, pars_k2=pars_k2,
                    pars_k_reg_ecf=pars_k_reg_ecf,
                    pars_k_class_ecf=pars_k_class_ecf, K=5, R=5)
    
    #extract best parameters
    k1_emf = cv$bestemf$nodeSize1
    k2_emf = cv$bestemf$nodeSize2
    k_erf = cv$bestERF$nodeSize
    lambda_erf = cv$bestERF$lambda
    kreg_ecf = cv$bestecfReg
    kclass_ecf = cv$bestecfClass
    
    
    #fit the methods using their best set of parameters
    emffit = emf_fit(X_train, Y_train, q, k1_emf, k2_emf,
                     100, 100)
    
    erffit = erf_fit(X_train, Y_train, q, k_erf, 100)
    
    ecfregfit = ecf_reg_fit(X_train, Y_train, u, kreg_ecf, 100)
    ecfclassfit = ecf_class_fit(X_train, Y_train, u, kclass_ecf, 100)
    
    #for every p, make predictions and store
    for(j in 1:length(p)){
      emfpred = emf_pred(X_test, p[j], q, emffit)
      erfpred = erf_pred(X_train, Y_train, X_test, p[j], q, erffit, lambda_erf, App=FALSE)
      ecfpred = ecf_pred(X_test, p[j], u, ecfregfit, ecfclassfit)
      
      preds_emf[[j]][i+1,] = as.numeric(emfpred$pred_varp)
      preds_erf[[j]][i+1,] = as.numeric(erfpred$pred_varp)
      preds_ecf[[j]][i+1,] = as.numeric(ecfpred$pred_varp)
    }
    
    t2 = Sys.time()
    print(paste0('Progress: ', as.character(i), '/', as.character(m), '|',
                 as.character((i/m)*100), '%', '|', 'Timer: ', 
                 as.character(Sys.time())))
  }
  #for every p, write predictions to csv files in designated folders
  for(j in 1:length(p)){
    #write EMF 
    write_csv(preds_emf[[j]], file=paste0(getwd(),'/','Results/SimPreds/EMF/', dgp,'/',
                                          'DGP', dgp,
                                          'p', as.character(p[j]),
                                          'q', as.character(q),
                                          'd', as.character(d),
                                          'm', as.character(m),'.csv'))
    #write ERF
    write_csv(preds_erf[[j]], file=paste0(getwd(),'/','Results/SimPreds/ERF/', dgp,'/',
                                          'DGP', dgp,
                                          'p', as.character(p[j]),
                                          'q', as.character(q),
                                          'd', as.character(d),
                                          'm', as.character(m),'.csv'))
    #write ECF
    write_csv(preds_ecf[[j]], file=paste0(getwd(),'/','Results/SimPreds/ECF/', dgp,'/',
                                          'DGP', dgp,
                                          'p', as.character(p[j]),
                                          'q', as.character(q),
                                          'd', as.character(d),
                                          'm', as.character(m),'.csv'))
  }
  print(paste0('Finished at: ', Sys.time()))
}

