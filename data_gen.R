#'pars_a: returns Student's t distribution parameters for DGP A
#'@X: predictors (dataframe)
#'@return: list containing distribution parameters
pars_a <- function(X){
  df = 5
  sigma = 3*(1+1*(X[,1]>0)+1*(X[,2]<0))
  return(list('df'= df, 'sigma'=sigma))
}

#'pars_b: returns Student's t distribution parameters for DGP B
#'@X: predictors (dataframe)
#'@return: list containing distribution parameters
pars_b <- function(X){
  df = 3+X[,3]^2+ (X[,3]^3)*(X[,3]>0)
  sigma = 3*(1+1*(X[,1]>0)+1*(X[,2]<0))
  return(list('df'= df, 'sigma'=sigma))
}

#'pars_c: returns Student's t distribution parameters for DGP C
#'@X: predictors (dataframe)
#'@return: list containing distribution parameters
pars_c <- function(X){
  df = 3 + 18/((exp(X[,3]^2)+2)^2)
  sigma = (2+tanh(3*X[,1]))*exp(1+X[,2]^2)
  return(list('df'= df, 'sigma'=sigma))
}

#'return_pars: returns true distribution parameters for certain DGP
#'@dgp: which dgp to consider (character)
#'@X: predictors (dataframe)
#'@return: list containing conditional degrees of freedom and scale
return_pars <- function(dgp=c('A','B','C'), X){
  pars = switch(dgp,
                'A'=pars_a(X),
                'B'=pars_b(X),
                'C'=pars_c(X))
  return(pars)
}
  
  
#'gen_a: generates data for DGP A and writes to csv
#'@d: predictor space dimension (scalar)
#'@return: list containing train data, test data and true distribution parameters of test (for peformance evaluation)
gen_a <- function(d=c(10, 20, 30)){
  n_train = 5000
  n_test = 1000
  #Training data
  X_train = data.frame(matrix(runif(n_train*d, min=-1,max=1), nrow=n_train, ncol=d))
  pars_train = pars_a(X_train)
  Y_train = ggdist::rstudent_t(n_train, df=pars_train$df, mu=0, sigma=pars_train$sigma)
  data_train = cbind(X_train,data.frame(Y_train))
  write.csv(data_train, file=paste0(getwd(),'/','DGPs/','Train_A_dim_',as.character(d), '.csv' )
            ,row.names = FALSE)
  
  #Test data
  X_test = data.frame(matrix(mnorm::halton(n_test*d, base=as.integer(2), random='Tuffin')*2-1,
                             nrow=n_test, ncol=d))
  pars_test = pars_a(X_test)
  Y_test = ggdist::rstudent_t(n_test, df=pars_test$df, mu=0, sigma=pars_test$sigma)
  data_test = cbind(X_test, data.frame(Y_test))
  write.csv(data_test, file=paste0(getwd(),'/','DGPs/','Test_A_dim_',as.character(d), '.csv' )
            ,row.names = FALSE)
  return(list('dataTrain'=data_train, 
              'dataTest'=data_test,
              'dfTest'=pars_test$df,
              'sigmaTest'=pars_test$sigma))
}

#'gen_b: generates data for DGP B and writes to csv
#'@d: predictor space dimension (scalar)
#'@return: list containing train data, test data and true distribution parameters of test (for peformance evaluation)
gen_b <- function(d=c(10, 20, 30)){
  n_train = 5000
  n_test = 1000
  #Training data
  X_train = data.frame(matrix(runif(n_train*d, min=-1,max=1), nrow=n_train, ncol=d))
  pars_train = pars_b(X_train)
  Y_train = ggdist::rstudent_t(n_train, df=pars_train$df, mu=0, sigma=pars_train$sigma)
  data_train = cbind(X_train,data.frame(Y_train))
  write.csv(data_train, file=paste0(getwd(),'/','DGPs/','Train_B_dim_',as.character(d), '.csv' )
            ,row.names = FALSE)
  
  #Test data
  X_test = data.frame(matrix(mnorm::halton(n_test*d, base=as.integer(2), random='Tuffin')*2-1,
                             nrow=n_test, ncol=d))
  pars_test = pars_b(X_test)
  Y_test = ggdist::rstudent_t(n_test, df=pars_test$df, mu=0, sigma=pars_test$sigma)
  data_test = cbind(X_test, data.frame(Y_test))
  write.csv(data_test, file=paste0(getwd(),'/','DGPs/','Test_B_dim_',as.character(d), '.csv' )
            ,row.names = FALSE)
  return(list('dataTrain'=data_train, 
              'dataTest'=data_test,
              'dfTest'=pars_test$df,
              'sigmaTest'=pars_test$sigma))
}

#'gen_c: generates data for DGP C and writes to csv
#'@d: predictor space dimension (scalar)
#'@return: list containing train data, test data and true distribution parameters of test (for peformance evaluation)
gen_c <- function(d=c(10, 20, 30)){
  n_train = 5000
  n_test = 1000
  
  #Train data
  #generate correlated uniform variables using inverse transformation
  #the first 8 variables are correlated with correlation coefficient 0.7
  mu = rep(0, 8)
  sigma = matrix(0.7, nrow=8, ncol=8)+diag(8)*0.3
  X_train = data.frame(cbind(pnorm(MASS::mvrnorm(n=n_train, mu=mu, Sigma=sigma))*2-1, 
                             matrix(runif(n_train*(d-8), min=-1,max=1), nrow=n_train, ncol=(d-8))))
  pars_train = pars_c(X_train)
  Y_train = ggdist::rstudent_t(n_train, df=pars_train$df, mu=0, sigma=pars_train$sigma)
  data_train = cbind(X_train,data.frame(Y_train))
  write.csv(data_train, file=paste0(getwd(),'/','DGPs/','Train_C_dim_',as.character(d), '.csv' )
            ,row.names = FALSE)
  
  #Test data
  X_test = data.frame(cbind(pnorm(MASS::mvrnorm(n=n_test, mu=mu, Sigma=sigma))*2-1, 
                            matrix(mnorm::halton(n_test*(d-8), base=as.integer(2), random='Tuffin')*2-1,
                                   nrow=n_test, ncol=(d-8))))
  pars_test = pars_c(X_test)
  Y_test = ggdist::rstudent_t(n_test, df=pars_test$df, mu=0, sigma=pars_test$sigma)
  data_test = cbind(X_test, data.frame(Y_test))
  write.csv(data_test, file=paste0(getwd(),'/','DGPs/','Test_C_dim_',as.character(d), '.csv' )
            ,row.names = FALSE)
  return(list('dataTrain'=data_train, 
              'dataTest'=data_test,
              'dfTest'=pars_test$df,
              'sigmaTest'=pars_test$sigma))
}

#testa = gen_a(30)
#testb = gen_b(30)
#testc = gen_c(30)

