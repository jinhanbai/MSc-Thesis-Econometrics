setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')
directory = 'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/Results/'

#significance levels
alpha1 = 0.1
alpha2 = 0.05

#' testerino: function to test significance in performance difference
#' @DGP: which DGP ==> A, B or C
#' @dim: which DGP dimension to consider
#' @alpha1: significance level 1
#' @alpha2: significance level 2
testerino <- function(DGP, dim, alpha1, alpha2){
  resfiles = list.files(path=paste0(directory,'ISRE/', DGP), 
                        pattern=paste0('d',as.character(dim),'m50.csv'))
  
  #sw test results matrix, cell format: test statistic (p-value)
  test = data.frame(matrix(NA, nrow=9, ncol=7))
  colnames(test) = c('d', 'p', 'q', 'EMF JB', 'ERF JB',
                     'F', 'T')
  test$d = c(rep(NA, 7), dim, NA)
  test$p = c(rep(0.95, 3), rep(0.995, 3), rep(0.99, 3))
  test$q = rep(seq(0.7, 0.9, 0.1), 3)
  
  
  
  for (i in 1:length(resfiles)){
    res = read.csv(file=paste0(directory, 'ISRE/', DGP,'/', resfiles[i]))
    
    #emf-ISRE
    emfres = res$ISRE_emf
    #erf-ISRE
    erfres = res$ISRE_erf
    
    #conduct Jarque bera tests and find p-values up to 3 decimals
    emfSWp = round(jarque.bera.test(emfres)$p.value, digits=3)
    erfSWp = round(jarque.bera.test(erfres)$p.value, digits=3)
    # add asterix to indicate signifiance at various levels
    if(emfSWp < alpha2){
      test$`EMF JB`[i] = paste0(as.character(emfSWp), '**')
    }
    else if(emfSWp < alpha1){
      test$`EMF JB`[i] = paste0(as.character(emfSWp), '*')
    }
    else{
      test$`EMF JB`[i] = as.character(emfSWp)
    }
    
    if(erfSWp < alpha2){
      test$`ERF JB`[i] = paste0(as.character(erfSWp), '**')
    }
    else if(erfSWp < alpha1){
      test$`ERF JB`[i] = paste0(as.character(erfSWp), '*')
    }
    else{
      test$`ERF JB`[i] = as.character(erfSWp)
    }
    
    #conduct F-test to test variance equality and find p-value up to 3 decimals
    fTestp =  round(var.test(emfres, erfres, alternative='two.sided')$p.value, digits=3)
    
    # add asterix to indicate signifiance at various levels
    if(fTestp < alpha2){
      test$`F`[i] = paste0(as.character(fTestp), '**')
    }
    else if(fTestp < alpha1){
      test$`F`[i] = paste0(as.character(fTestp), '*')
    }
    else{
      test$`F`[i] = as.character(fTestp)
    }
    
    #conduct two sample t-test to test equality of means and find p-value up to 3 decimals
    tTestp = round(t.test(emfres, erfres, var.equal=TRUE)$p.value, digits=3)
    
    # add asterix to indicate signifiance at various levels
    if(tTestp < alpha2){
      test$`T`[i] = paste0(as.character(tTestp), '**')
    }
    else if(tTestp < alpha1){
      test$`T`[i] = paste0(as.character(tTestp), '*')
    }
    else{
      test$`T`[i] = as.character(tTestp)
    }
  }
  write.csv(test, file=paste0(directory, 'ResTests/test', 
                              DGP, as.character(dim), '.csv'), row.names = FALSE)
}

testerino('A', 10, alpha1, alpha2)
testerino('A', 20, alpha1, alpha2)
testerino('A', 30, alpha1, alpha2)

testerino('B', 10, alpha1, alpha2)
testerino('B', 20, alpha1, alpha2)
testerino('B', 30, alpha1, alpha2)

testerino('C', 10, alpha1, alpha2)
testerino('C', 20, alpha1, alpha2)
testerino('C', 30, alpha1, alpha2)

