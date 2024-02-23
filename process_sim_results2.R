setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')
directory = 'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/Results/SimPreds/'


respath  = 'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/Results/'

#' preds2ISRE: function that calculates ISREs based on predictions
#' @DGP: which dgp A, B or C
#' @dim: which dimension 10, 20 or 30
preds2ISRE <- function(DGP, dim){
  emf_predlist = list.files(path=paste0(directory,'EMF/', DGP, '/'), 
                            pattern=paste0('d',as.character(dim),'m50.csv'))
  erf_predlist = list.files(path=paste0(directory,'ERF/', DGP, '/'), 
                            pattern=paste0('d',as.character(dim),'m50.csv'))
  ecf_predlist = list.files(path=paste0(directory,'ECF/', DGP, '/'), 
                            pattern=paste0('d',as.character(dim),'m50.csv'))
  
  p = c(rep(0.95, 3), rep(0.995, 3), rep(0.99, 3))
  q = rep(seq(0.7, 0.9, 0.1), 3)
  
  for (i in 1:length(emf_predlist)){
    ISRE_frame = data.frame('ISRE_emf'=rep(NA, 50),
                            'ISRE_erf'=rep(NA, 50),
                            'ISRE_ecf'=rep(NA, 50))
    emf_pred = read.csv(file=paste0(directory, 'EMF/', DGP, '/', emf_predlist[i]))
    erf_pred = read.csv(file=paste0(directory, 'ERF/', DGP, '/', erf_predlist[i]))
    ecf_pred = read.csv(file=paste0(directory, 'ECF/', DGP, '/', ecf_predlist[i]))
    
    #true value and predictions
    true = emf_pred[1,]
    emf_pred = apply(emf_pred[-1,],MARGIN=1,FUN= as.list)
    erf_pred = apply(erf_pred[-1,],MARGIN=1,FUN= as.list)
    ecf_pred = apply(ecf_pred[-1,],MARGIN=1,FUN= as.list)
    
    #calculate ISRE for every simulation iteration
    ISRE_frame$ISRE_emf = as.numeric(lapply(emf_pred, FUN=calcISRE, true=true))
    ISRE_frame$ISRE_erf = as.numeric(lapply(erf_pred, FUN=calcISRE, true=true))
    ISRE_frame$ISRE_ecf = as.numeric(lapply(ecf_pred, FUN=calcISRE, true=true))
    
    write.csv(ISRE_frame, file=paste0(
      'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/Results/ISRE/',
      DGP, '/ISRE_DGP',
      DGP,
      'p',
      p[i],
      'q',
      q[i],
      'd',
      dim,
      'm50.csv'), row.names = FALSE)
  }
}

#'formatTable: formats results tables
#'@respath: directory path where results are stored
#'@DGP: which DGP A, B or C
#'@dim: which dimension 10, 20 or 30
formatTable <- function(respath, DGP, dim){
  filepaths = list.files(path=paste0(respath,'ISRE/',DGP), 
                         pattern=paste0('d',as.character(dim),'m50.csv'))
  
  res = data.frame(matrix(NA, nrow=9, ncol=6))
  colnames(res) = c('d', 'p', 'q',
                    paste0( 'mu_{emf}', ' (sigma_{emf})'),
                    paste0( 'mu_{erf}', ' (sigma_{erf})'),
                    paste0( 'mu{ecf}', ' (sigma_{ecf})'))
  
  res$d= c(rep(NA, 7), dim, NA)
  res$p = c(rep(0.95, 3), rep(0.995, 3), rep(0.99, 3))
  res$q = rep(seq(0.7, 0.9, 0.1), 3)
  
  for (i in 1:length(filepaths)){
    tab = read.csv(paste0(respath,'ISRE/',as.character(DGP),'/',filepaths[i]))
    mu_emf = round(mean(tab$ISRE_emf), digits=3)
    mu_erf = round(mean(tab$ISRE_erf), digits=3)
    mu_ecf = round(mean(tab$ISRE_ecf), digits=3)
    sig_emf = round(sd(tab$ISRE_emf), digits=3)
    sig_erf = round(sd(tab$ISRE_erf), digits=3)
    sig_ecf = round(sd(tab$ISRE_ecf), digits=3)
    
    #res emf
    res[i,4] = paste0(as.character(mu_emf),' (', as.character(sig_emf),')')
    #res erf
    res[i,5] = paste0(as.character(mu_erf),'(', as.character(sig_erf),')')
    #res ecf
    res[i,6] = paste0(as.character(mu_ecf),' (', as.character(sig_ecf),')')
    
  }
  write.csv(res, file=paste0(respath,'TablesSim/',
                             DGP,as.character(dim),'.csv'), row.names=FALSE)
}


# preds2ISRE('A', 10)
# preds2ISRE('A', 20)
# preds2ISRE('A', 30)
# 
# preds2ISRE('B', 10)
# preds2ISRE('B', 20)
# preds2ISRE('B', 30)
# 
# preds2ISRE('C', 10)
# preds2ISRE('C', 20)
# preds2ISRE('C', 30)

# formatTable(respath = respath, DGP='A', dim=10)
# formatTable(respath = respath, DGP='A', dim=20)
# formatTable(respath = respath, DGP='A', dim=30)
# 
# 
# formatTable(respath = respath, DGP='B', dim=10)
# formatTable(respath = respath, DGP='B', dim=20)
# formatTable(respath = respath, DGP='B', dim=30)
# 
# formatTable(respath = respath, DGP='C', dim=10)
# formatTable(respath = respath, DGP='C', dim=20)
# formatTable(respath = respath, DGP='C', dim=30)
