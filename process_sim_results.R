setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')
directory = 'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/Results/'
DGP = 'A'


#'process_sim_res: function to process simulation results and store to table
#'@directory: directory path
#'@DGP: which dgp A, B or C
#'@dim: which dimension 10, 20 or 30
process_sim_res <- function(directory, DGP, dim){
  filepaths = list.files(path=paste0(directory,DGP), 
                         pattern=paste0('d',as.character(dim),'m50.csv'))
  res = data.frame(matrix(NA, nrow=9, ncol=4))
  colnames(res) = c('(p, q)',
                     paste0( 'mu_{emf}', ' (sigma_{emf})'),
                     paste0( 'mu_{erf}', ' (sigma_{erf})'),
                     paste0( 'mu{ecf}', ' (sigma_{ecf})'))
  res[, 1] = c('(0.95, 0.7)', '(0.95, 0.8)', '(0.95, 0.9)',
               '(0.995, 0.7)', '(0.995, 0.8)', '(0.995, 0.9)',
               '(0.99, 0.7)', '(0.99, 0.8)', '(0.99, 0.9)')
  for (i in 1:length(filepaths)){
    tab = read.csv(paste0(directory,as.character(DGP),'/',filepaths[i]))
    mu_emf = round(mean(tab$ise_gold), digits=2)
    mu_erf = round(mean(tab$ise_erf), digits=2)
    mu_ecf = round(mean(tab$ise_ahmed), digits=2)
    sig_emf = round(sd(tab$ise_gold), digits=2)
    sig_erf = round(sd(tab$ise_erf), digits=2)
    sig_ecf = round(sd(tab$ise_ahmed), digits=2)
    
    #res emf
    res[i,2] = paste0(as.character(mu_emf),' (', as.character(sig_emf),')')
    #res erf
    res[i,3] = paste0(as.character(mu_erf),'(', as.character(sig_erf),')')
    #res ecf
    res[i,4] = paste0(as.character(mu_ecf),' (', as.character(sig_ecf),')')
    
  }
  write.csv(res, file=paste0(directory,'TablesSim/',
                             DGP,as.character(dim),'.csv'), row.names=FALSE)
}

#DGP A
# process_sim_res(directory, 'A', 10)
# process_sim_res(directory, 'A', 20)
# process_sim_res(directory, 'A', 30)
# 
# #DGP B
# process_sim_res(directory, 'B', 10)
# process_sim_res(directory, 'B', 20)
# process_sim_res(directory, 'B', 30)
# 
# #DGP C
# process_sim_res(directory, 'C', 10)
# process_sim_res(directory, 'C', 20)
# process_sim_res(directory, 'C', 30)

