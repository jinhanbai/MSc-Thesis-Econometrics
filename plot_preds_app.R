setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')


p = 0.995 #0.99, 0.995
q = 0.75 #0.7, 0.75, 0.8, 0.85, 0.9, 0.95
window = 'Rolling' #'Fixed', 'Expanding'

preds = read.csv(file=paste0(getwd(), 
                             '/Results/Application/',
                             window,
                             '_preds_p',
                             as.character(p),
                             '_q',
                             as.character(q),
                             '.csv'))
preds_emf = preds$emf_predictions
preds_erf = preds$erf_predictions
preds_ecf = preds$ecf_predictions
preds_hs = preds$varHS_predictions
true = preds$true.values
index = rownames(preds)

# plot(index, preds_hs, pch=4, col='red', xlab='Index',
#      ylab='Loss return %', main=paste0('HS with p = ', as.character(p)), xlim=c(0, 1050), ylim=c(0, max(preds_hs)+1))
# lines(index, true, type='l', col='black')
# legend('topright',cex=0.7, legend=c('VaR_hat', 'Actual Loss'), pch=c('x','-'), col=c('red', 'black'),
#        text.font=3)

plot(index, preds_emf, pch=4, col='green', xlab='Index',
     ylab='Loss return %', main=paste0('EMF with p = ', as.character(p), ' and q = ', as.character(q)), xlim=c(0, 1050), ylim=c(0, max(preds_emf)+1))
lines(index, true, type='l', col='black')
legend('topright',cex=0.7, legend=c('VaR_hat', 'Actual Loss'), pch=c('x','-'), col=c('green', 'black'),
       text.font=3)

plot(index, preds_erf, pch=4, col='blue', xlab='Index',
     ylab='Loss return %', main=paste0('ERF with p = ', as.character(p), ' and q = ', as.character(q)), xlim=c(0, 1050), ylim=c(0, max(preds_erf)+1))
lines(index, true, type='l', col='black')
legend('topright',cex=0.7, legend=c('VaR_hat', 'Actual Loss'), pch=c('x','-'), col=c('blue', 'black'),
       text.font=3)

plot(index, preds_ecf, pch=4, col='purple', xlab='Index',
     ylab='Loss return %', main=paste0('ECF with p = ', as.character(p), ' and q = ', as.character(q)), xlim=c(0, 1050), ylim=c(0, max(preds_ecf)+1))
lines(index, true, type='l', col='black')
legend('topright',cex=0.7, legend=c('VaR_hat', 'Actual Loss'), pch=c('x','-'), col=c('purple', 'black'),
       text.font=3)

