setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')

#which run?
p = 0.99 #0.99, 0.95
q = 0.85 #0.7, 0.75, 0.8, 0.85, 0.9, 0.95
window = 'Expanding'  #'Fixed', 'Rolling'

################################################################################
preds = read.csv(file=paste0(getwd(), 
                             '/Results/Application/',
                             window,
                             '_preds_p',
                             as.character(p),
                             '_q',
                             as.character(q),
                             '.csv'))

# % violations
v = data.frame(
  'HS'= preds$true.values>preds$varHS_predictions,
  'EMF'=preds$true.values>preds$emf_predictions,
  'ERF' = preds$true.values>preds$erf_predictions,
  'ECF' = preds$true.values>preds$ecf_predictions
)
violations = data.frame(
  'HS' = sum(preds$true.values>preds$varHS_predictions),
  'EMF' = sum(preds$true.values>preds$emf_predictions),
  'ERF' = sum(preds$true.values>preds$erf_predictions),
  'ECF' = sum(preds$true.values>preds$ecf_predictions)
)

# unconditional coverage test => get pvalue and inverse transform to get statistic
hs_uc_L = ucTest(preds$true.values, preds$varHS_predictions, p)$LR
hs_uc_p = ucTest(preds$true.values, preds$varHS_predictions, p)$p_value

emf_uc_L = ucTest(preds$true.values, preds$emf_predictions, p)$LR
emf_uc_p = ucTest(preds$true.values, preds$emf_predictions, p)$p_value

erf_uc_L = ucTest(preds$true.values, preds$erf_predictions, p)$LR
erf_uc_p = ucTest(preds$true.values, preds$erf_predictions, p)$p_value

ecf_uc_L = ucTest(preds$true.values, preds$ecf_predictions, p)$LR
ecf_uc_p = ucTest(preds$true.values, preds$ecf_predictions, p)$p_value



# independence coverage test => get pvalue and inverse transofmration to get statistic
hs_ind_p = indTest(preds$true.values, preds$varHS_predictions, p)$p_value
hs_ind_L = indTest(preds$true.values, preds$varHS_predictions, p)$LR

emf_ind_p = indTest(preds$true.values, preds$emf_predictions, p)$p_value
emf_ind_L = indTest(preds$true.values, preds$emf_predictions, p)$LR

erf_ind_p = indTest(preds$true.values, preds$erf_predictions, p)$p_value
erf_ind_L = indTest(preds$true.values, preds$erf_predictions, p)$LR

ecf_ind_p = indTest(preds$true.values, preds$ecf_predictions, p)$p_value
ecf_ind_L = indTest(preds$true.values, preds$ecf_predictions, p)$LR

# aggregate statistics for conditional coverage => perform chi-2 test to get pvalue
hs_cci_L = hs_uc_L + hs_ind_L
hs_cci_p = pchisq(hs_cci_L, 2, lower.tail = FALSE)

emf_cci_L = emf_uc_L + emf_ind_L
emf_cci_p = pchisq(emf_cci_L, 2, lower.tail = FALSE)

erf_cci_L = erf_uc_L + erf_ind_L
erf_cci_p = pchisq(erf_cci_L, 2, lower.tail=FALSE)

ecf_cci_L = ecf_uc_L + ecf_ind_L
ecf_cci_p = pchisq(ecf_cci_L, 2, lower.tail=FALSE)



