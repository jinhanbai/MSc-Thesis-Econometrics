source('runfile.R')
source('raw_daily_data_prep.R')
source('raw_monthly_data_prep.R')
source('raw_quarterly_data_prep.R')

## INITIALIZE dataframe, to be merged with other variables later
Data = data.frame('Date'=ndx$Date,'Y_ndx_loss'=ndx$ndx_loss,
                  'ndx_loss_l1'=ndx$ndx_loss_l1,
                  'ndx_loss_l2'=ndx$ndx_loss_l2,
                  'ndx_price'=ndx$ndx)
Data = merge(Data, ndx_vol, by='Date')
# shift all dates by 1 to the future, since you want to estimate var_t using
# information set at t-1. You want to relate Y_t with x_t-1
Data$Date = Data$Date-1
Data = na.omit(Data)

#Create density plots of the negative loss distribution
hist(Data$Y_ndx_loss, freq = FALSE, main = "Histogram and density")
denL = density(Data$Y_ndx_loss)
lines(denL, lwd = 2, col = "red")
rug(jitter(Data$Y_ndx_loss))

# Time series plot of negative returns
ts_plot(ts(Data$Y_ndx_loss))

#JB test on loss returns
JB = jarque.bera.test(Data$Y_ndx_loss)

########### Aggregate covariates to dataframe

#indices
Data = merge(Data, aex, by.x='Date', by.y='Date')
Data = merge(Data, gdaxi, by.x='Date', by.y='Date')
Data = merge(Data, hsi, by.x='Date', by.y='Date')
Data = merge(Data, topx, by.x='Date', by.y='Date')
Data = merge(Data, rut, by.x='Date', by.y='Date')
Data = merge(Data, ssec, by.x='Date', by.y='Date')
Data = merge(Data, irts, by.x='Date', by.y='Date')
Data = merge(Data, n100, by.x='Date', by.y='Date')

#vix
Data = merge(Data, vix, by.x='Date', by.y='Date')

#fama french
Data = merge(Data, fama, by.x='Date', by.y='Date')

#FX = dxy
Data = merge(Data, dxy, by.x='Date', by.y='Date')

#commodities
Data = merge(Data, oil, by.x='Date', by.y='Date')
Data = merge(Data, gold, by.x='Date', by.y='Date')
Data = merge(Data, wheat, by.x='Date', by.y='Date')
Data = merge(Data, silver, by.x='Date', by.y='Date')
Data = merge(Data, natgas, by.x='Date', by.y='Date')

#rates
Data = merge(Data, libor, by.x='Date', by.y='Date')
Data = merge(Data, fedrate, by.x='Date', by.y='Date')
Data = merge(Data, baa10y, by.x='Date', by.y='Date')

#treasury
Data = merge(Data, ust1m, by.x='Date', by.y='Date')
Data = merge(Data, ust3m, by.x='Date', by.y='Date')
Data = merge(Data, ust1y, by.x='Date', by.y='Date')
Data = merge(Data, ust2y, by.x='Date', by.y='Date')
Data = merge(Data, ust5y, by.x='Date', by.y='Date')
Data = merge(Data, ust10y, by.x='Date', by.y='Date')
Data = merge(Data, ust20y, by.x='Date', by.y='Date')
Data = merge(Data, ust30y, by.x='Date', by.y='Date')

#macroeconomic indicators
Data = merge(Data, capacity, by.x='Date', by.y='Date')
Data = merge(Data, energy, by.x='Date', by.y='Date')
Data = merge(Data, housing, by.x='Date', by.y='Date')
Data = merge(Data, unemployment, by.x='Date', by.y='Date')
Data = merge(Data, manufac, by.x='Date', by.y='Date')
Data = merge(Data, m2, by.x='Date', by.y='Date')
Data = merge(Data, indpro, by.x='Date', by.y='Date')
Data = merge(Data, cpi_monthlygrowth, by.x='Date', by.y='Date')
Data = merge(Data, cpi_yoygrowth, by.x='Date', by.y='Date')
Data = merge(Data, gdp, by.x='Date', by.y='Date')

#converts every column into numeric vector

Data = lapply(Data, as.numeric, margin=2)
write.csv(Data, paste0(datapath, '/appData.csv'))

