datapath = 'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/DGPs/Application/'


daily_path = paste0(datapath, "DAILY_RAW/")


calc_return <- function(priceVec){
  priceVec = as.numeric(priceVec)
  returnVec = rep(NA, length(priceVec))
  for (i in 2:length(priceVec)){
    returnVec[i] = ((priceVec[i]-priceVec[i-1])/priceVec[i-1])*100
  }
  return(returnVec)
}


##################### INDICES
#NASDAQ100 Series (ndx) = Y
ndx = read_excel(path=paste0(daily_path,'NDX.xlsx'))
ndx$Date = as.Date(ndx$Date)
ndx = ndx[order(ndx$Date),]
ndx = na.omit(ndx)
ndx$ndx_loss = -calc_return(ndx$ndx)
ndx$ndx_loss_l1 = lag(ndx$ndx_loss, n=1)
ndx$ndx_loss_l2 = lag(ndx$ndx_loss, n=2)
ndx = na.omit(ndx)
#ndx = subset(ndx, select=-ndx)

#ndx volume
ndx_vol = read.csv(file=paste0(daily_path,'NDXVol.csv'))
ndx_vol = data.frame('Date'=as.Date(ndx_vol$Date), 'ndx_vol'=ndx_vol$Volume)

#AEX
aex = read_excel(path=paste0(daily_path,'AEX.xlsx'))
aex$Date = as.Date(aex$Date)
aex = aex[order(aex$Date),]
aex$aex_return = calc_return(aex$aex)
aex = na.omit(aex)
aex = data.frame('Date'=aex$Date, 'aex_return'=aex$aex_return)

#DAX 
gdaxi = read_excel(path=paste0(daily_path,'DAX.xlsx'))
gdaxi$Date = as.Date(gdaxi$Date)
gdaxi = gdaxi[order(gdaxi$Date),]
gdaxi$gdaxi_return = calc_return(gdaxi$gdaxi)
gdaxi = na.omit(gdaxi)
gdaxi = data.frame('Date'=gdaxi$Date, 'gdaxi_return'=gdaxi$gdaxi_return)

#Hangseng
hsi = read_excel(path=paste0(daily_path,'HSI.xlsx'))
hsi$Date = as.Date(hsi$Date)
hsi = hsi[order(hsi$Date),]
hsi$hsi_return = calc_return(hsi$hsi)
hsi = na.omit(hsi)
hsi = data.frame('Date'=hsi$Date, 'hsi_return'=hsi$hsi_return)

#topx (tokyo stock exchange)
topx = read_excel(path=paste0(daily_path,'TOPX.xlsx'))
topx$Date = as.Date(topx$Date)
topx = topx[order(topx$Date),]
topx$topx_return = calc_return(topx$topx)
topx = na.omit(topx)
topx = data.frame('Date'=topx$Date, 'topx_return'=topx$topx_return)

#Russel 2000
rut = read_excel(path=paste0(daily_path,'RUT2000.xlsx'))
rut$Date = as.Date(rut$Date)
rut = rut[order(rut$Date),]
rut$rut_return = calc_return(rut$Rut)
rut = na.omit(rut)
rut = data.frame('Date'=rut$Date, 'rut_return'=rut$rut_return)

#ssec
ssec = read_excel(path=paste0(daily_path,'SSEC.xlsx'))
ssec$Date = as.Date(ssec$Date)
ssec = ssec[order(ssec$Date),]
ssec$ssec_return = calc_return(ssec$ssec)
ssec = na.omit(ssec)
ssec = data.frame('Date'=ssec$Date, 'ssec_return'=ssec$ssec_return)

#rts
irts = read_excel(path=paste0(daily_path,'RTS.xlsx'))
irts$Date = as.Date(irts$Date)
irts = irts[order(irts$Date),]
irts$irts_return = calc_return(irts$irts)
irts = na.omit(irts)
irts = data.frame('Date'=irts$Date, 'irts_return'=irts$irts_return)

#Euronext100 
n100 = read_excel(path=paste0(daily_path,'N100.xlsx'))
n100$Date = as.Date(n100$Date)
n100 = n100[order(n100$Date),]
n100$n100_return = calc_return(n100$n100)
n100 = na.omit(n100)
n100 = data.frame('Date'=n100$Date, 'n100_return'=n100$n100_return)

#VIX
vix = read.csv(file=paste0(daily_path, 'vix.csv'))
vix = vix %>% filter(Volume != 'null')
vix$Date = as.Date(vix$Date)
vix$vix_return = calc_return(vix$Close)
vix = data.frame('Date'=vix$Date, 'vix_return'=vix$vix_return)


# ################# FX rates

#US Dollar currency index DXY
dxy = read.csv(file=paste0(daily_path, 'DXY.csv'), sep=';')
dxy$Date = as.character(dxy$Date)
dxy = separate(dxy, Date, sep='-', c('M','D', 'Y'))
dxy$M = as.numeric(dxy$M)
dxy$D = as.numeric(dxy$D)
dxy$Y = as.numeric(dxy$Y)
dxy$Date = as.Date(ISOdate(dxy$Y, dxy$M, dxy$D))
dxy = dxy[order(dxy$Date),]
dxy = na.omit(dxy)
dxy$dxy_return = calc_return(dxy$Price)
dxy = na.omit(dxy)
dxy = data.frame('Date'=dxy$Date, 'dxy_return'= dxy$dxy_return)

################# FAMA FRENCH
fama = read.csv(file=paste0(daily_path, 'famafrench.csv'))
fama$Date = as.Date(fama$date)
fama = subset(fama, select=-date)
temp = fama[, 1:7]*100
fama[,1:7] = temp

################ Commodities

#WTI crude oil front month futures returns
oil = read.csv(file=paste0(daily_path, 'WTICRUDEOIL.csv'))
oil = oil %>% filter(DCOILWTICO != '.')
oil$Date = as.Date(oil$DATE)
oil$oil_return = calc_return(oil$DCOILWTICO)
oil = na.omit(oil)
oil = data.frame('Date'=oil$Date, 'oil_return'=oil$oil_return)


#GCc1 Gold front month futures returns
gold = read.csv(file=paste0(daily_path, 'GCc1Gold.csv'), sep=';')
gold$Date = as.Date(gold$Date, format='%d-%m-%Y')
gold = gold[order(gold$Date),]
gold = gold[gold$Date >= '2001-01-01',]
gold$gold_return = calc_return(gold$Price)
gold = na.omit(gold)
gold = data.frame('Date'=gold$Date, 'gold_return'=gold$gold_return)


#Wc1 Wheat front month futures return
wheat = read.csv(file=paste0(daily_path, 'Wc1Wheat.csv'), sep=';')
wheat$Date = as.Date(wheat$Date, format='%d-%m-%Y')
wheat = wheat[order(wheat$Date),]
wheat = wheat[wheat$Date >= '2001-01-01',]
wheat$wheat_return = calc_return(wheat$Price)
wheat = na.omit(wheat)
wheat = data.frame('Date'=wheat$Date, 'wheat_return'=wheat$wheat_return)


#Slc1 Silver front month futures returns
silver = read.csv(file=paste0(daily_path, 'SIc1Silver.csv'), sep=';')
silver$Date = as.Date(silver$Date, format='%d-%m-%Y')
silver = silver[order(silver$Date),]
silver = silver[silver$Date >= '2001-01-01',]
silver$silver_return = calc_return(silver$Price)
silver = na.omit(silver)
silver = data.frame('Date'=silver$Date, 'silver_return'=silver$silver_return)


#NGc1 Natural gas front month futures return
natgas = read.csv(file=paste0(daily_path, 'NGc1NaturalGas.csv'), sep=';')
natgas$Date = as.Date(natgas$Date, format='%d-%m-%Y')
natgas = natgas[order(natgas$Date),]
natgas = natgas[natgas$Date >= '2001-01-01',]
natgas$natgas_return = calc_return(natgas$Price)
natgas = na.omit(natgas)
natgas = data.frame('Date'=natgas$Date, 'natgas_return'=natgas$natgas_return)


#KCc1 Coffee front month futures return
coffee = read.csv(file=paste0(daily_path, 'KCc1Coffee.csv'), sep=';')
coffee$Date = as.Date(coffee$Date, format='%d-%m-%Y')
coffee = coffee[order(coffee$Date),]
coffee = coffee[coffee$Date >= '2001-01-01',]
coffee$coffee_return = calc_return(coffee$Price)
coffee = na.omit(coffee)
coffee = data.frame('Date'=coffee$Date, 'coffee_return'=coffee$coffee_return)

###################### Yield rates/ bonds
#libor rates 1m, 3m and 6m
libor = read.csv(file=paste0(daily_path, 'LIBOR.csv'), sep=';')
libor$Date = as.Date(libor$Date)
libor = libor[order(libor$Date),]
libor = data.frame('Date'=libor$Date, 
                   'libor_1m'=libor$X1M,
                   'libor_3m'=libor$X3M,
                   'libor_6m'=libor$X6M)

#federal funds effective rates
fedrate = read.csv(file=paste0(daily_path, 'FEDERALFUNDSEFFECTIVE.csv'))
fedrate$Date = as.Date(fedrate$Date)


#spread BAA10Y: Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity (measure of default risk)
baa10y = read.csv(file=paste0(daily_path, 'SPREADBAA10Y.csv'))
baa10y = baa10y %>% filter(baa10y != '.')
baa10y$Date = as.Date(baa10y$Date)


#us treasury 1 month
ust1m = read.csv(file=paste0(daily_path, 'USTYIELD1M.csv'))
ust1m = ust1m %>% filter(ust1m != '.')
ust1m$Date = as.Date(ust1m$Date)


#us treasury 3 month
ust3m = read.csv(file=paste0(daily_path, 'USTYIELD3M.csv'))
ust3m = ust3m %>% filter(ust3m != '.')
ust3m$Date = as.Date(ust3m$Date)


#us treasury 1 year
ust1y = read.csv(file=paste0(daily_path, 'USTYIELD1Y.csv'))
ust1y = ust1y %>% filter(ust1y != '.')
ust1y$Date = as.Date(ust1y$Date)


#us treasury 2 year
ust2y = read.csv(file=paste0(daily_path, 'USTYIELD2Y.csv'))
ust2y = ust2y %>% filter(ust2y != '.')
ust2y$Date = as.Date(ust2y$Date)


#us treasury 5 year
ust5y = read.csv(file=paste0(daily_path, 'USTYIELD5Y.csv'))
ust5y = ust5y %>% filter(ust5y != '.')
ust5y$Date = as.Date(ust5y$Date)


#us treasury 10 year
ust10y = read.csv(file=paste0(daily_path, 'USTYIELD10Y.csv'))
ust10y = ust10y %>% filter(ust10y != '.')
ust10y$Date = as.Date(ust10y$Date)


#us treasury 20 year
ust20y = read.csv(file=paste0(daily_path, 'USTYIELD20Y.csv'))
ust20y = ust20y %>% filter(ust20y != '.')
ust20y$Date = as.Date(ust20y$Date)


#us treasury 30 year
ust30y = read.csv(file=paste0(daily_path, 'USTYIELD30Y.csv'))
ust30y = ust30y %>% filter(ust30y != '.')
ust30y$Date = as.Date(ust30y$Date)

