datapath = 'C:/Users/baiji/Documents/MSc Econometrics/Master Thesis Tail Risk RF Code/DGPs/Application/'

quarterly_path = paste0(datapath, "QUARTERLY_RAW/")

calcQuarterlyGrowth <- function(index){
  index = as.numeric(index)
  res = rep(NA, length(index))
  for (i in 2:length(index)){
    res[i] = ((index[i]-index[i-1])/index[i-1])*100
  }
  return(res)
}
calcYoYGrowth_Quarter <- function(index){
  index = as.numeric(index)
  res = rep(NA, length(index))
  for (i in 5:length(index)){
    res[i] = ((index[i]-index[i-4])/index[i-4])*100
  }
  return(res)
}

gdp = read.csv(file=paste0(quarterly_path, 'GDP.csv'))
gdp$Date = as.Date(gdp$Date) + months(3)
gdp$gdp_quarterly = calcQuarterlyGrowth(gdp$gdp)
gdp$gdp_yoy = calcYoYGrowth_Quarter(gdp$gdp)
gdp = na.omit(gdp)

gdp_daily_date = seq(min(gdp$Date), max(gdp$Date), by='days')
gdp = data.frame(gdp %>% 
  complete(Date = gdp_daily_date) %>%
  fill(gdp_quarterly, .direction='down'))
gdp = data.frame(gdp %>% 
  complete(Date = gdp_daily_date) %>%
  fill(gdp_yoy, .direction='down'))
gdp = subset(gdp, select=-gdp)

