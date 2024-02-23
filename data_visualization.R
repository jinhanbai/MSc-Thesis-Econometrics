source('data_aggregate.R')

Data = data.frame(Data)
Data$Date = as.Date(Data$Date)
#closing price plot
ndx_price_plot = ggplot(data=Data,
                        mapping=aes(x=Date, y=ndx_price))
ndx_price_plot +
  geom_path(color='blue') +
  labs(x='Date', y='$',
       title='NASDAQ 100 Index Prices') +
  xlim(Data$Date[1], Data$Date[length(Data$Date)])
  
#loss plot
ndx_loss_plot = ggplot(data=Data,
                       mapping=aes(x=Date, y=Y_ndx_loss))
ndx_loss_plot +
  geom_path(color='blue') +
  labs(x='Date', y='%',
       title='NASDAQ 100 Loss') +
  xlim(Data$Date[1], Data$Date[length(Data$Date)])

#loss distribution plot
ndx_dist = ggplot(data=Data,
                  mapping=aes(x=Y_ndx_loss))
ndx_dist + 
  geom_histogram(aes(y=after_stat(density)), binwidth=0.01) +
  geom_density(colour='blue') +
  labs(x='%', y='Density',
       title='NASDAQ 100 Loss Distribution')

## descriptive statistics
df = subset(Data, select=-c(Date, ndx_price))
meanStat = data.frame('Mean'=sapply(df, mean, margin=2))
medStat = data.frame('Median'=sapply(df, median, margin=2))
stdStat = data.frame('Std.Dev.'=sapply(df, sd))
minStat = data.frame('Min.'=sapply(df, min, margin=2))
maxStat = data.frame('Max.'=sapply(df, max, margin=2))
skewStat = data.frame('Skewness'=sapply(df, skewness))
kurtStat = data.frame('Kurtosis'=sapply(df, kurtosis))

statDf = cbind(meanStat, medStat)
statDf = cbind(statDf, stdStat)
statDf = cbind(statDf, minStat)
statDf = cbind(statDf, maxStat)
statDf = cbind(statDf, skewStat)
statDf = cbind(statDf, kurtStat)
write.csv(statDf,paste0(datapath, '/descriptiveStats.csv'))

