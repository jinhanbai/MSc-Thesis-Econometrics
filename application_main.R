setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')


datapath = getwd()
Data = read.csv(file=paste0(datapath,'/DGPs/Application/appData.csv'))
Data = subset(Data, select=-c(X, ndx_price))
Data$Date = as.Date(Data$Date)


# 70/30 train/test split
trainfrac = 0.7

train_Data = Data[1:round(trainfrac*nrow(Data)),]
train_Data = subset(train_Data, select=-Date)
colnames(train_Data)[1]= 'Y'

test_Data = Data[(round(trainfrac*nrow(Data))+1):nrow(Data),]
test_Data = subset(test_Data, select=-Date)
colnames(test_Data)[1]='Y'


#define running parameters
p = 0.995

q_vec = c(0.8)

for (q in q_vec){
  u = sort(train_Data$Y, decreasing = FALSE)[ceiling(q*length(train_Data$Y))]
  run_app(p, q, u, train_Data, test_Data)
}





