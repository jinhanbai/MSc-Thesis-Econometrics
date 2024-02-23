setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')

data = read.csv(file=paste0(getwd(), '/DGPs/Application/appData.csv'))
data = data[, -c(1,2)]
data = subset(data, select=-c(ndx_price))

#pearson correlation matrix

corrplot(cor(data, method='pearson'), method='color')

#fit regression forest on entire dataset
Y_all = data$Y_ndx_loss
X_all = subset(data, select = -c(Y_ndx_loss))
varnames = colnames(X_all)
forestAllFit = randomForest::randomForest(x=X_all, y=Y_all, ntree=500, importance=TRUE)
varImpPlot(forestAllFit, sort=TRUE, type=1, 
           main='NASDAQ 100 All Observations')
varImpPlot(forestAllFit, sort=TRUE, type=2, 
           main='NASDAQ 100 All Observations')


# allImp = t(data.frame(grf::variable_importance(forestAllFit)))
# colnames(allImp) = varnames
# allImp = allImp[, order(-allImp[1,])]
# histogram(allImp)


#subset tail observations of the data and generate variable importance
tailfrac = 0.1
t = sort(Y_all, decreasing=FALSE)[ceiling((1-tailfrac)*length(Y_all))]
largelossdat = data[data$Y_ndx_loss>=t,]
Y_tail = log(largelossdat$Y_ndx_loss/t)
X_tail = subset(largelossdat, select=-c(Y_ndx_loss))
forestTailFit = randomForest::randomForest(x=X_tail, y=Y_tail, ntree=100, importance=TRUE)
varImpPlot(forestTailFit, sort=TRUE, type=1,
           main=paste0('Extreme Value Index Top ', as.character(100*tailfrac), '% Largest Losses'))
varImpPlot(forestTailFit, sort=TRUE, type=2,
           main=paste0('Extreme Value Index Top ', as.character(100*tailfrac), '% Largest Losses'))

# tailImp = t(data.frame(grf::variable_importance(forestTailFit)))
# colnames(tailImp) = varnames
# tailImp = tailImp[, order(-tailImp[1,])]

