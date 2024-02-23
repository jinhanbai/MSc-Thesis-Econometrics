setwd("~/MSc Econometrics/Master Thesis Tail Risk RF Code")
source('runfile.r')
dgp = 'C'
p = 0.995
q = 0.9
d = 30
filepath = paste0(getwd(), '/Results/ISRE/', dgp, '/ISRE_DGP',dgp, 'p', 
                 as.character(p),'q', as.character(q), 'd', as.character(d),
                 'm50.csv')

res = read.csv(file=filepath)
res = subset(res, select=-ISRE_ecf)
colnames(res) = c('EMF', 'ERF')
res$ind = row.names(res)
res = melt(res, id='ind')
colnames(res) = c('ind', 'Method', 'ISRE')

plot = ggplot(res, aes(x=Method, y=ISRE, fill=Method))
plot + geom_boxplot() + 
  labs(title=paste0('DGP ', dgp, ' & p = ', as.character(p))) +
  theme(legend.position='none')

