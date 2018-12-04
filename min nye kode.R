#REcol Gentry, Author: My Kim Nguyen Vo, Student ID: E10704005

#getwd()
#pdf(file = 'gentry_E10704005.pdf',width = 5.90551,height = 5.90551)
load (url ('http://www.davidzeleny.net/wiki/lib/exe/fetch.php?media=recol:data:gentry197.r'))
gentry.coord <- read.delim ('http://www.davidzeleny.net/wiki/lib/exe/fetch.php?media=recol:data:gentry.coord.txt', row.names = 1)

library(RColorBrewer) 
cols<-brewer.pal(n=3,name='Dark2')
library(vegan) 
mean.alpha.diversity <- unlist( lapply(gentry197,function(x) mean(specnumber(x))), use.names = FALSE)

gamma.diversity <- unlist( lapply(gentry197,function(x) sum(specnumber(colSums(x)))), use.names = FALSE)


plot(mean.alpha.diversity~gentry.coord$Lat,data=gentry.coord, ylim=c(min(mean.alpha.diversity),max(gamma.diversity)),
log='y',pch=16,col=cols[2],xlab='Absolute lalitude', ylab= 'Diversity',cex.lab=1.3)

points(gamma.diversity~gentry.coord$Lat,pch=17, col=cols[3])


#regression for mean.alpha.diversity
lm.alpha<-lm(log(mean.alpha.diversity)~gentry.coord$Lat)
predict.alpha<-predict(lm.alpha)
lines(exp(predict.alpha)~gentry.coord$Lat,data=gentry.coord,col=cols[2])

#regression for gamma.diversity 
lm.gamma<-lm(log(gamma.diversity)~gentry.coord$Lat) 
predict.gamma<-predict(lm.gamma)
ymin.max<-c(max(predict.gamma),min(predict.gamma))  
yindex<-match(ymin.max,predict.gamma) 
xmin.max<-gentry.coord$Lat[yindex] 
lines(exp(ymin.max)~xmin.max, lty=2,col=cols[3]) 


