library(gdata)

unemp = read.csv("unemp.csv")
unempstates = read.csv("unempstates.csv")
View(unemp)

plot(unempstates[,10],type="l",xlab="month",ylab="unemployment rate",col="red")
View(unemp)
View(unempstates)
plot(unemp$mean,unemp$stddev)
unempstates = t(unempstates)

#on 416 dimensions
set.seed(1)
grpunemp <- kmeans(unempstates, centers=3, nstart=10)
o=order(grpunemp$cluster)
d1 = data.frame(unemp$state[o],grpunemp$cluster[o])




#on 2 dimensions
set.seed(1)
grpunemp1 <- kmeans(unemp[,c("mean","stddev")], centers=3, nstart=10)
o1 = order(grpunemp1$cluster)
d2 = data.frame(unemp$state[o1],grpunemp1$cluster[o1])
plot(unemp$mean,unemp$stddev,type="n",xlab="mean", ylab="stddev")
text(x=unemp$mean,y=unemp$stddev,labels=unemp$state, col=grpunemp1$cluster+1)

View(d1)
View(d2)
