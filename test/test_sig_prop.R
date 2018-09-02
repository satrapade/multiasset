


signal<-runif(100)>0.5
period<-sort(sample(1:5,100,replace=TRUE))

x<-cbind(signal)[,rep(1,5)]
y<-cbind(period)[,rep(1,5)]
z<-rbind(1:5)[rep(1,100),]
colSums(x*(y==z))
mapply(sum,split(signal,period))


