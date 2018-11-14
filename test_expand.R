

x<-data.table(
  day=rep(seq(from=1,by=3,length.out=15),each=3),
  market=rep(seq(from=1,to=3),times=15),
  v=runif(15*3)
)

min(x[market==1][,day])
max(x[market==1][,day])

y<-data.table(
  i=seq(from=min(x$i),to=max(x$i)),
  j=findInterval(seq(from=min(x$i),to=max(x$i)))
)





