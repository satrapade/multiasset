

x<-data.table(
  day=rep(seq(from=1,by=3,length.out=15),each=3),
  strike=rep(seq(from=1,to=3),times=15),
  v=runif(15*3)
)

x1<-min(x[strike==1][,day])
x2<-max(x[strike==1][,day])

i<-findInterval(seq(from=x1,to=x2),x[strike==1,day])

data.table(
  from=x[strike==1,day][i],
  to=seq(x1,x2)
)
