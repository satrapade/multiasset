


xm <- matrix(rnorm(N),ncol=10)
dts <- seq(from=Sys.Date(),length.out=10,by="days")
x<- fts(index=dts,data=xm)




x <- fts(index=seq(from=Sys.Date(),by="days",length.out=100),data=1:100)
y <- fts(index=seq(from=Sys.Date(),by="days",length.out=100),data=1:100)
x.mean <- moving.mean(x,20)
x.sum <- moving.sum(x,20)
x.prod <- moving.product(x,20)
x.max <- moving.max(x,20)
x.min <- moving.min(x,20)
x.sd <- moving.sd(x,20)
x.rank <- moving.rank(x,20)


