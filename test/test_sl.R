#
# period-specific stop loss
#

set.seed(0)
r<-rnorm(300)
rolls<-floor(seq(1,length(r),length.out = 10))


apply_stop<-function(px,rolls=floor(seq(1,length(px),length.out = 10)))data.table(
  tret=c(0,diff(px)),
  px=px
)[,
  c(.SD,list(
    start_px=rep(px[rolls],times=c(diff(rolls),1)),
    roll=rep(rolls,times=c(diff(rolls),1))
  ))
][,
  c(.SD,list(dd=px-start_px))
][,
  c(.SD,list(sl=c(0,head(cummax(dd<(-1)),-1)))),
  keyby=roll
][,
  c(.SD,list(stopped_tret=ifelse(sl>0,0,tret)))
][,
  c(.SD,list(stopped_px=cumsum(stopped_tret)))
]


a<-apply_stop(cumsum(rnorm(300)))

plot(a$px,col=rgb(0,0,0,1),lwd=3,type="l")
lines(a$stopped_px,col=rgb(0,1,0,0.25),lwd=3)
abline(v=sort(unique(a$roll)),col="red")


