require(data.table)
require(scales)
require(magrittr)
require(fasttime)
require(lubridate)
require(scales)

# option backtest

source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/make_date_range.R")

strikes<-seq(from=100,to=500,length.out=100)
maturities<-c(1,7,15,30,45,60,90,120,180)

# dates
make_date_df<-function(start,end){
  make_date_range(start,end) %>% 
  {data.table(
  date_string=.,
  date=fastPOSIXct(.))[,
    c(.SD,list(
      day=day(date),
      mday=mday(date),
      qday=qday(date),
      yday=yday(date),
      pday=seq_along(date),
      wday=weekdays(date),
      month=months(date),
      wday_count=ceiling(mday(date)/7)
    ))
  ]}
}

market_df<- make_date_df("2015-01-01","2018-09-01") %>% 
{
  px<-rescale(cumsum(rnorm(nrow(.))),to=range(strikes[length(strikes)/2+c(-10,10)]))
  .$px<-px
  .
}


make_volsurface<-function(i,market_df){
  date<-market_df$date_string[i]
  vol=runif(1)*0.1+0.1
  volf<-((1-rescale(rep(strikes,time=length(maturities))))+rescale(rep(maturities,each=length(strikes))))/2
  data.table(
    date=fastPOSIXct(date),
    strike=rep(strikes,time=length(maturities)),
    maturity=rep(maturities,each=length(strikes)),
    px=market_df$px[i],
    vol=volf*vol+0.1
  )
}

hvsurf<-do.call(rbind,mapply(make_volsurface,i=market_df$pday,MoreArgs=list(market_df=market_df),SIMPLIFY=FALSE))

expiries<-list(
  quote(month %in% c("March","June","September","December")),
  quote(wday=="Friday"),
  quote(wday_count==3)
)

make_dates<-function(filter_list,market_df){
  res<-Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=market_df)
  res$start<-res$pday
  res$end<-c(res$pday[-1],nrow(market_df))
  res
}

roll_dates<-make_dates(expiries,market_df)
roll_dates$strike<-roll_dates$px*1.1

ptf<-data.table(
  day=market_df$pday,
  date=market_df$date,
  roll=findInterval(market_df$pday,roll_dates$pday,rightmost.closed = FALSE,all.inside = FALSE)
)[roll>0 & roll<nrow(roll_dates)][,c(.SD,list(
  strike=roll_dates$strike[roll],
  days=roll_dates$end[roll]-market_df$pday[day]
))][,c(.SD,list(
  lo_strike=strikes[findInterval(strike,strikes)],
  hi_strike=strikes[findInterval(strike,strikes)+1],
  lo_mat=maturities[findInterval(days,maturities)],
  hi_mat=maturities[findInterval(days,maturities)+1]
))]

ptf$vol_ll<-merge(x=ptf,y=hvsurf,by.x=c("date","lo_strike","lo_mat"),by.y=c("date","strike","maturity"))$vol
ptf$vol_lh<-merge(x=ptf,y=hvsurf,by.x=c("date","lo_strike","hi_mat"),by.y=c("date","strike","maturity"))$vol
ptf$vol_hl<-merge(x=ptf,y=hvsurf,by.x=c("date","hi_strike","lo_mat"),by.y=c("date","strike","maturity"))$vol
ptf$vol_hh<-merge(x=ptf,y=hvsurf,by.x=c("date","hi_strike","hi_mat"),by.y=c("date","strike","maturity"))$vol
ptf$t_strike<-(ptf$strike-ptf$lo_strike)/(ptf$hi_strike-ptf$lo_strike)
ptf$t_mat<-(ptf$days-ptf$lo_mat)/(ptf$hi_mat-ptf$lo_mat)

ptf$vol <-  ptf$vol_ll*(1-ptf$t_strike)*(1-ptf$t_mat) + 
            ptf$vol_hl*(ptf$t_strike)*(1-ptf$t_mat) +
            ptf$vol_lh*(1-ptf$t_strike)*(ptf$t_mat) + 
            ptf$vol_hh*(ptf$t_strike)*(ptf$t_mat)


ptf$px<-merge(x=ptf,y=hvsurf,by.x=c("date","lo_strike","lo_mat"),by.y=c("date","strike","maturity"))$px

ptf$yfrac<-ptf$days/365
ptf$nvol<-ptf$vol*sqrt(ptf$yfrac)
ptf$nmny<-log(ptf$strike/ptf$px)
ptf$Dplus<-(ptf$nmny+(ptf$nvol)^2/2)/ptf$nvol
ptf$Dminus<-(ptf$nmny-(ptf$nvol)^2/2)/ptf$nvol
ptf$premium<-ptf$px*pnorm(ptf$Dplus)-ptf$strike*pnorm(ptf$Dminus)


















