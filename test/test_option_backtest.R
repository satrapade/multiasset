require(data.table)
require(scales)
require(magrittr)
require(fasttime)
require(lubridate)
require(scales)
require(fields)


# option backtest

source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/make_date_range.R")

# option models

EC <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  S*pnorm(d1)-X*exp(-r*t)*pnorm(d2)
}

# option models
EP  <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  X*exp(-r*t)*pnorm(-d2)-S*pnorm(-d1)
}


# strikes
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

# a vol surface
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

# timeseries of vol surfaces
hvsurf<-do.call(
  rbind,
  mapply(make_volsurface,i=market_df$pday,MoreArgs=list(market_df=market_df),SIMPLIFY=FALSE)
)

# criteria for roll dates
expiries<-list(
  quote(month %in% c("March","June","September","December")),
  quote(wday=="Friday"),
  quote(wday_count==3)
)

# apply a set of filters to all dates 
make_dates<-function(filter_list,market_df){
  res<-Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=market_df)
  res$start<-res$pday
  res$end<-c(res$pday[-1],nrow(market_df))
  res
}

roll_dates<-make_dates(expiries,market_df)
roll_dates$strike<-roll_dates$px*1.1

# create portfolio 
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


# fetch volatilities required for bilinear interpolation
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



ptf$call<-call.value(ptf$px,ptf$strike,ptf$yfrac,0,ptf$vol)
ptf$put<-put.value(ptf$px,ptf$strike,ptf$yfrac,0,ptf$vol)



# fast resampling



resample_vol_grid<-function (vol_df,strikes,maturities) 
{
  strikes_in<-sort(unique(vol_df$strike))
  maturities_in<-sort(unique(vol_df$maturity))
  j<-data.table::frank(vol_df$strike,ties.method = "dense")
  i<-data.table::frank(vol_df$maturity,ties.method = "dense")
  vol_grid<-sparseMatrix(j=j,i=i,x=vol_df$vol)
  res<-bilinear(
    y=strikes_in,
    x=maturities_in,
    z=vol_grid,
    y0=rep(strikes,times=length(maturities)),
    x0=rep(maturities,each=length(strikes))
  )
  data.table(strike=res$y,maturity=res$x,vol=res$z)
}

vol_df<-hvsurf[date==fastPOSIXct("2015-01-01")]

system.time(for(i in 1:1000){x<-resample_vol_grid(vol_df,strikes,maturities)})
























