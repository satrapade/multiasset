#
# option backtest
#
#
#

require(data.table)
require(scales)
require(magrittr)
require(fasttime)
require(lubridate)
require(scales)
require(fields)
require(gsubfn)
require(stringi)
require(Matrix)
require(akima)

source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/query.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/sql_tools/make_query.R")
source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/make_date_range.R")


make_market<-function(id="108105",vol_table="VOLATILITY_SURFACE_2014")
{
  dbma<-dbConnect(
    odbc::odbc(),
    .connection_string = paste0(
    "driver={SQL Server};",
    "server=SQLS071FP\\MULTIASSET;",
    "database=MultiAsset;",
    "trusted_connection=true"
  ))
  the_query<-make_query(
    security_id=id,
    the_table=vol_table,
    query_string = "
      SELECT 
        --R{the_table}--.Date AS Date,
        --R{the_table}--.Days AS Days,
        --R{the_table}--.Strike AS Strike,
        --R{the_table}--.Delta AS Delta,
        --R{the_table}--.ImpliedVol AS ImpliedVol,
        SECURITY_PRICE.ClosePrice AS ClosePrice
      FROM --R{the_table}--
      LEFT JOIN SECURITY_PRICE 
      ON SECURITY_PRICE.SecurityID = --R{the_table}--.SecurityID
      AND SECURITY_PRICE.Date = --R{the_table}--.Date
      WHERE --R{the_table}--.SecurityID = --R{security_id}--
  ")
  vol_df<-query(the_query,db=dbma)
  dbDisconnect(dbma)
  x<-vol_df[Delta==80]
  x$Strike<-0
  y<-vol_df[Delta==(-80)]
  y$Strike<-max(y$Strike)*10
  z<-vol_df[Days==30]
  z$Days<-0
  res<-rbind(vol_df,x,y,z)
  res$Date<-fastPOSIXct(res$Date)
  res
}





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

# apply a set of filters to all dates 
make_dates<-function(filter_list,market_df){
  res<-Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=market_df)
  res$start<-res$pday
  res$end<-c(res$pday[-1],max(market_df$pday))
  res
}

# option models

EC <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  S*pnorm(d1)-X*exp(-r*t)*pnorm(d2)
}


EP  <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  X*exp(-r*t)*pnorm(-d2)-S*pnorm(-d1)
}

# resamples volgrid to specified strikes, maturities on a single date
resample_vol_grid<-function (vol_df,strikes,maturities) 
{
  strikes_in<-sort(unique(vol_df$Strike))
  maturities_in<-sort(unique(vol_df$Days))
  j<-data.table::frank(vol_df$Strike,ties.method = "dense")
  i<-data.table::frank(vol_df$Days,ties.method = "dense")
  vol_grid<-t(apply(sparseMatrix(j=j,i=i,x=vol_df$ImpliedVol,use.last.ij=TRUE),1,function(a){
    ndx<-which(a>0)
    approx(x=ndx,y=a[ndx],xout=seq_along(a),yleft=max(a[ndx]),yright=min(a[ndx]))$y
  }))
  res<-bilinear(
    y=strikes_in,
    x=maturities_in,
    z=vol_grid,
    y0=rep(strikes,times=length(maturities)),
    x0=rep(maturities,each=length(strikes))
  )
  data.table(Strike=res$y,Days=res$x,ImpliedVol=res$z,ClosePrice=vol_df$ClosePrice[1])
}


#
# fetch vol surface
#

on_site<-TRUE

if(on_site){
  spx_vol <- make_market(id="108105",vol_table="VOLATILITY_SURFACE_2014")
  fwrite(spx_vol,"spx_vol.csv")
} else {
  spx_vol<-fread("spx_vol.csv")
  spx_vol$Date<-fastPOSIXct(spx_vol$Date)
}

maturities<-sort(unique(spx_vol$Days))
strikes<-seq(from=1000,to=3000,length.out=50)
resampled_spx_vol<-spx_vol[,resample_vol_grid(.SD,strikes,maturities),keyby=Date]




market_df<- make_date_df(
  start=as.character(min(spx_vol$Date)),
  end=as.character(max(spx_vol$Date))
)[date %in% spx_vol$Date]


# criteria for roll dates
expiries<-list(
  quote(month %in% c("March","June","September","December")),
  quote(wday=="Friday"),
  quote(wday_count==3)
)

roll_dates<-make_dates(expiries,market_df)

roll_dates$strike<-merge(
  x=roll_dates,
  y=spx_vol[,.(close=ClosePrice[1]),keyby=Date],
  by.x=c("date"),
  by.y=c("Date")
)$close*0.95




  
# create portfolio 
ptf<-data.table(
  day=market_df$pday,
  date=market_df$date,
  roll=findInterval(market_df$pday,roll_dates$pday,rightmost.closed = FALSE,all.inside = FALSE)
)[roll>0 & roll<nrow(roll_dates)][,c(.SD,list(
  strike=roll_dates$strike[roll],
  start=roll_dates$start[roll],
  end=roll_dates$end[roll],
  days=roll_dates$end[roll]-day
))]


interpolate_vol<-function(ptf,vsurf)
{
  strikes<-sort(unique(vsurf$Strike))
  maturities<-sort(unique(vsurf$Days))
  option_dets<-data.table(
    date=ptf$date,
    lo_strike=strikes[findInterval(ptf$strike,strikes)],
    hi_strike=strikes[findInterval(ptf$strike,strikes)+1],
    lo_mat=maturities[findInterval(ptf$days,maturities)],
    hi_mat=maturities[findInterval(ptf$days,maturities)+1]
  )
  vol_ll<-merge(x=option_dets,y=vsurf,by.x=c("date","lo_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_lh<-merge(x=option_dets,y=vsurf,by.x=c("date","lo_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_hl<-merge(x=option_dets,y=vsurf,by.x=c("date","hi_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_hh<-merge(x=option_dets,y=vsurf,by.x=c("date","hi_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  t_strike<-(ptf$strike-option_dets$lo_strike)/(option_dets$hi_strike-option_dets$lo_strike)
  t_mat<-(ptf$days-option_dets$lo_mat)/(option_dets$hi_mat-option_dets$lo_mat)
  rowSums(cbind(
    vol_ll*(1-t_strike)*(1-t_mat),
    vol_hl*(t_strike)*(1-t_mat),
    vol_lh*(t_strike)*(t_mat),
    vol_hh*(t_strike)*(t_mat)
  ))
}




ptf$vol <-  interpolate_vol(ptf,resampled_spx_vol)

ptf$yfrac<-ptf$days/365

ptf$close<-resampled_spx_vol[Date %in% ptf$date,ClosePrice[1],keyby=Date][[2]]


ptf$EC<-EC(ptf$close,ptf$strike,ptf$yfrac,0,ptf$vol)
ptf$EP<-EP(ptf$close,ptf$strike,ptf$yfrac,0,ptf$vol)









