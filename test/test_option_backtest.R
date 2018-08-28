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

# the multi-asset database
dbma<-dbConnect(
odbc::odbc(),
.connection_string = paste0(
"driver={SQL Server};",
"server=SQLS071FP\\MULTIASSET;",
"database=MultiAsset;",
"trusted_connection=true"
))



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
  res$end<-c(res$pday[-1],nrow(market_df))
  res
}

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


resample_vol_grid<-function (vol_df,strikes,maturities) 
{
  strikes_in<-sort(unique(vol_df$Strike))
  maturities_in<-sort(unique(vol_df$Days))
  j<-data.table::frank(vol_df$Strike,ties.method = "dense")
  i<-data.table::frank(vol_df$Days,ties.method = "dense")
  vol_grid<-sparseMatrix(j=j,i=i,x=vol_df$ImpliedVol)
  res<-bilinear(
    y=strikes_in,
    x=maturities_in,
    z=vol_grid,
    y0=rep(strikes,times=length(maturities)),
    x0=rep(maturities,each=length(strikes))
  )
  data.table(Strike=res$y,Days=res$x,ImpliedVol=res$z)
}


on_site<-TRUE

if(on_site){
  spx_vol <- query(make_query(
  security_id="108105",
  query_string = "
    SELECT 
      VOLATILITY_SURFACE_2014.Date AS Date,
      VOLATILITY_SURFACE_2014.Days AS Days,
      VOLATILITY_SURFACE_2014.Strike AS Strike,
      VOLATILITY_SURFACE_2014.Delta AS Delta,
      VOLATILITY_SURFACE_2014.ImpliedVol AS ImpliedVol,
      SECURITY_PRICE.ClosePrice AS ClosePrice
    FROM VOLATILITY_SURFACE_2014
    LEFT JOIN SECURITY_PRICE 
    ON SECURITY_PRICE.SecurityID = VOLATILITY_SURFACE_2014.SecurityID
    AND SECURITY_PRICE.Date = VOLATILITY_SURFACE_2014.Date
    WHERE VOLATILITY_SURFACE_2014.SecurityID = --R{security_id}--
  "),
  db=dbma) %>% {
    x<- .[Delta==80]
    x$Strike<-9999
    y<- .[Delta==(-80)]
    y$Strike<-0
    z<- .[Days==30]
    z$Days<-0
    res<-rbind(.,x,y,z)
    res$Date<-fastPOSIXct(res$Date)
    res
  }
  fwrite(spx_vol,"spx_vol.csv")
} else {
  spx_vol<-fread("spx_vol.csv")
  spx_vol$Date<-fastPOSIXct(spx_vol$Date)
}

maturities<-sort(unique(spx_vol$Days))
new_strikes<-seq(from=1000,to=3000,length.out=50)
resampled_spx_vol<-spx_vol[,resample_vol_grid(.SD,new_strikes,maturities),keyby=Date]

market_df<- make_date_df(as.character(min(spx_vol$Date)),as.character(max(spx_vol$Date))) 


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
)$close*1.1

  
# create portfolio 
ptf<-data.table(
  day=market_df$pday,
  date=market_df$date,
  roll=findInterval(market_df$pday,roll_dates$pday,rightmost.closed = FALSE,all.inside = FALSE)
)[roll>0 & roll<nrow(roll_dates)][,c(.SD,list(
  strike=roll_dates$strike[roll],
  days=roll_dates$end[roll]-market_df$pday[day]
))][,c(.SD,list(
  lo_strike=new_strikes[findInterval(strike,new_strikes)],
  hi_strike=new_strikes[findInterval(strike,new_strikes)+1],
  lo_mat=maturities[findInterval(days,maturities)],
  hi_mat=maturities[findInterval(days,maturities)+1]
))]



ptf$vol_ll<-merge(x=ptf,y=resampled_spx_vol,by.x=c("date","lo_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
ptf$vol_lh<-merge(x=ptf,y=resampled_spx_vol,by.x=c("date","lo_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
ptf$vol_hl<-merge(x=ptf,y=resampled_spx_vol,by.x=c("date","hi_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
ptf$vol_hh<-merge(x=ptf,y=resampled_spx_vol,by.x=c("date","hi_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
ptf$t_strike<-(ptf$strike-ptf$lo_strike)/(ptf$hi_strike-ptf$lo_strike)
ptf$t_mat<-(ptf$days-ptf$lo_mat)/(ptf$hi_mat-ptf$lo_mat)

ptf$vol <-  ptf$vol_ll*(1-ptf$t_strike)*(1-ptf$t_mat) + 
            ptf$vol_hl*(ptf$t_strike)*(1-ptf$t_mat) +
            ptf$vol_lh*(1-ptf$t_strike)*(ptf$t_mat) + 
            ptf$vol_hh*(ptf$t_strike)*(ptf$t_mat)

ptf$yfrac<-ptf$days/365



ptf$EC<-EC(ptf$px,ptf$strike,ptf$yfrac,0,ptf$vol)
ptf$EP<-EP(ptf$px,ptf$strike,ptf$yfrac,0,ptf$vol)



# fast resampling






resampled_spx_vol[Date==fastPOSIXct("2014-01-02")]
spx_vol[Date==fastPOSIXct("2014-01-02")]

















