#
# option backtest
#
# volatility_surface -
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
require(ggplot2)

source("https://raw.githubusercontent.com/satrapade/utility/master/utility_functions.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/query.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/sql_tools/make_query.R")
source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/make_date_range.R")

#
# spx, ndx, sx5e, dax, ftse, nky
#
spx_id<-"108105"
ndx_id<-"102480"
sx5e_id<-"504880"
dax_id<-"506496"
ftse_id<-"506528"
nky_id<-"902278"
eem_id<-"116959"

market_id<-nky_id
fname<-"nky"

make_market<-function(id=sx5e_id,vol_table="VOLATILITY_SURFACE_2014")
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



# apply a set of filters to all dates 
make_strategy_roll_dates<-function(filter_list,strategy_df){
  res<-Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=strategy_df)
  res$start<-res$pday
  res$end<-c(res$pday[-1],max(strategy_df$pday))
  res
}


# date and 
make_strategy_df<-function(
  volsurf,
  filter_list=expiries<-list(
    quote(month %in% c("March","June","September","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
  )
)
{
  strategy_df <- volsurf[,.(
    date=Date[1],
    date_string=as.character(Date[1],format="%Y-%m-%d"),
    close=ClosePrice[1]
  ),keyby=Date][,.(
    date=date,
    date_string=date_string,
    close=close
  )]
  strategy_df$day <- day(strategy_df$date)
  strategy_df$mday <- mday(strategy_df$date)
  strategy_df$qday <- qday(strategy_df$date)
  strategy_df$yday <- yday(strategy_df$date)
  strategy_df$pday <- seq_along(strategy_df$date)
  strategy_df$wday <- weekdays(strategy_df$date)
  strategy_df$month <- months(strategy_df$date)
  strategy_df$wday_count <- ceiling(mday(strategy_df$date)/7)
  setkey(strategy_df,date_string)
  roll_dates<-make_strategy_roll_dates(filter_list,strategy_df)
  strategy_df$roll<-findInterval(
    strategy_df$pday,
    roll_dates$pday,
    rightmost.closed = FALSE,
    all.inside = FALSE
  )
  strategy_df<-strategy_df[roll>0 & roll<nrow(roll_dates)]
  strategy_df$roll_close<-roll_dates$close[strategy_df$roll]
  strategy_df$start<-roll_dates$start[strategy_df$roll]
  strategy_df$end<-roll_dates$end[strategy_df$roll]
  strategy_df$days<-roll_dates$end[strategy_df$roll]-strategy_df$pday
  strategy_df$yfrac<-strategy_df$days/365
  strategy_df
}



# resamples volgrid to specified strikes, maturities on a single date
resample_vol_grid<-function (vol_df,strikes,maturities,verbose=FALSE) 
{
  if(verbose){
    the_date<-as.character(vol_df$Date[1],format="%Y-%m-%d")
    cat(the_date,"\n")
  }
  vol_df<-vol_df[ImpliedVol>0]
  if(nrow(vol_df)<1)return(NULL)
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
# interpolate vols
# on resampled vol surface
#
interpolate_vol<-function(date,strike,days,vsurf)
{
  strikes<-sort(unique(vsurf$Strike))
  maturities<-sort(unique(vsurf$Days))
  option_dets<-data.table(
    date=date,
    lo_strike=strikes[findInterval(strike,strikes)],
    hi_strike=strikes[findInterval(strike,strikes)+1],
    lo_mat=maturities[findInterval(days,maturities)],
    hi_mat=maturities[findInterval(days,maturities)+1]
  )
  vol_ll<-merge(x=option_dets,y=vsurf,by.x=c("date","lo_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_lh<-merge(x=option_dets,y=vsurf,by.x=c("date","lo_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_hl<-merge(x=option_dets,y=vsurf,by.x=c("date","hi_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_hh<-merge(x=option_dets,y=vsurf,by.x=c("date","hi_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  t_strike<-(strike-option_dets$lo_strike)/(option_dets$hi_strike-option_dets$lo_strike)
  t_mat<-(days-option_dets$lo_mat)/(option_dets$hi_mat-option_dets$lo_mat)
  rowSums(cbind(
    vol_ll*(1-t_strike)*(1-t_mat),
    vol_hl*(t_strike)*(1-t_mat),
    vol_lh*(1-t_strike)*(t_mat),
    vol_hh*(t_strike)*(t_mat)
  ))
}

#
#
#

make_option_pnl<-function(
  model=EP,
  strike=0.9,
  dir=1,
  strategy,
  vsurf
)
{
  strikes<-strategy$roll_close*strike
  vols<-interpolate_vol(date=strategy$date,strike=strikes,days=strategy_df$days,vsurf=vsurf)
  reval<-model(strategy$close, strikes, strategy$yfrac, 0,vols)
  cash_start<-ifelse(strategy$pday==strategy$start,-reval,0)
  cash_end<-ifelse(strategy$pday==strategy$end-1,reval,0)
  data.table(
    strike=strikes,
    yfrac=strategy$yfrac,
    vol=vols,
    close=strategy$close,
    reval=round(dir*ifelse(strategy$pday==strategy$end-1,0,reval),digits=3),
    cash_start=round(dir*cash_start,digits=3),
    cash_end=round(dir*cash_end,digits=3),
    cash=round(dir*cumsum(cash_start+cash_end),digits=3),
    pnl=round(dir*(ifelse(strategy$pday==strategy$end-1,0,reval)+cumsum(cash_start+cash_end)),digits=3)
  )
}


################################################################################

#
# 1. fetch vol surface
# 2. resample vol surface
# 3. create strategy blotter
# 4. compute option values
#

on_site<-TRUE

if(on_site){
  market_vol <- make_market(id=market_id,vol_table="VOLATILITY_SURFACE_ALL")
  fwrite(market_vol,paste0(fname,"_vol.csv"))
} else {
  market_vol<-fread(paste0(fname,"_vol.csv"))
  market_vol$Date<-fastPOSIXct(market_vol$Date)
}

#
# resample vol surface to common set of strikes
#

maturities<-sort(unique(market_vol$Days))
strikes<-round(seq(from=150,to=40000,length.out=50),digits=0)
resampled_market_vol<-market_vol[,resample_vol_grid(data.table(Date=Date,.SD),strikes,maturities,TRUE),keyby=Date]


#
# make strategy data frame
#
strategy_df<- make_strategy_df(
  volsurf=resampled_market_vol,
  filter_list=list(
    quote(month %in% c("March","June","September","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
  )
)

fwrite(strategy_df,paste0(fname,"_strategy_df.csv"))
fwrite(resampled_market_vol,paste0("resampled_",fname,"_vol.csv"))
write(compress(fread(paste0("resampled_",fname,"_vol.csv"))),paste0("compressed_resampled_",fname,"_vol.txt"))

strategy_df<-fread("strategy_df.csv")
resampled_market_vol<- paste0("compressed_resampled_",fname,"_vol.txt") %>% scan(character()) %>% decompress

#
# compute option pnls

system.time(strategy_pnl<-data.table(
  date=fastPOSIXct(strategy_df$date),
  pnl=rowSums(cbind(
    make_option_pnl(model=EP,strike=1.0,dir=-1,strategy=strategy_df,vsurf=resampled_market_vol)$pnl,
    make_option_pnl(model=EP,strike=0.90,dir=+1,strategy=strategy_df,vsurf=resampled_market_vol)$pnl
  ))
))

strategy_pnl %>%
  ggplot() +
  geom_line(aes(x=date,y=pnl))
  

