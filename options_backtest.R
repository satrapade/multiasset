
#
# backtest components
#
# 1. schedule
# 2. market data (close, atm vol)
# 3. strikes
# 4. reval vols
#

source("https://raw.githubusercontent.com/satrapade/utility/master/utility_functions.R")

require(data.table)
require(scales)
require(RcppRoll)
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

resampled_spx_vol<-"compressed_resampled_spx_vol.txt" %>% scan(character()) %>% decompress
resampled_ndx_vol<-"compressed_resampled_ndx_vol.txt" %>% scan(character()) %>% decompress
resampled_dax_vol<-"compressed_resampled_dax_vol.txt" %>% scan(character()) %>% decompress
resampled_sx5e_vol<-"compressed_resampled_sx5e_vol.txt" %>% scan(character()) %>% decompress

all_vol<-rbind( # data.table with all volatilities
  data.table(resampled_spx_vol,market="spx"),
  data.table(resampled_ndx_vol,market="ndx"),
  data.table(resampled_dax_vol,market="dax"),
  data.table(resampled_sx5e_vol,market="sx5e")
)[,c(.SD,list(market_count=length(market))),keyby=Date]

listed_expiries<-list(
    quote(month %in% c("March","June","September","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
)


#
# get common dates for all markets 
# in volsurface table
make_common_dates<-function(vsurfs){
  vsurfs[
    ,.(date_string=stri_sub(Date[1],1,10)),
    keyby=c("market","Date")
  ][
    ,.(markets=length(market)),keyby=date_string
  ][
    markets==max(markets),as.Date(date_string,format="%Y-%m-%d")
  ]
}


#
# make a shedule from a list of volatility surfaces
make_shedule<-function(volsurfs)
{
  common_dates<-make_common_dates(volsurfs)
  markets<-sort(unique(volsurfs$market))
  shedule_df <- data.table(
    date=rep(common_dates,times=length(markets)),
    market=rep(markets,each=length(common_dates))
  )
  shedule_df
}

# create a roll schedule from a shedule
make_roll_dates<-function(filter_list,vsurfs){
  
  shedule<-make_shedule(vsurfs)
  
  date_df<-data.table(
    date=shedule$date,
    day=day(shedule$date),
    mday=mday(shedule$date),
    qday=qday(shedule$date),
    yday=yday(shedule$date),
    pday=seq_along(shedule$date),
    wday=weekdays(shedule$date),
    month=months(shedule$date),
    wday_count=ceiling(mday(shedule$date)/7)
  )[,.SD,keyby=date]
  
  res0<-Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=date_df)
  res1<-res0[,.SD,keyby=date]
  ndx<-sort(unique(res1$date))
  res2<-res1[J(ndx),.SD,mult="first"]
  res3<-data.table(
    res2,
    start=shedule$date[res2$pday],
    end=shedule$date[c(res2$pday[-1],nrow(shedule))],
    roll=1:nrow(res2)
  )
  shedule_with_roll<-data.table(shedule,roll=findInterval(shedule$date,res3$date))
  res4<-res3[,.SD,keyby=roll][shedule_with_roll[,.SD,keyby=roll]][roll>0][,.(
    roll=roll,
    date=i.date,
    start=start,
    end=end,
    market=market,
    maturity=as.integer(end-i.date)
  )]
  px<-all_vol[,.(
      date=as.Date(stri_sub(Date[1],1,10),format="%Y-%m-%d"),
      close=ClosePrice[1],
      vol=mean(ImpliedVol)
  ),keyby=c("market","Date"),][,.(date,market,close,vol)]
  
  res5<-px[,.SD,keyby=c("date","market")][res4[,.SD,keyby=c("date","market")]]
  
  res5
}



shedule<-make_roll_dates(
  filter_list=listed_expiries,
  vsurfs=all_vol
)


shedule %>% 
  ggplot() +
  geom_line(aes(x=date,y=close,col=market))

shedule %>% 
  ggplot() +
  geom_line(aes(x=date,y=vol,col=market))



