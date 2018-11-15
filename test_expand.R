require(magrittr)
require(brotli)
source("https://raw.githubusercontent.com/satrapade/utility/master/utility_functions.R")

resampled_spx_vol<-"compressed_resampled_spx_vol.txt" %>% scan(character()) %>% decompress
resampled_sx5e_vol<-"compressed_resampled_sx5e_vol.txt" %>% scan(character()) %>% decompress
resampled_ftse_vol<-"compressed_resampled_ftse_vol.txt" %>% scan(character()) %>% decompress
resampled_nky_vol<-"compressed_resampled_nky_vol.txt" %>% scan(character()) %>% decompress
#resampled_eem_vol<-"compressed_resampled_eem_vol.txt" %>% scan(character()) %>% decompress

all_vol<-rbind( # data.table with all volatilities
  data.table(resampled_spx_vol,market="spx"),
  data.table(resampled_sx5e_vol,market="sx5e"),
  data.table(resampled_ftse_vol,market="ftse"),
  data.table(resampled_nky_vol,market="nky")
)[,.(
    Date=as.Date(stri_sub(Date,1,10),format="%Y-%m-%d"),
    Strike=Strike,
    Days=Days,
    ImpliedVol=ImpliedVol,
    ClosePrice=ClosePrice,
    market=market
)][,
  .(
    Strike=Strike,
    Days=Days,
    ImpliedVol=ImpliedVol,
    ClosePrice=ClosePrice,
    market=market,
    market_count=length(market)
  ),
  keyby=Date
][ClosePrice>0]


#
# vector of dates -> 
# data.table of
#   all dates
#   rolled date
#   roll count
#
roll_days<-function(dates){
  diffs<-as.integer(diff(dates))
  locs<-cumsum(c(1,diffs))
  times<-c(diffs,1)
  ndx<-seq(from=1,to=locs[length(locs)],by=1)
  last_reset<-rep(locs,times=times)
  rolled_dates<-rep(dates,times=c(diffs,1))
  sequential_dates<-seq(from=dates[1],to=dates[length(dates)],by=1)
  data.table(
    allDate=sequential_dates,
    Date=rolled_dates,
    roll_count=ndx-last_reset
  )
}

#
#
#
roll_vols<-function(vsurf){
  vsurf[
    roll_days(sort(unique(vsurf$Date))),
    on="Date", 
    allow.cartesian=TRUE
  ][,.(
    Date=allDate,
    Strike=Strike,
    Days=Days,
    ImpliedVol=ImpliedVol,
    ClosePrice=ClosePrice,
    market=market,
    market_count=market_count,
    roll_count=roll_count
  )]
}

spx_vol<-all_vol[market=="spx"]

