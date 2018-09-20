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

vsurfs<-list(
  spx=resampled_spx_vol,
  ndx=resampled_ndx_vol,
  sx5e=resampled_sx5e_vol,
  dax=resampled_dax_vol
)

listed_expiries<-list(
    quote(month %in% c("March","June","September","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
)

# get common dates for list of
# vol surfaces
make_common_dates<-function(
  vsurf_list=list(
    resampled_spx_vol,resampled_ndx_vol,resampled_sx5e_vol,resampled_dax_vol
  )
){
  date_strings<-mapply(
    function(v)sort(unique(stri_sub(v$Date,1,10))),
    vsurf_list
  )
  common_dates<-sort(unique(Reduce(intersect,date_strings)))
  common_dates
}


# make a shedule from a list of volatility surfaces
make_shedule<-function(
  volsurf_list,
  filter_list=expiries<-list(
    quote(month %in% c("March","June","September","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
  )
)
{
  common_date_string<-make_common_dates(volsurf_list)
  common_date_value<-as.Date(common_date_string,format="%Y-%m-%d")
  shedule_df <- data.table(
    date=rep(common_date_value,times=length(volsurf_list)),
    date_string=rep(common_date_string,times=length(volsurf_list)),
    market=rep(names(volsurf_list),each=length(common_date_string))
  )
  shedule_df
}

# create a roll schedule from a shedule
make_roll_dates<-function(filter_list,shedule){
  
  date_df<-data.table(
    date=shedule$date,
    date_string=shedule$date_string,
    day=day(shedule$date),
    mday=mday(shedule$date),
    qday=qday(shedule$date),
    yday=yday(shedule$date),
    pday=seq_along(shedule$date),
    wday=weekdays(shedule$date),
    month=months(shedule$date),
    wday_count=ceiling(mday(shedule$date)/7)
  )[,.SD,keyby=date_string]
  
  res<-Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=date_df)
  res<-res[,.SD,keyby=date_string][sort(unique(res$date_string)),.SD,mult="first"]
  
  res$start <- shedule$date_string[res$pday]
  res$end   <- shedule$date_string[c(res$pday[-1],nrow(shedule))]
  
  res$roll  <- 1:nrow(res)
  res
}


x<-make_shedule(vsurfs)
y<-make_roll_dates(listed_expiries,x)
x$roll<-findInterval(x$date,y$date)
z<-y[,.SD,keyby=roll][x[,.SD,keyby=roll]]

















