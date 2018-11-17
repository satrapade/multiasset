#
#
#
#
#
#

source("https://raw.githubusercontent.com/satrapade/latex_utils/master/latex_helpers_v2.R")
source("https://raw.githubusercontent.com/satrapade/utility/master/utility_functions.R")
source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
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
    #market=market,
    market_count=market_count,
    roll_count=roll_count
  )]
}

resampled_spx_vol<-"compressed_resampled_spx_vol.txt" %>% scan(character()) %>% decompress
resampled_sx5e_vol<-"compressed_resampled_sx5e_vol.txt" %>% scan(character()) %>% decompress
resampled_ftse_vol<-"compressed_resampled_ftse_vol.txt" %>% scan(character()) %>% decompress
resampled_nky_vol<-"compressed_resampled_nky_vol.txt" %>% scan(character()) %>% decompress


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
][ClosePrice>0][,roll_vols(.SD),keyby=market][,.SD,keyby=Date]

fwrite(all_vol,paste0("all_vol.csv"))
write(compress(fread(paste0("all_vol.csv"))),paste0("compressed_all_vol.txt"))

cat("loading functions")
source("create_dof_report_backtest_functions.R")


# read DOF performance, 
# convert character to Date
# convert daily P&L to cummulative P&L
dof<- fread("dof.csv") %>% 
{setNames(.,tolower(names(.)))} %>%
{.$date<-as.Date(.$date,format="%Y-%m-%d"); .} %>%
{.$pnl<-cumsum(.$pnl); .}


############################################################################################

source("create_dof_report_strategies.R")

############################################################################################

cat("backtest 3m\n")
all_backtests_3m<-data.table(
  strategy=rep(names(strategies),times=length(markets)),
  market=rep(markets,each=length(strategies))
)[,c(.SD,list(
  backtest=mapply(function(the_market,the_strategy){
    make_strategy(
      strategy=strategies[[the_strategy]],
      shedule_3m[market==the_market],
      all_vol[market==the_market]
    )
  },market,strategy,SIMPLIFY=FALSE)
))]
saveRDS(all_backtests_3m,file="all_backtests_3m.RData")

cat("backtest 6m_a\n")
all_backtests_6m_a<-data.table(
  strategy=rep(names(strategies),times=length(markets)),
  market=rep(markets,each=length(strategies))
)[,c(.SD,list(
  backtest=mapply(function(the_market,the_strategy){
    make_strategy(
      strategy=strategies[[the_strategy]],
      shedule_6m_a[market==the_market],
      all_vol[market==the_market]
    )
  },market,strategy,SIMPLIFY=FALSE)
))]
saveRDS(all_backtests_6m_a,file="all_backtests_6m_a.RData")

cat("backtests 6m_b\n")
all_backtests_6m_b<-data.table(
  strategy=rep(names(strategies),times=length(markets)),
  market=rep(markets,each=length(strategies))
)[,c(.SD,list(
  backtest=mapply(function(the_market,the_strategy){
    make_strategy(
      strategy=strategies[[the_strategy]],
      shedule_6m_b[market==the_market],
      all_vol[market==the_market]
    )
  },market,strategy,SIMPLIFY=FALSE)
))]
saveRDS(all_backtests_6m_b,file="all_backtests_6m_b.RData")





