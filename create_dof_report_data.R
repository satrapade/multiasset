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

#
all_vol<-"compressed_all_vol.txt" %>% 
  scan(character()) %>% 
  decompress %>% 
  {.$Date <- as.Date(.$Date,format="%Y-%m-%d");.}

# surface pillars
all_strikes<-all_vol[,.(strikes=sort(unique(Strike))),keyby=market]
all_maturities<-all_vol[,.(maturities=sort(unique(Days))),keyby=market]

listed_expiries_3m<-list(
    quote(month %in% c("March","June","September","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
)

listed_expiries_6m_a<-list(
    quote(month %in% c("June","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
)

listed_expiries_6m_b<-list(
    quote(month %in% c("March","September")),
    quote(wday=="Friday"),
    quote(wday_count==3)
)


#
# get common dates for all markets 
# in volsurface table
make_common_dates<-function(vsurfs){
  vsurfs[
    ,.(date=Date[1]),
    keyby=c("market","Date")
  ][
    ,.(markets=length(market)),keyby=date
  ][
    markets==max(markets),date
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
make_roll_dates<-function(
  filter_list,
  vsurfs
){
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
  
  roll_dates<-Reduce(
    function(a,b)eval(bquote(.(a)[.(b)])),
    filter_list,
    init=date_df
  )[,.SD,keyby=date][J(sort(unique(date))),.SD,mult="first"]
  
  roll_intervals<-data.table(
    start=c(min(shedule$date),roll_dates$date),
    end=c(roll_dates$date,max(shedule$date)),
    roll=c("pre",as.character(seq_along(1:(nrow(roll_dates)-1))),"post")
  )
  
  
  shedule_with_roll<-data.table(
    shedule,
    roll=roll_intervals$roll[findInterval(shedule$date,roll_intervals$start)]
  )[,.SD,keyby=roll]
  
  shedule_with_roll_and_intervals<-merge(
    x=shedule_with_roll,
    y=roll_intervals,
    by="roll"
  )[,.SD,keyby=date]
  
  
  px<-vsurfs[,.(
    date=as.Date(stri_sub(Date[1],1,10),format="%Y-%m-%d"),
    close=ClosePrice[1],
    vol=mean(ImpliedVol)
  ),keyby=c("market","Date"),][,.(date,market,close,vol)][,.SD,keyby=c("date","market")]
  
  shedule_with_price<-merge(
    x=shedule_with_roll_and_intervals,
    y=px,
    by=c("date","market")
  )
  
  shedule_with_price[,c(.SD,list(
    maturity=as.integer(end-date)
  ))]

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



# option models

# european call
EC <- function(S,X,t,r,v)
{
  ifelse(
    t<1e-10,
    pmax(S-X,0),
    local({
      d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
      d2 <- d1-v*sqrt(t)
      S*pnorm(d1)-X*exp(-r*t)*pnorm(d2)
    })
  )
}


# european put
EP  <- function(S,X,t,r,v)
{
  ifelse(
    t<1e-10,
    pmax(X-S,0),
    local({
      d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
      d2 <- d1-v*sqrt(t)
      X*exp(-r*t)*pnorm(-d2)-S*pnorm(-d1)
    })
  )
}


# strike solvers

# call premium to strike
C2K<-function(K,C,F,v,t,n=3){
  for(i in 1:n){
    d1<-(log(F/K)+(0.5*v^2)*t)/(v*sqrt(t))
    d2<-d1-v*sqrt(t)
    Nd1<-pnorm(d1)
    Nd2<-pnorm(d2)
    K<-F*Nd1/Nd2-C/Nd2
  }
  K
}

# put premium to strike
P2K<-function(K,P,F,v,t,n=3){
  for(i in 1:n){
    d1<-(log(F/K)+(0.5*v^2)*t)/(v*sqrt(t))
    d2<-d1-v*sqrt(t)
    Nmd1<-pnorm(-d1)
    Nmd2<-pnorm(-d2)
    K<-F*Nmd1/Nmd2+P/Nmd2
  }
  K
}

# delta to put strike 
D2CK<-function(delta,close,vol,maturity){
  close*exp(qnorm(1-delta,sd=vol*sqrt(maturity),lower.tail=TRUE))
}

# delta to call strike 
D2PK<-function(delta,close,vol,maturity){
  close*exp(qnorm(delta,sd=vol*sqrt(maturity),lower.tail=TRUE))
}



#
#
#
compute_option_premium<-function(
  shedule,
  all_vol,
  strike_fun=function(close,vol,maturity)close,
  model=EC,
  all_strikes=all_vol[,.(strikes=sort(unique(Strike))),keyby=market],
  all_maturities=all_vol[,.(maturities=sort(unique(Days))),keyby=market]
)merge(
  x=shedule[date==start,.(
    market,
    strike_close=close,
    strike_vol=vol,
    strike=strike_fun(close,vol,maturity),
    roll
  )],
  y=shedule[,.(date,market,close,roll,maturity)],
  by.x=c("market","roll"),
  by.y=c("market","roll")
)[,
  local({
    the_strikes<-all_strikes[market,strikes]
    the_maturities<-all_maturities[market,maturities]
    i<-findInterval(pmax(strike,0),the_strikes)
    j<-findInterval(pmax(maturity,0),the_maturities)
    dets<-data.table(
      date=date,
      market=market,
      lo_strike=the_strikes[i],
      hi_strike=the_strikes[i+1],
      lo_maturity=the_maturities[j],
      hi_maturity=the_maturities[j+1]
    )
    vol_ll<-merge(
      x=dets,
      y=all_vol,
      by.x=c("market","date","lo_strike","lo_maturity"),
      by.y=c("market","Date","Strike","Days")
    )$ImpliedVol
    vol_lh<-merge(
      x=dets,
      y=all_vol,
      by.x=c("market","date","lo_strike","hi_maturity"),
      by.y=c("market","Date","Strike","Days")
    )$ImpliedVol
    vol_hl<-merge(
      x=dets,
      y=all_vol,
      by.x=c("market","date","hi_strike","lo_maturity"),
      by.y=c("market","Date","Strike","Days")
    )$ImpliedVol
    vol_hh<-merge(
      x=dets,
      y=all_vol,
      by.x=c("market","date","hi_strike","hi_maturity"),
      by.y=c("market","Date","Strike","Days")
    )$ImpliedVol
    t_strike<-(strike-dets$lo_strike)/(dets$hi_strike-dets$lo_strike)
    t_mat<-(maturity-dets$lo_maturity)/(dets$hi_maturity-dets$lo_maturity)
    vol<-rowSums(cbind(
      vol_ll*(1-t_strike)*(1-t_mat),
      vol_hl*(t_strike)*(1-t_mat),
      vol_lh*(1-t_strike)*(t_mat),
      vol_hh*(t_strike)*(t_mat)
    ))
    premium<-ifelse(grepl("^[0-9]+$",roll),model(close, strike, maturity/365, 0,vol),0)
    list(
      date=date,
      strike_close=strike_close,
      strike_vol=strike_vol,
      strike=strike,
      option_maturity=maturity,
      option_vol=vol,
      premium=premium
    )
  }),
  keyby=market
]


#
#
#
make_backtest<-function(
  shedule,
  volsurf,
  model,
  strike_fun=function(close,vol,maturity)close*0.9
)
{
  
  # roll strikes, 
  # compute option premium
  options<-merge(
    x=shedule,
    y=compute_option_premium(
      shedule,
      volsurf,
      strike_fun=strike_fun,
      model=model
    )[,.(market,date,strike_close,strike,premium)],
    by.x=c("date","market"),
    by.y=c("date","market")
  )
  
  # 
  #
  backtest<-data.table(
    market=options$market,
    date=options$date,
    start=options$start,
    end=options$end,
    maturity=options$maturity,
    reval=options$premium/options$strike_close
  )[,c(.SD,list(
    consideration=ifelse(start==date,-reval,0),
    payout=ifelse(maturity==1,reval,0)
  ))][,c(.SD,list(
    cash=cumsum(consideration+payout),
    holdings=ifelse(maturity==1,0,reval)
  ))][,.(
    market,
    date,
    start,
    end,
    maturity,
    reval,
    consideration,
    payout,
    cash,
    holdings,
    pnl=cash+holdings
  )]

  #
  #
  backtest 
  
}

strategy_weights<-function(s)do.call(rbind,mapply(function(x)x$weight,s,SIMPLIFY=FALSE))

make_strategy<-function(
  strategy,
  shedule,
  volsurf
){
 
  bk<-mapply(function(leg){
    leg_res<-make_backtest(
      shedule=shedule,
      volsurf=volsurf,
      model=leg$model,
      strike_fun=leg$strike
    )
    leg_res
  },strategy,SIMPLIFY=FALSE)
  
  w<-strategy_weights(strategy)
  
  strat_res<-do.call(cbind,mapply(function(leg)leg$pnl,bk,SIMPLIFY=FALSE))%*%w
  
  data.table(
    date=bk[[1]]$date,
    pnl=strat_res[,1],
    roll=shedule[market==market[1],][,.SD,keyby=date][J(bk[[1]]$date),roll]
  )
  
}

#
#
weighted_strategy<-function(
  weights=c(CSC=0.33,PSC=0.0,ZCRR=0.0,PR=0.33,CR=0.0,SS=0.33),
  backtests=all_backtests_3m,
  stop_level=0.2
){
  
  backtest_matrix<-local({
    res<-do.call(cbind,mapply(function(b){
      stop_loss(b,-abs(stop_level))$stopped_pnl
    },backtests$backtest,SIMPLIFY=FALSE))
    colnames(res)<-paste0(backtests$market,"|",backtests$strategy)
    rownames(res)<-as.character(backtests$backtest[[1]]$date,format="%Y-%m-%d")
    res
  })

  backtest_df<-data.table(
    date=backtests$backtest[[1]]$date,
    pnl=local({
      res<-drop(backtest_matrix %*% cbind(weights[backtests$strategy] %>% {./sum(.)}))
      res
    }),
    roll=backtests$backtest[[1]]$roll
  )
  
  backtest_df
  
}

#
#
weights<-list(
 equal=c(CSC=0.33,PSC=0.0,ZCRR=0.0,PR=0.33,CR=0.0,SS=0.33),
 theoretical=c(CSC=0.33,PSC=0.0,ZCRR=0.0,PR=0.33,CR=0.0,SS=0.33),
 historical=c(CSC=0.43,PSC=0.19,ZCRR=0.07,PR=0.15,CR=0.0,SS=0.16)
)

#
#
sl_levels<-seq(0,0.1,length.out=20)

#
# add stop-loss to strategy
#
stop_loss<-function(
  strategy,
  max_loss=(-0.05)
){
  period_change<-c(TRUE,tail(strategy$roll,-1)!=head(strategy$roll,-1))
  squelched_seq_along<-seq_along(period_change)*period_change
  carry_fwd_seq_along<-cummax(squelched_seq_along)
  carry_fwd_pnl<-strategy$pnl[carry_fwd_seq_along]
  period_pnl<-strategy$pnl-carry_fwd_pnl
  below_stop<-period_pnl<max_loss
  cum_period_stop<-cumsum(below_stop)
  period_below_stop<-(cum_period_stop-cum_period_stop[carry_fwd_seq_along])>0
  pnl_diff<-c(strategy$pnl[1],diff(strategy$pnl))
  lagged_stop<-c(FALSE,period_below_stop[-1])
  stopped_pnl_diff<-pnl_diff*(!lagged_stop)
  stopped_pnl<-cumsum(stopped_pnl_diff)
  data.table(
    strategy,
    period=carry_fwd_seq_along,
    period_pnl=period_pnl,
    below_stop=below_stop,
    period_stop=period_below_stop,
    stopped_pnl=stopped_pnl
  )
}

#
# drawdown stats
#
compute_drawdown_stats<-function(strategy){
  cum_max_px <- cummax(strategy$pnl)
  high_watermark <- (strategy$pnl==cum_max_px)
  squelched_seq_along<-seq_along(high_watermark)*high_watermark
  carry_fwd_seq_along<-cummax(squelched_seq_along)
  carry_fwd_pnl<-strategy$pnl[carry_fwd_seq_along]
  drawdown<-carry_fwd_pnl-strategy$pnl
  
  res<-data.table(
    strategy,
    drawdown=drawdown,
    high_watermark_value=carry_fwd_pnl,
    episode=cumsum(high_watermark)
  )
  
  setkey(res,episode)
  
  res_stats<-res[,list(
    max_draw=max(drawdown),
    length_draw=length(drawdown),
    date_draw=date[which.max(drawdown)],
    date_start=min(date),
    date_end=max(date)
  ),keyby=episode]
  
  res_stats[res]
  
}

#
# drawdown stats
#
make_sl_stats<-function(
  w,
  backtest=weighted_strategy( weights=weights[[w]], backtests=all_backtests_3m),
  sl_levels=seq(0,0.1,length.out = 20)
)do.call(rbind,mapply(
    function(level,backtest){
      dds<-compute_drawdown_stats(stop_loss(backtest[date %in% dof$date],-level)[,pnl:=stopped_pnl])
      data.table(
        levels=level,
        max_draw=max(dds$max_draw),
        median_draw=median(dds$max_draw),
        max_length=max(dds$length_draw),
        median_length=median(dds$length_draw)
      )
    },
    level=sl_levels,
    MoreArgs = list(
      backtest= backtest
    ),
    SIMPLIFY=FALSE
))



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
))][,c(.SD,list(
  plot=mapply(make_simple_backtest_plot,backtest=backtest,the_market=market,the_strategy=strategy)
))]

saveRDS(all_backtests_3m,file="all_backtests_3m.RData")

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
))][,c(.SD,list(
  plot=mapply(make_simple_backtest_plot,backtest=backtest,the_market=market,the_strategy=strategy)
))]

saveRDS(all_backtests_6m_a,file="all_backtests_6m_a.RData")

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
))][,c(.SD,list(
  plot=mapply(make_simple_backtest_plot,backtest=backtest,the_market=market,the_strategy=strategy)
))]

saveRDS(all_backtests_6m_b,file="all_backtests_6m_b.RData")

NNcast<-function(
  data,
  i_name="date",
  j_name="id",
  v_name="value",
  fun=sum,
  scrub_fun=function(x)scrub(x,default=0),
  scrub=function(x, default = 0){
    if(length(x) == 0) return(default)
    x[which(!is.finite(x))] <- default
    return(x)
  }
){
  i_expr<-parse(text=as.character(i_name))
  j_expr<-parse(text=as.character(j_name))
  v_expr<-parse(text=as.character(v_name))
  i<-as.character(eval(i_expr,envir=data))
  j<-as.character(eval(j_expr,envir=data))
  x<-eval(v_expr,envir=data)
  df<-data.table(i=i,j=j,x=x)[,.(x=fun(x)),keyby="i,j"]
  is<-sort(unique(df$i))
  js<-sort(unique(df$j))
  res<-matrix(
    do.call(class(x),list(1)),
    nrow=length(is),
    ncol=length(js),
    dimnames = list(is,js)
  )
  i<-match(df$i,rownames(res))
  j<-match(df$j,colnames(res))
  res[cbind(i,j)[!is.na(df$x),]]<-df$x[!is.na(df$x)]
  scrub_fun(res)
}


backtest_3m_plot_matrix<-NNcast(
  all_backtests_3m,
  i_name="strategy",
  j_name="market",
  v_name="plot",
  fun=identity,
  scrub_fun = identity
)

backtest_6m_a_plot_matrix<-NNcast(
  all_backtests_6m_a,
  i_name="strategy",
  j_name="market",
  v_name="plot",
  fun=identity,
  scrub_fun = identity
)

backtest_6m_b_plot_matrix<-NNcast(
  all_backtests_6m_b,
  i_name="strategy",
  j_name="market",
  v_name="plot",
  fun=identity,
  scrub_fun = identity
)



spx_tail1_backtest<-make_strategy(
  strategy=strategy_TAIL1,
  shedule_3m[market=="spx"],
  all_vol[market=="spx"]
)[,.SD,keyby=date]

spx_tail2_backtest<-make_strategy(
  strategy=strategy_TAIL2,
  shedule_3m[market=="spx"],
  all_vol[market=="spx"]
)[,.SD,keyby=date]

spx_tail5_backtest<-make_strategy(
  strategy=strategy_TAIL5,
  shedule_3m[market=="spx"],
  all_vol[market=="spx"]
)[,.SD,keyby=date]


x<-merge(
  x=spx_tail2_backtest,
  y=dof,
  by.x="date",
  by.y="date"
)

g_tails<-rbind(
  data.table(spx_tail1_backtest,delta="1"),
  data.table(spx_tail2_backtest,delta="2"),
  data.table(spx_tail5_backtest,delta="5")
) %>% 
  ggplot() +
  geom_line(aes(x=date,y=pnl,col=delta),size=2,alpha=0.75)+
  ggtitle("Tail hedges: 1, 2 and 5 deltas")

g_tail2_vs_dof<-melt(merge(
  x=spx_tail2_backtest,
  y=dof,
  by.x="date",
  by.y="date"
)[,.(
  date,
  teeny=cumsum(c(0,diff(pnl.x))),
  dof=cumsum(c(0,diff(pnl.y)))
)],id.vars = "date",measure.vars = c("teeny","dof")) %>% 
  ggplot() +
  geom_line(aes(x=date,y=value,col=variable),size=2,alpha=0.75)+
  ggtitle("Quarterly rolling 2-delta SPX puts vs DOF")

#
#
#
dof_drawdown_stats <- compute_drawdown_stats(dof)[,.(
  max_draw=round(100*max_draw[1],digits=2),
  date_draw=date_draw[1],
  date_start=date_start[1],
  date_end=date_end[1]
),keyby=episode][head(order(-max_draw),10)][date_end>date_start][,list(
  max_draw=max_draw,
  date_draw=as.character(date_draw,format="%Y-%m-%d"),
  date_start=as.character(date_start,format="%Y-%m-%d"),
  date_end=as.character(date_end,format="%Y-%m-%d"),
  teeny1=round(100*spx_tail1_backtest[(date>date_start) & (date<date_draw),max(pnl-pnl[1])],digits=2),
  teeny2=round(100*spx_tail2_backtest[(date>date_start) & (date<date_draw),max(pnl-pnl[1])],digits=2),
  teeny5=round(100*spx_tail5_backtest[(date>date_start) & (date<date_draw),max(pnl-pnl[1])],digits=2)
),keyby=episode]

