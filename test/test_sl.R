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

ES<-function(S,X,t,r,v)EP(S,X,t,r,v)+EC(S,X,t,r,v)


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
  vols<-interpolate_vol(date=strategy$date,strike=strikes,days=strategy$days,vsurf=vsurf)
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

#
#
#

backtest_option<-function(option,strategy=spx_strategy_df,vsurf=resampled_spx_vol){
  
  if(length(option$strike[[1]])<1)return(list(data.table(
    strike=0,
    yfrac=strategy$yfrac,
    vol=rep(0,nrow(strategy)),
    close=strategy$close,
    reval=rep(0,nrow(strategy)),
    cash_start=rep(0,nrow(strategy)),
    cash_end=rep(0,nrow(strategy)),
    cash=rep(0,nrow(strategy)),
    pnl=rep(0,nrow(strategy))
  )$pnl))
  
  option_results<-mapply(
    function(model,strike,dir,strategy,vsurf){
      make_option_pnl(
        model=model,
        strike=strike,
        dir=dir,
        strategy=strategy,
        vsurf=vsurf
      )$pnl
    },
    strike=option$strike[[1]],
    dir=option$size[[1]],
    model=option$model[[1]],
    MoreArgs=list(strategy=strategy,vsurf=vsurf),
    SIMPLIFY=FALSE
  )
  
  option_results
  
}

model_payoff<-function(payoff){
  
  if(length(payoff$strike[[1]])<1)return(rep(0,25))
  MoreArgs<-list(
    models=payoff$model[[1]],
    strikes=payoff$strike[[1]],
    sizes=payoff$size[[1]]
  ) 
  
  spots<-seq(0.75,1.25,length.out=25)
  
  payoff_fun<-function(spot,models,strikes,sizes)sum(mapply(
      function(spot,model,strike,size)size*model(S=spot,X=strike,t=0.01,r=0,v=0.1),
      model=models,
      strike=strikes,
      size=sizes,
      MoreArgs=list(spot=spot)
  ))
  
  
  mapply(payoff_fun, spot=spots, MoreArgs = MoreArgs)
  
}

apply_stop<-function(px,stop_level,rolls=floor(seq(1,length(px),length.out = 10)))data.table(
  tret=c(0,diff(px)),
  px=px
)[,
  c(.SD,list(
    start_px=rep(px[rolls],times=c(diff(rolls),length(px)-tail(rolls,1)+1)),
    roll=rep(rolls,times=c(diff(rolls),length(px)-tail(rolls,1)+1))
  ))
][,
  c(.SD,list(dd=px-start_px))
][,
  c(.SD,list(sl=c(0,head(cummax(dd<(-stop_level)),-1)))),
  keyby=roll
][,
  c(.SD,list(stopped_tret=ifelse(sl>0,0,tret)))
][,
  c(.SD,list(stopped_px=cumsum(stopped_tret)))
]

    
make_backtest<-function(options,sl,w,strategy,vsurf)
{
    all_results<-mapply(
      function(o,strategy,vsurf){
        do.call(cbind,backtest_option(option=o,strategy=strategy,vsurf=vsurf))
      },
      o=options,
      MoreArgs = list(strategy=strategy,vsurf=vsurf),
      SIMPLIFY = FALSE
    )
  
    if(length(all_results)<1)return(NULL)
    
    result_matrix<-do.call(cbind,all_results)
    all_pnl<-rowSums(result_matrix)
    
    a<-apply_stop(px=all_pnl,stop_level=sl,rolls=which(strategy$pday==strategy$start))
    stopped_pnl<-a$stopped_px
    
    strategy_pnl<-data.table( 
      date=as.Date(strategy$date,format="%Y-%m-%d"), 
      pnl=stopped_pnl
    )
    
    strategy_pnl
}



#
# period-specific stop loss
#
strategy_df<-fread("strategy_df.csv")

spx_strategy_df<-fread("spx_strategy_df.csv")
ndx_strategy_df<-fread("ndx_strategy_df.csv")
dax_strategy_df<-fread("dax_strategy_df.csv")
sx5e_strategy_df<-fread("dax_strategy_df.csv")

resampled_spx_vol<-"compressed_resampled_spx_vol.txt" %>% scan(character()) %>% decompress
resampled_ndx_vol<-"compressed_resampled_ndx_vol.txt" %>% scan(character()) %>% decompress
resampled_dax_vol<-"compressed_resampled_dax_vol.txt" %>% scan(character()) %>% decompress
resampled_sx5e_vol<-"compressed_resampled_sx5e_vol.txt" %>% scan(character()) %>% decompress

payoffs<-rbind(
  data.table( # No model
    name="None",model=list(list()),strike=list(),size=list()
  ),
  data.table( # European-style call
    name="Call",model=list(list(EC)),strike=list(c(1)),size=list(c(1))
  ),
  data.table( # European-style pus
    name="Put",model=list(list(EP)),strike=list(c(1)),size=list(c(1))
  ),
  data.table( # Call spread
    name="CallSpread",model=list(list(EC,EC)),strike=list(c(1,1.15)),size=list(c(1,-1))
  ),
  data.table( # Put spread
    name="PutSpread",model=list(list(EP,EP)),strike=list(c(0.85,1)),size=list(c(-1,1))
  ),
  data.table( # Strangle
    name="Strangle",model=list(list(EP,EC)),strike=list(c(0.85,1.15)),size=list(c(1,1))
  ),
  data.table( # Straddle
    name="Straddle",model=list(list(ES)),strike=list(c(1)),size=list(c(1))
  ),
  data.table( # Butterfly
    name="Butterfly",model=list(list(EP,EP,EP)),strike=list(c(0.85,1,1.15)),size=list(c(1,-2,1))
  )
)[,c(.SD,list(select=seq_along(name)))][,.SD,keyby="name"]



x<-make_backtest(
 options=list(payoffs["PutSpread"]),
 sl=1000,
 w=130,
 strategy=spx_strategy_df,
 vsurf=resampled_spx_vol
)





