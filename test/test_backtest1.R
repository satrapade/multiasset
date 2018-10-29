


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
  
  relevant_shedule<-shedule[date_df$pday<max(res2$pday)]
  
  res3<-data.table(
    head(res2,-1),
    start=head(res2$date,-1),
    end=tail(res2$date,-1),
    roll=seq.int(nrow(res2)-1)
  )
  
  shedule_with_roll<-data.table(relevant_shedule,roll=findInterval(relevant_shedule$date,res3$date))
  
  res4<-res3[,.SD,keyby=roll][shedule_with_roll[,.SD,keyby=roll]][roll>0][,.(
    roll=roll,
    date=i.date,
    start=start,
    end=end,
    market=market,
    maturity=as.integer(end-i.date)
  )]
  
  px<-vsurfs[,.(
      date=as.Date(stri_sub(Date[1],1,10),format="%Y-%m-%d"),
      close=ClosePrice[1],
      vol=mean(ImpliedVol)
  ),keyby=c("market","Date"),][,.(date,market,close,vol)]
  
  res5<-px[,.SD,keyby=c("date","market")][res4[,.SD,keyby=c("date","market")]]
  
  res5

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
    premium<-model(close, strike, maturity/365, 0,vol)
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





synthetic_vol<-data.table(expand.grid(
  Date=seq(Sys.Date()-365,Sys.Date(),by=1),
  Strike=seq(0,300,length.out=10),
  Days=seq(0,700,length.out=10),
  ClosePrice=100.0,
  ImpliedVol=0.1,
  market="synthetic",
  market_count=1,
  stringsAsFactors = FALSE
))[,.SD,keyby=Date]

synthetic_shedule<-make_roll_dates(
  filter_list=listed_expiries_3m,
  vsurfs=synthetic_vol
)


