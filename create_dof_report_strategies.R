#
# strategy setup
#

########################################
# shedules
########################################

shedule_3m<-make_roll_dates(
  filter_list=listed_expiries_3m,
  vsurfs=all_vol
)

shedule_6m_a<-make_roll_dates(
  filter_list=listed_expiries_6m_a,
  vsurfs=all_vol
)

shedule_6m_b<-make_roll_dates(
  filter_list=listed_expiries_6m_b,
  vsurfs=all_vol
)


########################################
# short 90-100 putspread
########################################
strategy_90_100_PS<-list(
  leg1=list(
    model=EP,
    strike=function(close,vol,maturity){ close*0.9 },
    weight=(+1)
  ),
  leg2=list(
    model=EP,
    strike=function(close,vol,maturity){ close*1.0 },
    weight=(-1)
  )
)

########################################
# short ATM straddle
########################################
strategy_SS<-list(
  leg1=list(
    model=EC,
    strike=function(close,vol,maturity){ close*1.0 },
    weight=(-1)
  ),
  leg2=list(
    model=EP,
    strike=function(close,vol,maturity){ close*1.0 },
    weight=(-1)
  )
)

########################################
# CSC, 25-45 delta CS vs solve-P
########################################
strategy_CSC<-list(
  leg1=list(
    model=EC,
    strike=function(close,vol,maturity){ 
      call_K_low<-K<-D2CK(0.45,close,vol,maturity/365)
      call_K_low
    },
    weight=(+1)
  ),
  leg2=list(
    model=EC,
    strike=function(close,vol,maturity){ 
      call_K_hi<-K<-D2CK(0.25,close,vol,maturity/365)
      call_K_hi
    },
    weight=(-1)
  ),
  leg3=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      call_K_low<-K<-D2CK(0.45,close,vol,maturity/365)
      call_K_hi<-K<-D2CK(0.25,close,vol,maturity/365)
      CS_premium<-EC(close,call_K_low,maturity/365,0,vol)-EC(close,call_K_hi,maturity/365,0,vol)
      put_K<-P2K(close,CS_premium,close,vol,maturity/365)
      put_K
    },
    weight=(-1)
  )
)

########################################
# PSC, 25-45 delta PS vs solve-C
########################################
strategy_PSC<-list(
  leg1=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      put_K_hi<-D2PK(0.45,close,vol,maturity/365)
      put_K_hi
    },
    weight=(+1)
  ),
  leg2=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      put_K_low<-D2PK(0.25,close,vol,maturity/365)
      put_K_low
    },
    weight=(-1)
  ),
  leg3=list(
    model=EC,
    strike=function(close,vol,maturity){ 
      put_K_hi<-D2PK(0.45,close,vol,maturity/365)
      put_K_low<-D2PK(0.25,close,vol,maturity/365)
      PS_premium<-EP(close,put_K_hi,maturity/365,0,vol)-EP(close,put_K_low,maturity/365,0,vol)
      call_K<-C2K(close,PS_premium,close,vol,maturity/365)
      call_K
    },
    weight=(-1)
  )
)

########################################
# PR, 45 delta P vs solve-P @ half the
# premium at twice the size
########################################
strategy_PR<-list(
  leg1=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      put_K_hi<-D2PK(0.45,close,vol,maturity/365)
      put_K_hi
    },
    weight=(+1)
  ),
  leg2=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      put_K_hi<-D2PK(0.45,close,vol,maturity/365)
      P_premium<-EP(close,put_K_hi,maturity/365,0,vol)
      put_K_low<-P2K(close,P_premium/2,close,vol,maturity/365)
      put_K_low
    },
    weight=(-2)
  )
)

########################################
# CR, 45 delta C vs solve-C @ half the
# premium at twice the size
########################################
strategy_CR<-list(
  leg1=list(
    model=EC,
    strike=function(close,vol,maturity){ 
      call_K_low<-D2CK(0.45,close,vol,maturity/365)
      call_K_low
    },
    weight=(+1)
  ),
  leg2=list(
    model=EC,
    strike=function(close,vol,maturity){ 
      call_K_low<-D2CK(0.45,close,vol,maturity/365)
      C_premium<-EC(close,call_K_low,maturity/365,0,vol)
      call_K_high<-C2K(close,C_premium/2,close,vol,maturity/365)
      call_K_high
    },
    weight=(-2)
  )
)

########################################
# ZCRR, 25 delta C vs solve-P 
########################################
strategy_ZCRR<-list(
  leg1=list(
    model=EC,
    strike=function(close,vol,maturity){ 
      call_K<-D2CK(0.25,close,vol,maturity/365)
      call_K
    },
    weight=(+1)
  ),
  leg2=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      call_K<-D2CK(0.25,close,vol,maturity/365)
      C_premium<-EC(close,call_K,maturity/365,0,vol)
      put_K<-P2K(close,C_premium,close,vol,maturity/365)
      put_K
    },
    weight=(-1)
  )
)


########################################
# TAIL1, 1 delta P
########################################
strategy_TAIL1<-list(
  leg1=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      put_K<-D2PK(0.01,close,vol,maturity/365)
      put_K
    },
    weight=(+1)
  )
)


########################################
# TAIL2, 2 delta P
########################################
strategy_TAIL2<-list(
  leg1=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      put_K<-D2PK(0.02,close,vol,maturity/365)
      put_K
    },
    weight=(+1)
  )
)

########################################
# TAIL5, 5 delta P
########################################
strategy_TAIL5<-list(
  leg1=list(
    model=EP,
    strike=function(close,vol,maturity){ 
      put_K<-D2PK(0.05,close,vol,maturity/365)
      put_K
    },
    weight=(+1)
  )
)

########################################
# list of strategies
########################################
strategies<-list(
  SS=strategy_SS,
  CSC=strategy_CSC,
  PSC=strategy_PSC,
  CR=strategy_CR,
  PR=strategy_PR,
  ZCRR=strategy_ZCRR,
  TAIL1=strategy_TAIL1,
  TAIL2=strategy_TAIL2,
  TAIL5=strategy_TAIL5,
  PS_100_90=strategy_90_100_PS
)


########################################
# list of markets
########################################
markets<-c(
  "spx",
  "sx5e",
  "ftse",
  "nky"
)

########################################
# strategy portfolios
########################################
weights<-list(
  ##
  equal=c(
    CSC=0.33, PSC=0.0, ZCRR=0.0, PR=0.33, CR=0.0, SS=0.33, TAIL1=0, TAIL2=0, TAIL5=0, PS_100_90=0
  ),
  ##
  theoretical=c(
    CSC=0.33, PSC=0.0, ZCRR=0.0, PR=0.33, CR=0.0, SS=0.33, TAIL1=0, TAIL2=0, TAIL5=0, PS_100_90=0
  ),
  ##
  historical=c(
    CSC=0.43, PSC=0.19, ZCRR=0.07, PR=0.15, CR=0.0, SS=0.16, TAIL1=0, TAIL2=0, TAIL5=0, PS_100_90=0
  ),
  ##
  putspread=c(
    CSC=0, PSC=0, ZCRR=0, PR=0, CR=0, SS=0, TAIL1=0, TAIL2=0, TAIL5=0, PS_100_90=1.0
  ),
  ##
  tail1=c(
    CSC=0, PSC=0, ZCRR=0, PR=0, CR=0, SS=0, TAIL1=1.0, TAIL2=0, TAIL5=0, PS_100_90=0
  ),
  ##
  tail2=c(
    CSC=0, PSC=0, ZCRR=0, PR=0, CR=0, SS=0, TAIL1=0, TAIL2=1.0, TAIL5=0, PS_100_90=0
  ),
  ##
  tail5=c(
    CSC=0, PSC=0, ZCRR=0, PR=0, CR=0, SS=0, TAIL1=0, TAIL2=0, TAIL5=1.0, PS_100_90=0
  )
)

########################################
#
# stop-loss levels
#
########################################
sl_levels<-seq(0,0.1,length.out=20)

