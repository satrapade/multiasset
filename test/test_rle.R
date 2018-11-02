

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


