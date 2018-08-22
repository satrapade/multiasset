
require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)
require(fasttime)
require(lubridate)

source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/query.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/sql_tools/make_query.R")
source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/make_date_range.R")

# the multi-asset database
dbma<-dbConnect(
odbc::odbc(),
.connection_string = paste0(
"driver={SQL Server};",
"server=SQLS071FP\\MULTIASSET;",
"database=MultiAsset;",
"trusted_connection=true"
))

spx_divy<-query(make_query(
security_id="108105",
query_string = "
  SELECT 
    *
  FROM INDEX_DIVIDEND
  WHERE INDEX_DIVIDEND.SecurityID = --R{security_id}--
"),
db=dbma)

spx_fut<-query(make_query(
security_id="108105",
query_string = "
  SELECT 
    *
  FROM FUTURE_PRICE
  WHERE FUTURE_PRICE.SecurityID = --R{security_id}--
"),
db=dbma)

spx_vol<-query(make_query(
security_id="108105",
query_string = "
  SELECT 
    *
  FROM VOLATILITY_SURFACE_2014
  WHERE VOLATILITY_SURFACE_2014.SecurityID = --R{security_id}--
"),
db=dbma)



# dates
make_date_df<-function(start,end)make_date_range(start,end) %>% 
{data.table(
  date_string=.,
  date=fastPOSIXct(.))[,
  c(.SD,list(
    day=day(date),
    mday=mday(date),
    qday=qday(date),
    yday=yday(date),
    pday=seq_along(date),
    wday=weekdays(date),
    month=months(date),
    wday_count=ceiling(mday(date)/7)
  ))
]}

date_df<-make_date_df("2015-01-01","2018-08-01")

expiries<-list(
  quote(month %in% c("March","June","September","December")),
  quote(wday=="Friday"),
  quote(wday_count==3)
)

make_dates<-function(filter_list,date_df){
  Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=date_df)
}

expiry_dates<-make_dates(expiries,make_date_df("2015-01-01","2018-08-01"))

leg<-list(
  start=expiries,
  size=quote(AUM*0.1),
  strike=quote(call_delta2strike(30)),
  type="EC"
)









template_1<-list(
  opt1=list(
    size=list(100,"pct"),
    strike=list(110,"pct"),
    maturity=list(90,"days"),
    type=list("EC")
  ),
  opt2=list(
    size=list(100,"pct"),
    strike=list(140,"pct"),
    maturity=list(90,"days"),
    type=list("EC")
  ),
  opt3=list(
    size=list(100,"pct"),
    strike=list(110,"pct"),
    maturity=list(90,"days"),
    type=list("EC")
  )
)





