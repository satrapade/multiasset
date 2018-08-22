
require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)
require(fasttime)
require(lubridate)

require(Rblpapi)
rcon<-Rblpapi::blpConnect()

# the multi-asset database
dbma<-dbConnect(
odbc::odbc(),
.connection_string = paste0(
"driver={SQL Server};",
"server=SQLS071FP\\MULTIASSET;",
"database=MultiAsset;",
"trusted_connection=true"
))


# macro expansion for dynamic SQL queries
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/query.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/sql_tools/make_query.R")
source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/make_date_range.R")


# query(make_query(
# security_id="108105",
# query_string = "
#  SELECT TOP 1 
#       SecurityID, 
#       Ticker
#     FROM SECURITY_NAME
#     WHERE SecurityID = --R{security_id}--
#     ORDER BY EffectiveDate DESC
# "),
# db=dbma
# )

x1<-query(make_query(
security_id="108105",
query_string = "
  SELECT
    OPTION_PRICE.Date AS date,
    SECURITY.Ticker AS ticker,
    SECURITY_PRICE.ClosePrice AS price,
    OPTION_PRICE.Symbol AS symbol,
    OPTION_PRICE.CallPut AS payoff,
    OPTION_PRICE.Strike/1000 AS strike,
    OPTION_PRICE.Expiration AS expiry,
    DATEDIFF(day,OPTION_PRICE.Date,OPTION_PRICE.Expiration)  AS days,
    OPTION_PRICE.Bid AS bid,
    OPTION_PRICE.Ask AS ask
  FROM OPTION_PRICE
  LEFT JOIN (
    SELECT TOP 1 SecurityID, Ticker
    FROM SECURITY_NAME WHERE SecurityID = --R{security_id}--
    ORDER BY EffectiveDate DESC
  ) AS SECURITY
  ON SECURITY.SecurityID = OPTION_PRICE.SecurityID
  LEFT JOIN SECURITY_PRICE
  ON OPTION_PRICE.SecurityID = SECURITY_PRICE.SecurityID
  AND OPTION_PRICE.Date = SECURITY_PRICE.Date
  WHERE OPTION_PRICE.SecurityID = --R{security_id}--
  /*AND DATEDIFF(day,OPTION_PRICE.Date,OPTION_PRICE.Expiration) < 100*/
"),
db=dbma)

# query(make_query(
# security_id="108105",
# symbol="'SPXW 180329P2850000'",
# query_string = "
#   SELECT
#     OPTION_PRICE.Date AS date,
#     OPTION_PRICE.Symbol AS symbol,
#     OPTION_PRICE.CallPut AS payoff,
#     OPTION_PRICE.Strike/1000 AS strike,
#     OPTION_PRICE.Expiration AS expiry,
#     DATEDIFF(day,OPTION_PRICE.Date,OPTION_PRICE.Expiration)  AS days,
#     OPTION_PRICE.Bid AS bid,
#     OPTION_PRICE.Ask AS ask
#   FROM OPTION_PRICE
#   WHERE OPTION_PRICE.SecurityID = --R{security_id}--
#   AND Symbol= --R{symbol}--
# "),
# db=dbma)


expiry_days<-unique(x1[days==0,date])

expiries<-data.table(
  date=expiry_days,
  weekday=weekdays(expiry_days),
  mday=mday(expiry_days),
  count=ceiling(mday(expiry_days)/7)
)


