
require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)
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

# the performance database
db<-dbConnect(
odbc::odbc(), 
.connection_string = paste0(
"driver={SQL Server};",
"server=SQLS071FP\\QST;",
"database=PRDQSTFundPerformance;",
"trusted_connection=true"
))


# macro expansion for dynamic SQL queries
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/query.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/sql_tools/make_query.R")


product<-make_query(
product_id="3",
query_string="
SELECT *
FROM tProduct
WHERE ProductId=--R{product_id}--
") %>%
query %>%
t %>%
{data.table(field=rownames(.),value=.[,1])}


holdings<-"
SELECT 
  SUBSTRING(CONVERT(varchar,tHistoricalProductHolding.HistoricalDate),1,10) AS date,
  tHistoricalProductHolding.SecurityId AS security_id,
  tSecurity.Name AS name,
  tSecurityType.Name as security_type,
  tSecurity.Ticker as ticker,
  tSecurity.UniqueId as unique_id,
  tHistoricalProductHolding.SecurityUnits AS units,
  tHistoricalSecurity.UnitPrice AS unit_price,
  tHistoricalSecurity.UnitAccruedIncome AS unit_accrued_income,
  tHistoricalSecurity.UnitAccruedIncome AS unit_accrued_income,
  tSecurity.MaturityDate AS maturity,
  tSecurity.ContractSize AS size,
  tSecurity.ContractStrike AS strike,
  tContractType.Name AS contract_type
FROM tHistoricalProductHolding
LEFT JOIN tSecurity 
ON tSecurity.SecurityId = tHistoricalProductHolding.SecurityId
LEFT JOIN tSecurityType 
ON tSecurityType.SecurityTypeId = tSecurity.SecurityTypeId
LEFT JOIN tHistoricalSecurity 
ON tHistoricalSecurity.SecurityId = tSecurity.SecurityId
AND tHistoricalSecurity.HistoricalDate = tHistoricalProductHolding.HistoricalDate
AND tHistoricalSecurity.DataSourceId = 0
LEFT JOIN tContractType
ON tContractType.ContractTypeId = tSecurity.ContractTypeId
WHERE tHistoricalProductHolding.ProductId=--R{product_id}--
AND tHistoricalProductHolding.DataSourceId=--R{data_source_id}--
" %>%
{list(
  query_string=.,
  product_id="3",
  data_source_id="1"
)} %>%
{do.call(make_query,.)} %>%
query

instrument_types<-table(holdings$security_type) %>%
{data.table(type=names(.),count=as.integer(.))} %>%
{.[order(-count)]}

current_holdings<-holdings[date==max(date)]

hb<-"
SELECT 
  t4Bucket.Name as root,
  t3Bucket.Name as level_1,
  t2Bucket.Name as level_2,
  t1Bucket.Name as level_3,
  tBucket.Name as level_4
FROM tBucket
LEFT JOIN tBucket AS t1Bucket
ON  t1Bucket.BucketId = tBucket.ParentBucketId 
LEFT JOIN tBucket AS t2Bucket
ON  t2Bucket.BucketId = t1Bucket.ParentBucketId 
LEFT JOIN tBucket AS t3Bucket
ON  t3Bucket.BucketId = t2Bucket.ParentBucketId 
LEFT JOIN tBucket AS t4Bucket
ON  t4Bucket.BucketId = t3Bucket.ParentBucketId 
WHERE t4Bucket.Name='MagBucket'
" %>%
{list(
  query_string=.
)} %>%
{do.call(make_query,.)} %>%
query


