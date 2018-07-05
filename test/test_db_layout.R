
require(DBI)


db<-dbConnect(
odbc::odbc(),
.connection_string = paste0(
"driver={SQL Server};",
"server=SQLS071FP\MULTIASSET;",
"database=MultiAsset;",
"trusted_connection=true"
))
