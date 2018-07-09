
require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)
require(Rblpapi)


df<-fread(input="
    index      | name                   | start        | end
    SPX Index  | Rbplapi(index,'NAME')  | '2017-01-01' |
    SXXP Index | Rbplapi(index,'NAME')  | '2017-01-01' |
",sep="||")



