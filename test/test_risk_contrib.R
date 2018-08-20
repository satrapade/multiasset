#
# testing extended dataframe with rendering
#

require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)
require(FactoMineR)

tri<-function(n,d=1,s=1)(s*row(diag(n))<s*col(diag(n)))+d*diag(n)

tret<- 50 %>% 
  {mapply(rnorm,sd=runif(.),MoreArgs=list(n=1000),SIMPLIFY=FALSE)} %>%
  {do.call(cbind,.)} 

ccy_tr <- fread("ccy_tr.csv") %>% 
  {as.matrix(.[,-1],rownames=.$rn)} %>%
  {.[,-which(apply(.,2,sd)<1e-10)]} %>%
  {apply(log(.),2,diff)} %>%
  {.%*%diag(1/apply(.,2,sd))}
  

ptfs<- tri(ncol(ccy_tr))%*%diag(1/seq(ncol(ccy_tr)))

vars<-setNames(apply(ccy_tr%*%ptfs,2,var),seq(ncol(ccy_tr)))
vols<-setNames(apply(ccy_tr%*%ptfs,2,sd),seq(ncol(ccy_tr)))





pc<-prcomp(tret)

pc1<-PCA(ccy_tr)


