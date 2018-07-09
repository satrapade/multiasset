#
# testing extended dataframe with rendering
#

require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)
require(Rblpapi)

rcon<-Rblpapi::blpConnect()

nn_field_seq_eval<-function(x){
  envir<-parent.frame()
  x_exp<-mapply(parse,text=x,SIMPLIFY = FALSE)
  x_res<-mapply(function(e,rownum){
    current_row<-mapply(
      function(n,i)get(n,envir)[[i]],
      n=ls(envir),
      MoreArgs=list(i=rownum),
      SIMPLIFY=FALSE
    )
    try(eval(e,envir=list2env(current_row)),silent=TRUE)
  },e=x_exp,rownum=seq_along(x_exp),SIMPLIFY = FALSE)
  x_res
}

df<-fread(input="
    index      | name                             | start        | end
    SPX Index  | Rblpapi::bdp(index,'NAME')$NAME  | '2017-01-01' | gsub(' Index','',index)
    SXXP Index | Rblpapi::bdp(index,'NAME')$NAME  | '2017-01-01' | gsub(' Index','',index)
",sep="|") %>% 
  set_format("start",list(.%>%paste0("XXX"))) %>%
  set_format("name",list(bdp=nn_field_seq_eval)) %>%
  set_format("end",list(subst=nn_field_seq_eval))

render_df<-function(x,use_format=1){
  rdf<-data.table(x)
  edf<-list2env(rdf)
  for(i in names(x)){
    current_format<-attributes(x[[i]])$format
    if(!is.null(current_format))if(class(current_format)=="list"){
      rdf[[i]]<-do.call(what=current_format[[use_format]],args=list(x[[i]]),envir=edf)
    }
  }
  rdf
}

df1<-data.table(
  index=c("SX7P Index","SXXP Index","UKX Index"),
  name="Rblpapi::bdp(index,'NAME')$NAME",
  history="Rblpapi::bdh(index,'PX_LAST',as.Date('2018-01-01'),as.Date('2018-07-01'))"
) %>% 
  set_format("name",list(bdp=nn_field_seq_eval)) %>%
  set_format("history",list(bdh=nn_field_seq_eval)) %>%
  render_df

print.nntable<-function(x,...){
  df<-render_df(x)
  print(df)
}

f1<-function(...){
  args<-mapply(
    eval,
    tail(as.list(match.call(expand.dots=TRUE)),-1),
    MoreArgs=list(envir=parent.frame()),
    SIMPLIFY = FALSE
  )
  args
}
  
df<-data.table(
  x=1:5,
  y=rev(1:5)
)

mapply(f1,df,SIMPLIFY=FALSE)

do.call(mapply,c(list(FUN=f1,SIMPLIFY=FALSE),df))






