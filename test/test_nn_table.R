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

render_df<-function(x,use_format=1){
  rdf<-data.table(x)
  for(i in names(x)){
    current_format<-attributes(x[[i]])$format
    if(!is.null(current_format))if(class(current_format)=="list"){
      rdf[[i]]<-do.call(what=current_format[[use_format]],args=list(x[[i]]))
    }
  }
  rdf
}

print.nntable<-function(x,...){
  df<-render_df(x)
  print(df,...)
}

set_format<-function(
  df,
  fn="*", # all fields
  ff=.%>%comma(digits=0) # add commas to large numbers
){
  table_fields<-names(df)
  matched_table_fields<-table_fields[grepl(fn,table_fields)]
  if(length(matched_table_fields)<1)return(df)
  for(i in matched_table_fields){
    current_format<-attributes(df[[i]])$format
    new_format<-c(current_format,ff)
    attributes(df[[i]])$format<-new_format
  }
  if(!"nntable" %in% class(df)){
    class(df)<-c("nntable",class(df))
  }
  df
}


nn_eval_byrow<-function(x){
  x_exp<-mapply(parse,text=x,SIMPLIFY = FALSE)
  x_res<-mapply(function(e){
    try(eval(e),silent=TRUE)
  },e=x_exp,rownum=seq_along(x_exp),SIMPLIFY = FALSE)
  x_res
}

row_wise_fun<-function(...){
  args<-mapply(
    eval,
    tail(as.list(match.call(expand.dots=TRUE)),-1),
    MoreArgs=list(envir=parent.frame()),
    SIMPLIFY = FALSE
  )
  args
}


df<-fread(input="
    index      | name                             | start        | end
    SPX Index  | Rblpapi::bdp(index,'NAME')$NAME  | '2017-01-01' | gsub(' Index','',index)
    SXXP Index | Rblpapi::bdp(index,'NAME')$NAME  | '2017-01-01' | gsub(' Index','',index)
",sep="|") %>% 
  set_format("start",list(.%>%paste0("XXX"))) %>%
  set_format("name",list(bdp=nn_eval_byrow)) %>%
  set_format("end",list(subst=nn_eval_byrow)) %>%
  render_df 



df1<-data.table(
  index=c("SX7P Index","SXXP Index","UKX Index"),
  name="Rblpapi::bdp(index,'NAME')$NAME",
  history="Rblpapi::bdh(index,'PX_LAST',as.Date('2018-01-01'),as.Date('2018-07-01'))"
) %>% 
  set_format("name",list(bdp=nn_field_seq_eval)) %>%
  set_format("history",list(bdh=nn_field_seq_eval)) %>%
  render_df



  
df<-data.table(
  x=1:5,
  y=rev(1:5)
)

mapply(f1,df,SIMPLIFY=FALSE)

do.call(mapply,c(list(FUN=f1,SIMPLIFY=FALSE),df))



f2<-function(x){
  res<-substitute(x,environment())
  eval(res)
}


