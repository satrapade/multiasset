#
# testing extended dataframe with rendering
#

require(DBI)
require(gsubfn)
require(data.table)
require(stringi)
require(magrittr)


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

df<-fread(input="
index      | name             | start        | end
SPX Index  | paste0('a','c')  | '2017-01-01' | gsub(' Index','',index)
SXXP Index | paste0('a','c')  | '2017-01-01' | gsub(' Index','',index)
",sep="|") %>% 
set_format("start",toupper) %>%
set_format(
  "name", 
  . %>% { mapply(function(n)parse(text=n),.,SIMPLIFY=FALSE) } %>% { mapply(function(n)parse(text=n),.,SIMPLIFY=FALSE) } 
) %>%
set_format(
  "end", 
  . %>% {parse(text=.)} %>% {mapply(function(n)try(eval(n),silent=TRUE),.)} 
) %>%
render_df 

