#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

require(data.table)
require(scales)
require(magrittr)
require(fasttime)
require(lubridate)
require(scales)
require(fields)
require(gsubfn)
require(stringi)
require(Matrix)
require(akima)
require(ggplot2)
require(shinyWidgets)

source("https://raw.githubusercontent.com/satrapade/utility/master/utility_functions.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/query.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/sql_tools/make_query.R")
source("https://raw.githubusercontent.com/satrapade/utility/master/nn_cast.R")
source("https://raw.githubusercontent.com/satrapade/pairs/master/utility/make_date_range.R")


make_market<-function(id="108105",vol_table="VOLATILITY_SURFACE_2014")
{
  dbma<-dbConnect(
    odbc::odbc(),
    .connection_string = paste0(
      "driver={SQL Server};",
      "server=SQLS071FP\\MULTIASSET;",
      "database=MultiAsset;",
      "trusted_connection=true"
    ))
  the_query<-make_query(
    security_id=id,
    the_table=vol_table,
    query_string = "
    SELECT 
    --R{the_table}--.Date AS Date,
    --R{the_table}--.Days AS Days,
    --R{the_table}--.Strike AS Strike,
    --R{the_table}--.Delta AS Delta,
    --R{the_table}--.ImpliedVol AS ImpliedVol,
    SECURITY_PRICE.ClosePrice AS ClosePrice
    FROM --R{the_table}--
    LEFT JOIN SECURITY_PRICE 
    ON SECURITY_PRICE.SecurityID = --R{the_table}--.SecurityID
    AND SECURITY_PRICE.Date = --R{the_table}--.Date
    WHERE --R{the_table}--.SecurityID = --R{security_id}--
    ")
  vol_df<-query(the_query,db=dbma)
  dbDisconnect(dbma)
  x<-vol_df[Delta==80]
  x$Strike<-0
  y<-vol_df[Delta==(-80)]
  y$Strike<-max(y$Strike)*10
  z<-vol_df[Days==30]
  z$Days<-0
  res<-rbind(vol_df,x,y,z)
  res$Date<-fastPOSIXct(res$Date)
  res
}

# option models

EC <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  S*pnorm(d1)-X*exp(-r*t)*pnorm(d2)
}


EP  <- function(S,X,t,r,v)
{
  d1 <- (log(S/X)+(r+0.5*v^2)*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  X*exp(-r*t)*pnorm(-d2)-S*pnorm(-d1)
}

ECS<-function(S,X1,X2,t,r,v1,v2){}
EPS<-function(S,X1,X2,t,r,v1,v2){}
ESTRAG<-function(S,X1,X2,t,r,v1,v2){}
ESTRAD<-function(S,X,t,r,v){}


# apply a set of filters to all dates 
make_strategy_roll_dates<-function(filter_list,strategy_df){
  res<-Reduce(function(a,b)eval(bquote(.(a)[.(b)])),filter_list,init=strategy_df)
  res$start<-res$pday
  res$end<-c(res$pday[-1],max(strategy_df$pday))
  res
}


# date and 
make_strategy_df<-function(
  volsurf,
  filter_list=expiries<-list(
    quote(month %in% c("March","June","September","December")),
    quote(wday=="Friday"),
    quote(wday_count==3)
  )
)
{
  strategy_df <- volsurf[,.(
    date=Date[1],
    date_string=as.character(Date[1],format="%Y-%m-%d"),
    close=ClosePrice[1]
  ),keyby=Date][,.(
    date=date,
    date_string=date_string,
    close=close
  )]
  strategy_df$day <- day(strategy_df$date)
  strategy_df$mday <- mday(strategy_df$date)
  strategy_df$qday <- qday(strategy_df$date)
  strategy_df$yday <- yday(strategy_df$date)
  strategy_df$pday <- seq_along(strategy_df$date)
  strategy_df$wday <- weekdays(strategy_df$date)
  strategy_df$month <- months(strategy_df$date)
  strategy_df$wday_count <- ceiling(mday(strategy_df$date)/7)
  setkey(strategy_df,date_string)
  roll_dates<-make_strategy_roll_dates(filter_list,strategy_df)
  strategy_df$roll<-findInterval(
    strategy_df$pday,
    roll_dates$pday,
    rightmost.closed = FALSE,
    all.inside = FALSE
  )
  strategy_df<-strategy_df[roll>0 & roll<nrow(roll_dates)]
  strategy_df$roll_close<-roll_dates$close[strategy_df$roll]
  strategy_df$start<-roll_dates$start[strategy_df$roll]
  strategy_df$end<-roll_dates$end[strategy_df$roll]
  strategy_df$days<-roll_dates$end[strategy_df$roll]-strategy_df$pday
  strategy_df$yfrac<-strategy_df$days/365
  strategy_df
}



# resamples volgrid to specified strikes, maturities on a single date
resample_vol_grid<-function (vol_df,strikes,maturities,verbose=FALSE) 
{
  if(verbose){
    the_date<-as.character(vol_df$Date[1],format="%Y-%m-%d")
    cat(the_date,"\n")
  }
  vol_df<-vol_df[ImpliedVol>0]
  if(nrow(vol_df)<1)return(NULL)
  strikes_in<-sort(unique(vol_df$Strike))
  maturities_in<-sort(unique(vol_df$Days))
  j<-data.table::frank(vol_df$Strike,ties.method = "dense")
  i<-data.table::frank(vol_df$Days,ties.method = "dense")
  vol_grid<-t(apply(sparseMatrix(j=j,i=i,x=vol_df$ImpliedVol,use.last.ij=TRUE),1,function(a){
    ndx<-which(a>0)
    approx(x=ndx,y=a[ndx],xout=seq_along(a),yleft=max(a[ndx]),yright=min(a[ndx]))$y
  }))
  res<-bilinear(
    y=strikes_in,
    x=maturities_in,
    z=vol_grid,
    y0=rep(strikes,times=length(maturities)),
    x0=rep(maturities,each=length(strikes))
  )
  data.table(Strike=res$y,Days=res$x,ImpliedVol=res$z,ClosePrice=vol_df$ClosePrice[1])
}

#
# interpolate vols
# on resampled vol surface
#
interpolate_vol<-function(date,strike,days,vsurf)
{
  strikes<-sort(unique(vsurf$Strike))
  maturities<-sort(unique(vsurf$Days))
  option_dets<-data.table(
    date=date,
    lo_strike=strikes[findInterval(strike,strikes)],
    hi_strike=strikes[findInterval(strike,strikes)+1],
    lo_mat=maturities[findInterval(days,maturities)],
    hi_mat=maturities[findInterval(days,maturities)+1]
  )
  vol_ll<-merge(x=option_dets,y=vsurf,by.x=c("date","lo_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_lh<-merge(x=option_dets,y=vsurf,by.x=c("date","lo_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_hl<-merge(x=option_dets,y=vsurf,by.x=c("date","hi_strike","lo_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  vol_hh<-merge(x=option_dets,y=vsurf,by.x=c("date","hi_strike","hi_mat"),by.y=c("Date","Strike","Days"))$ImpliedVol
  t_strike<-(strike-option_dets$lo_strike)/(option_dets$hi_strike-option_dets$lo_strike)
  t_mat<-(days-option_dets$lo_mat)/(option_dets$hi_mat-option_dets$lo_mat)
  rowSums(cbind(
    vol_ll*(1-t_strike)*(1-t_mat),
    vol_hl*(t_strike)*(1-t_mat),
    vol_lh*(1-t_strike)*(t_mat),
    vol_hh*(t_strike)*(t_mat)
  ))
}

#
#
#

make_option_pnl<-function(
  model=EP,
  strike=0.9,
  dir=1,
  strategy,
  vsurf
)
{
  strikes<-strategy$roll_close*strike
  vols<-interpolate_vol(date=strategy$date,strike=strikes,days=strategy_df$days,vsurf=vsurf)
  reval<-model(strategy$close, strikes, strategy$yfrac, 0,vols)
  cash_start<-ifelse(strategy$pday==strategy$start,-reval,0)
  cash_end<-ifelse(strategy$pday==strategy$end-1,reval,0)
  data.table(
    strike=strikes,
    yfrac=strategy$yfrac,
    vol=vols,
    close=strategy$close,
    reval=round(dir*ifelse(strategy$pday==strategy$end-1,0,reval),digits=3),
    cash_start=round(dir*cash_start,digits=3),
    cash_end=round(dir*cash_end,digits=3),
    cash=round(dir*cumsum(cash_start+cash_end),digits=3),
    pnl=round(dir*(ifelse(strategy$pday==strategy$end-1,0,reval)+cumsum(cash_start+cash_end)),digits=3)
  )
}



strategy_df<-fread("strategy_df.csv")
resampled_spx_vol<- "compressed_resampled_spx_vol.txt" %>% scan(character()) %>% decompress
x<-resampled_spx_vol[Strike==3000]
x$Strike<-9999
resampled_spx_vol<-rbind(resampled_spx_vol,x)

option_strategies<-list(
  None=0,                      
  Call=1,
  Put=2,
  Stradle=3,
  CallSpread=4,
  PutSpread=5,
  RiskReversal=6,
  Butterfly=6
)

option_payoffs<-list(
  None=function(spot,strikes)return(0),
  Call=function(spot,strikes)max(spot-strikes[1],0),
  Put=function(spot,strikes)max(strikes[1]-spot,0),
  CallSpread=function(spot,strikes)max(spot-strikes[1],0)-max(spot-strikes[2]),
  PutSpread=function(spot,strikes)max(strikes[1]-spot,0)-max(strikes[2]-spot,0),
  RiskReversal=function(spot,strikes)max(spot-strikes[1],0)-max(strikes[2]-spot,0),
  Butterfly=function(spot,strikes)max(spot-strikes[1],0)-2*max(spot-strikes[2],2)+max(strikes[3]-spot,0)
)

payoffs<-rbind(
  data.table(
    name="None",model=function(...)0,strike_count=0,size=numeric(0),payoff=function(spot,strikes)return(0)
  ),
  data.table(
    name="Call",model=EC,strike_count=1,size=c(1),payoff=function(spot,strikes)return(0)
  ),
  data.table(
    name="Put",model=EP,strike_count=1,size=c(1),payoff=function(spot,strikes)return(0)
  ),
  data.table(
    name="CallSpread",model=ECS,strike_count=2,size=c(1,-1),payoff=function(spot,strikes)return(0)
  ),
  data.table(
    name="PutSpread",model=EPS,strike_count=2,size=c(-1,1),payoff=function(spot,strikes)return(0)
  ),
  data.table(
    name="Strangle",model=ESTRAG,strike_count=2,size=c(1,1),payoff=function(spot,strikes)return(0)
  ),
  data.table(
    name="Straddle",model=ESTRAD,strike_count=1,size=c(1),payoff=function(spot,strikes)return(0)
  ),
  data.table(
    name="Butterfly",model=ESTRAD,strike_count=3,size=c(1,-2,1),payoff=function(spot,strikes)return(0)
  )
)[,c(.SD,list(select=seq_along(name)))]



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Rolling SPX option backtest"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(width=3,h3("Payoff")),
          column(width=2,h3("Direction")),
          column(width=7,h3("Strikes"))
        ),
        tags$hr(),
        fluidRow(
          column(width=3,selectInput("o1_type",label=NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
          column(width=2,selectInput("o1_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=4)),
          column(width=7,uiOutput("o1_strikes"))
        ),
        tags$hr(),
        fluidRow(
          column(width=3,selectInput("o2_type", label = NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
          column(width=2,selectInput("o2_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=3)),
          column(width=7,uiOutput("o2_strikes"))
        ),
        tags$hr(),
        fluidRow(actionButton("backtest",h2("GO!")))
      ),
    # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("summary"),
        plotOutput("backtestPlot"),
        plotOutput("backtestHist")
      )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  output$o1_strikes<-renderUI({
    
    o1_type<-as.integer(input$o1_type)
    if(o1_type<1)return(NULL)
    
    o1_option<-as.list(payoffs[o1_type,])
    
    noUiSliderInput(
      inputId="o1_strikes_slider",
      label=NULL,
      min = 0.75,
      max = 1.25,
      value = seq(from=0.75,to=1.25,length.out = o1_option$strike_count)
    )
    
    
  })
  
   output$o2_strikes<-renderUI({
    
    o2_type<-as.integer(input$o2_type)
    if(o2_type<1)return(NULL)
    
    o2_option<-as.list(payoffs[o2_type,])
    
    noUiSliderInput(
      inputId="o2_strikes_slider",
      label=NULL,
      min = 0.75,
      max = 1.25,
      value = seq(from=0.75,to=1.25,length.out = o2_option$strike_count)
    )
    
    
  })
   
  pnl <-reactive({
    
    input$backtest
    
    #
    o1_strikes<-isolate(input$o1_strikes_slider)
    if(length(o1_strikes)<1)return(NULL)
    o1_type<-isolate(as.integer(input$o1_type))
    if(o1_type<1)return(NULL)
    o1_option<-as.list(payoffs[o1_type,])
    o1_direction<-isolate(input$o1_direction)
    
    #
    o2_strikes<-isolate(input$o2_strikes_slider)
    if(length(o2_strikes)<1)return(NULL)
    o2_type<-isolate(as.integer(input$o2_type))
    if(o2_type<1)return(NULL)
    o2_option<-as.list(payoffs[o2_type,])
    o2_direction<-isolate(input$o2_direction)
    
    direction<-c(Long=1,Short=-1)
    
    
    if(length(o1_strikes)>0){
      o1_results<-mapply(
        function(model,strike,dir){
        make_option_pnl(
          model=model,
          strike=strike,
          dir=dir,
          strategy=strategy_df,
          vsurf=resampled_spx_vol
        )$pnl
        },
        strike=o1_strikes,
        dir=direction[as.integer(o1_direction)]*o1_option$size,
        MoreArgs=list(model=o1_option$model[[1]]),
        SIMPLIFY=FALSE
      )
    }else{
      o1_results<-list()
    }
    
    if(length(o2_strikes)>0){
      o2_results<-mapply(
        function(model,strike,dir){
          make_option_pnl(
            model=model,
            strike=strike,
            dir=dir,
            strategy=strategy_df,
            vsurf=resampled_spx_vol
          )$pnl
        },
        strike=o2_strikes,
        dir=direction[as.integer(o2_direction)]*o2_option$size,
        MoreArgs=list(model=o2_option$model[[1]],dir=direction[as.integer(o2_direction)]),
        SIMPLIFY=FALSE
      )
    }else{
      o2_results<-list()
    }
    
    all_results<-c(o1_results,o2_results)
    
    if(length(all_results)<1)return(NULL)
    
    strategy_pnl<-data.table( 
      date=fastPOSIXct(strategy_df$date), 
      pnl=rowSums(do.call(cbind,all_results))
    )
    
    list(
      strategy_pnl=strategy_pnl,
      o1_option=o1_option,
      o1_direction=o1_direction,
      o2_option=o2_option,
      o2_direction=o2_direction
    )
    
  })
  
  output$summary<-renderText({
    strategy_pnl<-pnl()
    if(is.null(strategy_pnl))return(NULL)
    final_pnl<-tail(strategy_pnl$strategy_pnl$pnl,1)
    max_draw<-max(cummax(strategy_pnl$strategy_pnl$pnl)-strategy_pnl$strategy_pnl$pnl)
    paste(
      paste0("P&L      : ",comma(final_pnl,digits=0)),
      paste0("Drawdown : ",comma(max_draw,digits=0)),
      paste0("Ratio    : ",round(final_pnl/max_draw,digits=2)),
      "\n",
      sep="\n"
    )
  })
  
  output$backtestPlot <- renderPlot({
     strategy_pnl<-pnl()
     if(is.null(strategy_pnl))return(NULL)
     g1<-strategy_pnl$strategy_pnl %>% ggplot() + 
      geom_line(aes(x=date,y=pnl)) +
      geom_vline(xintercept = strategy_pnl$strategy_pnl$date[which(strategy_df$days==1)],col="red",alpha=0.25) 
     
    plot(g1)
   })
  
  output$backtestHist <- renderPlot({
    strategy_pnl<-pnl()
    if(is.null(strategy_pnl))return(NULL)
    g1<-strategy_pnl$strategy_pnl %>% ggplot() + geom_histogram(aes(c(0,diff(pnl))),bins=100)
    plot(g1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
