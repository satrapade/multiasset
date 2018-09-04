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
require(RcppRoll)
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

ES<-function(S,X,t,r,v)EP(S,X,t,r,v)+EC(S,X,t,r,v)


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

#
#
#

backtest_option<-function(option){
  
  if(length(option$strike[[1]])<1)return(list(data.table(
    strike=0,
    yfrac=strategy_df$yfrac,
    vol=rep(0,nrow(strategy_df)),
    close=strategy_df$close,
    reval=rep(0,nrow(strategy_df)),
    cash_start=rep(0,nrow(strategy_df)),
    cash_end=rep(0,nrow(strategy_df)),
    cash=rep(0,nrow(strategy_df)),
    pnl=rep(0,nrow(strategy_df))
  )$pnl))
  
  option_results<-mapply(
    function(model,strike,dir){
      make_option_pnl(
        model=model,
        strike=strike,
        dir=dir,
        strategy=strategy_df,
        vsurf=resampled_spx_vol
      )$pnl
    },
    strike=option$strike[[1]],
    dir=option$size[[1]],
    model=option$model[[1]],
    SIMPLIFY=FALSE
  )
  
  option_results
  
}

dof<-fread("dof.csv")
dof$Date<-as.Date(dof$Date,format="%Y-%m-%d")
dof$PnL<-cumsum(dof$PnL)

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

#
# to be used by the app
#

app_env<-environment()

payoffs<-rbind(
  data.table( # No model
    name="None",model=list(list()),strike=list(),size=list()
  ),
  data.table( # European-style call
    name="Call",model=list(list(EC)),strike=list(c(1)),size=list(c(1))
  ),
  data.table( # European-style pus
    name="Put",model=list(list(EP)),strike=list(c(1)),size=list(c(1))
  ),
  data.table( # Call spread
    name="CallSpread",model=list(list(EC,EC)),strike=list(c(1,1.15)),size=list(c(1,-1))
  ),
  data.table( # Put spread
    name="PutSpread",model=list(list(EP,EP)),strike=list(c(0.85,1)),size=list(c(-1,1))
  ),
  data.table( # Strangle
    name="Strangle",model=list(list(EP,EC)),strike=list(c(0.85,1.15)),size=list(c(1,1))
  ),
  data.table( # Straddle
    name="Straddle",model=list(list(ES)),strike=list(c(1)),size=list(c(1))
  ),
  data.table( # Butterfly
    name="Butterfly",model=list(list(EP,EP,EP)),strike=list(c(0.85,1,1.15)),size=list(c(1,-2,1))
  )
)[,c(.SD,list(select=seq_along(name)))]

payoff_1<-payoffs[1,]
payoff_2<-payoffs[1,]
payoff_3<-payoffs[1,]

model_payoff<-function(payoff){
  
  if(length(payoff$strike[[1]])<1)return(rep(0,25))
  MoreArgs<-list(
    models=payoff$model[[1]],
    strikes=payoff$strike[[1]],
    sizes=payoff$size[[1]]
  ) 
  
  spots<-seq(0.75,1.25,length.out=25)
  
  payoff_fun<-function(spot,models,strikes,sizes)sum(mapply(
      function(spot,model,strike,size)size*model(S=spot,X=strike,t=0.01,r=0,v=0.1),
      model=models,
      strike=strikes,
      size=sizes,
      MoreArgs=list(spot=spot)
  ))
  
  
  mapply(payoff_fun, spot=spots, MoreArgs = MoreArgs)
  
}

make_backtest<-function(options,sl,w)
{
    all_results<-mapply(function(o)do.call(cbind,backtest_option(o)),options,SIMPLIFY = FALSE)
  
    if(length(all_results)<1)return(NULL)
    
    result_matrix<-do.call(cbind,all_results)
    all_pnl<-rowSums(result_matrix)
    
    cum_max_pnl<-c(cummax(all_pnl[1:(w-1)]),roll_max(all_pnl,w))
    drawdown<-cum_max_pnl-all_pnl
    drawdown_by_roll_period<-mapply(max,split(drawdown,strategy_df$roll))
    roll_drawdown<-c(0,drawdown_by_roll_period)[strategy_df$roll]
    
    
    stopped_pnl<-cumsum(c(0,diff(all_pnl))*(roll_drawdown<sl))
    
    strategy_pnl<-data.table( 
      date=as.Date(strategy_df$date,format="%Y-%m-%d"), 
      pnl=stopped_pnl
    )
    
    list(
      strategy_pnl=strategy_pnl,
      options=options
    )
}

    
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Rolling SPX option backtest"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fluidRow(width=12,plotOutput("payoff_plot",height="100px")),
        tags$hr(),
        fluidRow(
          column(width=6,noUiSliderInput(
            inputId="sl_slider", label="Training stopLoss (points)", min=0, max=2000, step=10, value=2000
          )),
          column(width=6,noUiSliderInput(
            inputId="w_slider", label="Window (days)", min=0, max=300, step=10, value=300
          ))
        ),
        fluidRow(
          column(width=6, radioButtons("strike_select", "Strike selection",c(
                 "As selected" = "asis",
                 "Zero cost on size of second structure" = "zero_size_2",
                 "Zero cost on stikes of second structure" = "zero_strike_2"
          ))),
          column(width=6, radioButtons("plot_select", "Plot select",c(
                 "Whole period" = "whole",
                 "Live IDOF period" = "idof"
          )))
        ),
        tags$hr(),
        fluidRow(
          column(width=3,h3("Payoff")),
          column(width=3,h3("Direction")),
          column(width=6,h3("Strikes"))
        ),
        tags$hr(),
        tabsetPanel(
          tabPanel(title="Structure1",
            fluidRow(checkboxInput("o1_enable", "Active", TRUE)),
            fluidRow(width=12,uiOutput("o1_strikes")),
            fluidRow(width=12,plotOutput("o1_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o1_type",label=NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o1_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=4)),
              column(width=6,noUiSliderInput(inputId="o1_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          ),
          tabPanel(title="Structure2",
            fluidRow(checkboxInput("o2_enable", "Active", TRUE)),
            fluidRow(width=12,uiOutput("o2_strikes")),
            fluidRow(width=12,plotOutput("o2_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o2_type", label = NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o2_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=3)),
              column(width=6,noUiSliderInput(inputId="o2_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          ),
          tabPanel(title="Structure3",
            fluidRow(checkboxInput("o3_enable", "Active", TRUE)),
            fluidRow(width=12,uiOutput("o3_strikes")),
            fluidRow(width=12,plotOutput("o3_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o3_type", label = NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o3_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=3)),
              column(width=6,noUiSliderInput(inputId="o3_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          ),
          tabPanel(title="Structure4",
            fluidRow(checkboxInput("o4_enable", "Active", TRUE)),
            fluidRow(width=12,uiOutput("o4_strikes")),
            fluidRow(width=12,plotOutput("o4_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o4_type", label = NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o4_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=3)),
              column(width=6,noUiSliderInput(inputId="o4_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          )
        ),
        tags$hr(),
        fluidRow(actionButton("backtest",h2("BACKTEST")))
      ),
    # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("summary"),
        plotOutput("backtestPlot"),
        plotOutput("dofPlot")
      )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
#
# strike sliders
#
  
output$o1_strikes<-renderUI({
    o1_type<-as.integer(input$o1_type)
    if(o1_type<1)return(NULL)
    o1_option<-as.list(payoffs[o1_type,])
    noUiSliderInput(
      inputId="o1_strikes_slider", label=NULL, min=0.75, max=1.25, step=0.01, value=o1_option$strike[[1]]
    )
})
  
output$o2_strikes<-renderUI({
    o2_type<-as.integer(input$o2_type)
    if(o2_type<1)return(NULL)
    o2_option<-as.list(payoffs[o2_type,])
    noUiSliderInput(
      inputId="o2_strikes_slider", label=NULL, min=0.75, max=1.25, step=0.01, value=o2_option$strike[[1]]
    )
})
  
output$o3_strikes<-renderUI({
    o3_type<-as.integer(input$o3_type)
    if(o3_type<1)return(NULL)
    o3_option<-as.list(payoffs[o3_type,])
    noUiSliderInput(
      inputId="o3_strikes_slider", label=NULL, min=0.75, max=1.25, step=0.01, value=o3_option$strike[[1]]
    )
})

output$o4_strikes<-renderUI({
    o4_type<-as.integer(input$o4_type)
    if(o4_type<1)return(NULL)
    o4_option<-as.list(payoffs[o4_type,])
    noUiSliderInput(
      inputId="o4_strikes_slider", label=NULL, min=0.75, max=1.25, step=0.01, value=o4_option$strike[[1]]
    )
})

#
# payoffs
#
  
selected_payoff_1<-reactive({
    o1_type<-as.integer(input$o1_type)
    if(o1_type<1)return(NULL)
    o1_direction<-c(Long=1,Short=-1)[as.integer(input$o1_direction)]
    app_env$payoff_1<-payoffs[o1_type,]
    if(length(app_env$payoff_1$size[[1]])>0){
      app_env$payoff_1$size<-list(list(app_env$payoffs[o1_type,]$size[[1]]*o1_direction*input$o1_size))
    }
    o1_strikes<-input$o1_strikes_slider
    if(
      length(app_env$payoff_1$strike)>0 & 
      length(app_env$payoff_1$strike[[1]])==length(o1_strikes)
    ){
      app_env$payoff_1$strike<-list(list(o1_strikes))
    }
    app_env$payoff_1
})

selected_payoff_2<-reactive({
   o2_type<-as.integer(input$o2_type)
   if(o2_type<1)return(NULL)
   o2_direction<-c(Long=1,Short=-1)[as.integer(input$o2_direction)]
   app_env$payoff_2<-payoffs[o2_type,]
   if(length(app_env$payoff_2$size[[1]])>0){
    app_env$payoff_2$size<-list(list(app_env$payoffs[o2_type,]$size[[1]]*o2_direction*input$o2_size))
   }
   o2_strikes<-input$o2_strikes_slider
   if(
     length(app_env$payoff_2$strike)>0 & 
     length(app_env$payoff_2$strike[[1]])==length(o2_strikes)
   ){
     app_env$payoff_2$strike<-list(list(o2_strikes))
   }
   app_env$payoff_2
})
 
selected_payoff_3<-reactive({
   o3_type<-as.integer(input$o3_type)
   if(o3_type<1)return(NULL)
   o3_direction<-c(Long=1,Short=-1)[as.integer(input$o3_direction)]
   app_env$payoff_3<-payoffs[o3_type,]
   if(length(app_env$payoff_3$size[[1]])>0){
    app_env$payoff_3$size<-list(list(app_env$payoffs[o3_type,]$size[[1]]*o3_direction*input$o3_size))
   }
   o3_strikes<-input$o3_strikes_slider
   if(
     length(app_env$payoff_3$strike)>0 & 
     length(app_env$payoff_3$strike[[1]])==length(o3_strikes)
   ){
     app_env$payoff_3$strike<-list(list(o3_strikes))
   }
   app_env$payoff_3
})

selected_payoff_4<-reactive({
   o4_type<-as.integer(input$o4_type)
   if(o4_type<1)return(NULL)
   o4_direction<-c(Long=1,Short=-1)[as.integer(input$o4_direction)]
   app_env$payoff_4<-payoffs[o4_type,]
   if(length(app_env$payoff_4$size[[1]])>0){
    app_env$payoff_4$size<-list(list(app_env$payoffs[o4_type,]$size[[1]]*o4_direction*input$o4_size))
   }
   o4_strikes<-input$o4_strikes_slider
   if(
     length(app_env$payoff_4$strike)>0 & 
     length(app_env$payoff_4$strike[[1]])==length(o4_strikes)
   ){
     app_env$payoff_4$strike<-list(list(o4_strikes))
   }
   app_env$payoff_4
})

#
#
#

output$o1_payoff_plot <- renderPlot({
      the_payoff<-selected_payoff_1()
      o1_pay<-model_payoff(the_payoff)*input$o1_enable  
      op<-par()$mai
      par(mai=c(0,0,0,0))
      plot(o1_pay,axes=FALSE,xlab="",ylab="",type="l",lwd=3)
      par(mai=op)
})
  
output$o2_payoff_plot <- renderPlot({
    the_payoff<-selected_payoff_2()
    o2_pay<-model_payoff(the_payoff)*input$o2_enable   
    op<-par()$mai
    par(mai=c(0,0,0,0))
    plot(o2_pay,axes=FALSE,xlab="",ylab="",type="l",lwd=3)
    par(mai=op)
})

output$o3_payoff_plot <- renderPlot({
    the_payoff<-selected_payoff_3()
    o3_pay<-model_payoff(the_payoff)*input$o3_enable    
    op<-par()$mai
    par(mai=c(0,0,0,0))
    plot(o3_pay,axes=FALSE,xlab="",ylab="",type="l",lwd=3)
    par(mai=op)
})
  
output$o4_payoff_plot <- renderPlot({
    the_payoff<-selected_payoff_4()
    o4_pay<-model_payoff(the_payoff)*input$o4_enable    
    op<-par()$mai
    par(mai=c(0,0,0,0))
    plot(o4_pay,axes=FALSE,xlab="",ylab="",type="l",lwd=3)
    par(mai=op)
})
  

output$payoff_plot <- renderPlot({
    f<-c(
      input$o1_enable,
      input$o2_enable,
      input$o3_enable,
      input$o4_enable
    )
    p<-mapply(model_payoff,
      p=list(
        selected_payoff_1(),
        selected_payoff_2(),
        selected_payoff_3(),
        selected_payoff_4()
      )[f],
      SIMPLIFY = FALSE
    )
    both_pay<-rowSums(do.call(cbind,p))
    op<-par()$mai
    par(mai=c(0,0,0,0))
    plot(both_pay,axes=FALSE,xlab="",ylab="",type="l",lwd=3)
    par(mai=op)
})

#
#
#

pnl <-reactive({
    input$backtest
    f<-c(
      input$o1_enable,
      input$o2_enable,
      input$o3_enable,
      input$o4_enable
    )
    res<-make_backtest(
      list(
        isolate(selected_payoff_1()),
        isolate(selected_payoff_2()),
        isolate(selected_payoff_3()),
        isolate(selected_payoff_4())
      )[f],
      sl=isolate(input$sl_slider),
      w=isolate(input$w_slider)
    )
    res
})

#
# main panel
#

output$summary<-renderText({
    strategy_pnl<-pnl()
    common_dates<-intersect(as.character(dof$Date),as.character(strategy_pnl$strategy_pnl$date))
    i<-which(as.character(strategy_pnl$strategy_pnl$date) %in% common_dates)
    a<-diff(strategy_pnl$strategy_pnl$pnl[i])
    j<-which(as.character(dof$Date) %in% common_dates)
    b<-diff(dof$PnL[j])
    final_pnl<-tail(strategy_pnl$strategy_pnl$pnl,1)
    max_draw<-max(cummax(strategy_pnl$strategy_pnl$pnl)-strategy_pnl$strategy_pnl$pnl)
    paste(
      paste0("P&L      : ",comma(final_pnl,digits=0)),
      paste0("Drawdown : ",comma(max_draw,digits=0)),
      paste0("Ratio    : ",round(final_pnl/max_draw,digits=2)),
      paste0("Correl   : ",round(100*cor(a,b),digits=2)),
      "\n",
      sep="\n"
    )
})
  
output$backtestPlot <- renderPlot({
     strategy_pnl<-pnl()
     common_dates<-intersect(as.character(dof$Date),as.character(strategy_pnl$strategy_pnl$date))
     if(input$plot_select=="idof"){
      i<-which(as.character(strategy_pnl$strategy_pnl$date) %in% common_dates)
     }
     if(input$plot_select=="whole"){
      i<-1:nrow(strategy_pnl$strategy_pnl)
     }
     g1<- strategy_pnl$strategy_pnl[i]%>% ggplot() + 
      geom_line(aes(x=date,y=pnl)) + 
      ggtitle("BACKTEST performance") +
      geom_vline(xintercept = strategy_pnl$strategy_pnl$date[which(strategy_df$days==1)],col="red",alpha=0.25) 
     
    plot(g1)
    
})
  
output$dofPlot <- renderPlot({
    strategy_pnl<-pnl()
    common_dates<-intersect(as.character(dof$Date),as.character(strategy_pnl$strategy_pnl$date))
    if(input$plot_select=="idof"){
      i<-which(as.character(dof$Date) %in% common_dates)
    }
     if(input$plot_select=="whole"){
      i<-1:nrow(dof)
    }
    g1<- ggplot(data=dof[i]) + 
      geom_line(aes(x=Date,y=PnL)) + 
      ggtitle("IDOF performance") +
      geom_vline(xintercept = strategy_pnl$strategy_pnl$date[which(strategy_df$days==1)],col="red",alpha=0.25) 
    
    plot(g1)
    
})


}

# Run the application 
shinyApp(ui = ui, server = server)


