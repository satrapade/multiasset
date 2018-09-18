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
require(DT)

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
  vols<-interpolate_vol(date=strategy$date,strike=strikes,days=strategy$days,vsurf=vsurf)
  reval<-model(strategy$close, strikes, strategy$yfrac, 0,vols)/strategy$roll_close
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

backtest_option<-function(option,strategy=spx_strategy_df,vsurf=resampled_spx_vol){
  
  if(length(option$strike[[1]])<1)return(list(data.table(
    strike=0,
    yfrac=strategy$yfrac,
    vol=rep(0,nrow(strategy)),
    close=strategy$close,
    reval=rep(0,nrow(strategy)),
    cash_start=rep(0,nrow(strategy)),
    cash_end=rep(0,nrow(strategy)),
    cash=rep(0,nrow(strategy)),
    pnl=rep(0,nrow(strategy))
  )$pnl))
  
  option_results<-mapply(
    function(model,strike,dir,strategy,vsurf){
      make_option_pnl(
        model=model,
        strike=strike,
        dir=dir,
        strategy=strategy,
        vsurf=vsurf
      )$pnl
    },
    strike=option$strike[[1]],
    dir=option$size[[1]],
    model=option$model[[1]],
    MoreArgs=list(strategy=strategy,vsurf=vsurf),
    SIMPLIFY=FALSE
  )
  
  option_results
  
}

dof<-fread("dof.csv")
dof$Date<-as.Date(dof$Date,format="%Y-%m-%d")
dof$PnL<-cumsum(dof$PnL)

strategy_df<-fread("strategy_df.csv")

spx_strategy_df<-fread("spx_strategy_df.csv")
ndx_strategy_df<-fread("ndx_strategy_df.csv")
dax_strategy_df<-fread("dax_strategy_df.csv")
sx5e_strategy_df<-fread("dax_strategy_df.csv")

resampled_spx_vol<-"compressed_resampled_spx_vol.txt" %>% scan(character()) %>% decompress
resampled_ndx_vol<-"compressed_resampled_ndx_vol.txt" %>% scan(character()) %>% decompress
resampled_dax_vol<-"compressed_resampled_dax_vol.txt" %>% scan(character()) %>% decompress
resampled_sx5e_vol<-"compressed_resampled_sx5e_vol.txt" %>% scan(character()) %>% decompress

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
payoff_4<-payoffs[1,]

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

apply_stop<-function(px,stop_level,rolls=floor(seq(1,length(px),length.out = 10)))data.table(
  tret=c(0,diff(px)),
  px=px
)[,
  c(.SD,list(
    start_px=rep(px[rolls],times=c(diff(rolls),length(px)-tail(rolls,1)+1)),
    roll=rep(rolls,times=c(diff(rolls),length(px)-tail(rolls,1)+1))
  ))
][,
  c(.SD,list(dd=px-start_px))
][,
  c(.SD,list(sl=c(0,head(cummax(dd<(-stop_level)),-1)))),
  keyby=roll
][,
  c(.SD,list(stopped_tret=ifelse(sl>0,0,tret)))
][,
  c(.SD,list(stopped_px=cumsum(stopped_tret)))
]

    
make_backtest<-function(options,sl,strategy,vsurf)
{
    if(length(options)<1)return(data.table(
        date=as.Date(strategy$date,format="%Y-%m-%d"), 
        pnl=0
    ))
    all_results<-mapply(
      function(o,strategy,vsurf){
        do.call(cbind,backtest_option(option=o,strategy=strategy,vsurf=vsurf))
      },
      o=options,
      MoreArgs = list(strategy=strategy,vsurf=vsurf),
      SIMPLIFY = FALSE
    )
  
    if(length(all_results)<1)return(NULL)
    
    result_matrix<-do.call(cbind,all_results)
    all_pnl<-rowSums(result_matrix)
    
    a<-apply_stop(px=all_pnl,stop_level=sl,rolls=which(strategy$pday==strategy$start))
    stopped_pnl<-a$stopped_px
    
    strategy_pnl<-data.table( 
      date=as.Date(strategy$date,format="%Y-%m-%d"), 
      pnl=stopped_pnl,
      pday=strategy$pday
    )
    
    strategy_pnl
}


x<-make_backtest(
 options=list(payoffs[2,],payoffs[3,]),
 sl=0.25,
 strategy=spx_strategy_df,
 vsurf=resampled_spx_vol
)

underlyings<-c("SPX", "NDX", "DAX","SX5E")


make_outcome<-function(strategy,backtest){
  outcome<-strategy[
    start==pday,
    .(
        roll=roll,
        date=date_string,
        wday=wday,
        mday=mday,
        month=month,
        close=close,
        start=start,
        end=end,
        days=days,
        yfrac=yfrac,
        pnl=mapply(
            function(s,e)100*(backtest$pnl[e]-backtest$pnl[s]),
            s=strategy[start==pday]$start,
            e=strategy[start==pday]$end
        ),
        dd=mapply(
            function(s,e)round(100*(max(cummax(backtest$pnl[s:e])-backtest$pnl[s:e])),digits=2),
            s=strategy[start==pday]$start,
            e=strategy[start==pday]$end
        )
    )
  ]
  outcome
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Option strategy backtest"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fluidRow(width=12,plotOutput("payoff_plot",height="100px")),
        tags$hr(),
        fluidRow(
          column(width=6,noUiSliderInput(
            inputId="sl_slider", label="Trailing stopLoss (pct of roll close)", min=0, max=20, step=0.25, value=20
          ))
        ),
        fluidRow(
          column(width=4,verticalLayout(
            selectizeInput(
              inputId = "underlying_select", 
              "Live Underlyings", 
              choices=underlyings, 
              selected = "SPX", 
              multiple = TRUE,
               options = NULL
            ),
            selectizeInput(
              inputId = "structure_select", 
              "Live Structures", 
              choices=c("Structure1","Structure2","Structure3","Structure4"), 
              selected = "Structure1", 
              multiple = TRUE,
               options = NULL
            )
          )),
          column(width=6, selectizeInput(
            inputId="strike_select", 
            label="Strike Calculation",
            choices=c(
              "NoSolve",
              "SolveZeroCostStructure1",
              "SolveZeroCostStructure2",
              "SolveZeroCostStructure3",
              "SolveZeroCostStructure4"
            ),
            selected="NoSolve",
            multiple=FALSE,
            options=NULL
          ))
        ),
        tags$hr(),
        tabsetPanel(
          tabPanel(title="Structure1",
            fluidRow(column(width=12,uiOutput("o1_strikes"))),
            fluidRow(width=12,plotOutput("o1_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o1_type",label=NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o1_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=1)),
              column(width=6,noUiSliderInput(inputId="o1_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          ),
          tabPanel(title="Structure2",
            fluidRow(width=12,uiOutput("o2_strikes")),
            fluidRow(width=12,plotOutput("o2_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o2_type", label = NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o2_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=1)),
              column(width=6,noUiSliderInput(inputId="o2_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          ),
          tabPanel(title="Structure3",
            fluidRow(width=12,uiOutput("o3_strikes")),
            fluidRow(width=12,plotOutput("o3_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o3_type", label = NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o3_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=1)),
              column(width=6,noUiSliderInput(inputId="o3_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          ),
          tabPanel(title="Structure4",
            fluidRow(width=12,uiOutput("o4_strikes")),
            fluidRow(width=12,plotOutput("o4_payoff_plot",height="100px")),
            fluidRow(
              column(width=3,selectInput("o4_type", label = NULL, choices=setNames(payoffs$select,payoffs$name),selected=1)),
              column(width=3,selectInput("o4_direction", label = NULL, choices = list("Long" = 1, "Short" = 2),selected=1)),
              column(width=6,noUiSliderInput(inputId="o4_size", label=NULL, min=0, max=1, step=0.01,value=1))
            )
          )
        ),
        tags$hr(),
        fluidRow(actionButton("backtest",h2("BACKTEST")))
      ),
    # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
            tabPanel(title="Stats", verbatimTextOutput("summary")),
            tabPanel(title="Plots", plotOutput("backtestPlot") ),
            tabPanel(title="Outcomes",tabsetPanel(
              fluidRow(
                column(width=12,selectizeInput(
                  inputId = "roll_underlying_select", 
                  "Underlying", 
                  choices=underlyings, 
                  selected = "SPX", 
                  multiple = FALSE,
                  options = NULL
              ))),
              fluidRow(
                column(width=6,DT::dataTableOutput("strategy_outcomes")),
                column(width=6,plotOutput("outcome_plot"))
              )
            ))
        )
      )
  )
)


make_payoff_plot<-function(p){
  op<-par()$mai
  par(mai=c(0,0,0,0))
  plot(model_payoff(p),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
  par(mai=op)
}

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

selected_structures<-reactive({
    f<-c("Structure1","Structure2","Structure3","Structure4") %in% input$structure_select
    p<-list(selected_payoff_1(), selected_payoff_2(), selected_payoff_3(), selected_payoff_4() )[f]
    p  
})

#
# plot
#

output$o1_payoff_plot <- renderPlot({make_payoff_plot(selected_payoff_1())})
output$o2_payoff_plot <- renderPlot({make_payoff_plot(selected_payoff_2())})
output$o3_payoff_plot <- renderPlot({make_payoff_plot(selected_payoff_3())})
output$o4_payoff_plot <- renderPlot({make_payoff_plot(selected_payoff_4())})


output$payoff_plot <- renderPlot({
  
    p<-selected_structures()
    if(length(p)<1)return(NULL)
    p<-mapply(model_payoff,p=p,SIMPLIFY = FALSE)
    
    both_pay<-rowSums(do.call(cbind,p))
    
    op<-par()$mai
    par(mai=c(0,0,0,0))
    plot(both_pay,axes=FALSE,xlab="",ylab="",type="l",lwd=3)
    par(mai=op)
    
})

#
# pnl calculation
#



spx_pnl<-reactive({
    input$backtest
    p<-isolate(selected_structures())
    sl<-isolate(input$sl_slider)/100
    spx_res<-make_backtest(p,sl=sl,strategy=spx_strategy_df,vsurf=resampled_spx_vol)
    spx_res
})

ndx_pnl<-reactive({
    input$backtest
    p<-isolate(selected_structures())
    sl<-isolate(input$sl_slider)/100
    ndx_res<-make_backtest(p,sl=sl,strategy=ndx_strategy_df,vsurf=resampled_ndx_vol)
    ndx_res
})

dax_pnl<-reactive({
    input$backtest
    p<-isolate(selected_structures())
    sl<-isolate(input$sl_slider)/100
    dax_res<-make_backtest(p,sl=sl,strategy=dax_strategy_df,vsurf=resampled_dax_vol)
    dax_res
})

sx5e_pnl<-reactive({
    input$backtest
    p<-isolate(selected_structures())
    sl<-isolate(input$sl_slider)/100
    sx5e_res<-make_backtest(p,sl=sl,strategy=sx5e_strategy_df,vsurf=resampled_sx5e_vol)
    sx5e_res
})


pnl <-reactive({
  
    spx_res<-spx_pnl()
    ndx_res<-ndx_pnl()
    dax_res<-dax_pnl()
    sx5e_res<-sx5e_pnl()
    
    common_dates<-Reduce(intersect,list(
      as.character(spx_res$date),
      as.character(ndx_res$date),
      as.character(dax_res$date),
      as.character(sx5e_res$date)
    ))
    
    common_pnl<-cbind(
      spx_res[as.character(date) %in% common_dates,pnl],
      ndx_res[as.character(date) %in% common_dates,pnl],
      dax_res[as.character(date) %in% common_dates,pnl],
      sx5e_res[as.character(date) %in% common_dates,pnl]
    )
    u<-isolate(input$underlying_select)
    ptf<-cbind(underlyings %in% u)/max(length(u),1)
    
    reval_date<-as.Date(common_dates,format="%Y-%m-%d")
    maturity<-spx_strategy_df[date_string %in% common_dates,days]
    roll<-spx_strategy_df[date_string %in% common_dates,start==pday]
    ptf_pnl<-drop(common_pnl %*% ptf)
    
    data.table(
      date=reval_date,
      pnl=ptf_pnl,
      day=maturity,
      roll=roll
    )
    
    
})

#
# main panel
#
# summary
# backtest plot
# IDOF plot
#

output$summary<-renderText({
  
    strategy_pnl<-pnl()
    common_dates<-intersect(as.character(dof$Date),as.character(strategy_pnl$date))
    i<-which(as.character(strategy_pnl$date) %in% common_dates)
    a<-diff(strategy_pnl$pnl[i])
    j<-which(as.character(dof$Date) %in% common_dates)
    b<-diff(dof$PnL[j])
    final_pnl<-tail(strategy_pnl$pnl,1)
    max_draw<-max(cummax(strategy_pnl$pnl)-strategy_pnl$pnl)
    paste(
      paste0("P&L      : ",comma(100*final_pnl,digits=2)),
      paste0("Drawdown : ",comma(100*max_draw,digits=0)),
      paste0("Ratio    : ",round(final_pnl/max_draw,digits=2)),
      paste0("Correl   : ",round(100*cor(roll_mean(a,14),roll_mean(b,14)),digits=2)),
      "\n",
      sep="\n"
    )
    
})
  
output$backtestPlot <- renderPlot({
  
    strategy_pnl<-pnl()
    common_dates<-intersect(as.character(dof$Date),as.character(strategy_pnl$date))
    i<-which(as.character(strategy_pnl$date) %in% common_dates)
    j<-which(as.character(dof$Date) %in% common_dates)
    roll_dates<-strategy_pnl[roll==TRUE,date]
     
    df<-rbind(
      strategy_pnl[i,.(date=date,pnl=pnl,source="backtest")],
      dof[j,.(date=Date,pnl=PnL,source="dof")]
    )[,.(date=date,pnl=rescale(pnl)),keyby="source"]
    
    g1 <- df%>% ggplot() + 
      geom_line(aes(x=date,y=pnl,col=source),size=2,alpha=0.5) + 
      ggtitle("BACKTEST vs IDOF performance") +
      geom_vline(xintercept = roll_dates,col="red",alpha=0.25) 
     
     plot(g1)
    
})
  


#
#
#

spx_outcome  <- reactive({ make_outcome(spx_strategy_df,spx_pnl())   })
ndx_outcome  <- reactive({ make_outcome(ndx_strategy_df,ndx_pnl())   })
dax_outcome  <- reactive({ make_outcome(dax_strategy_df,dax_pnl())   })
sx5e_outcome <- reactive({ make_outcome(sx5e_strategy_df,sx5e_pnl()) })

selected_outcome<-reactive({
   strategy_name<-input$roll_underlying_select
    if(length(strategy_name)<1)return(spx_outcome())
    outcome<-switch(
        strategy_name,
        SPX=spx_outcome(),
        NDX=ndx_outcome(),
        DAX=dax_outcome(),
        SX5E=sx5e_outcome()
    )
    outcome
})

selected_pnl<-reactive({
   strategy_name<-input$roll_underlying_select
    if(length(strategy_name)<1)return(spx_outcome())
    pnl<-switch(
        strategy_name,
        SPX=spx_pnl(),
        NDX=ndx_pnl(),
        DAX=dax_pnl(),
        SX5E=sx5e_pnl()
    )
    pnl
})

output$strategy_outcomes<-DT::renderDataTable({
      outcome<-selected_outcome()
      the_table<-DT::datatable(
        outcome[,.(roll,date,yfrac,pnl,dd)], 
        selection="single",
        rownames= FALSE,
        options = list(
          bPaginate = F, 
          scrollY="700px",
          searching = FALSE
        )
      ) %>%
      formatRound("yfrac",3) %>%
      formatRound("pnl",2)
      
      the_table
  })
  
output$outcome_plot<-renderPlot({
    pnl<-selected_pnl()
    outcome<-selected_outcome()
    o<-input$strategy_outcomes_rows_selected
    if(length(o)<1)return(NULL)
    the_date<-outcome$date[o]
    the_start<-which(pnl$pday==outcome$start[o])
    the_end<-which(pnl$pday==outcome$end[o])
    common_dates<-intersect(
      as.character(dof$Date,format="%Y-%m-%d"),
      as.character(pnl$date[the_start:the_end],format="%Y-%m-%d")
    )
    if(length(common_dates)<1){
      common_dates<-as.character(pnl$date[the_start:the_end],format="%Y-%m-%d")
    }
    i<-which(as.character(pnl$date,format="%Y-%m-%d") %in% common_dates)
    j<-which(as.character(dof$Date,format="%Y-%m-%d") %in% common_dates)
    
    
    dof_pnl<-dof[j,.(date=Date,pnl=cumsum(c(0,diff(PnL))),source="dof")]
    backtest_pnl<-pnl[i,.(date=date,pnl=cumsum(c(0,diff(pnl))),source="backtest")]
    if(nrow(dof_pnl)>0){
      a<-sd(dof_pnl$pnl)/sd(backtest_pnl$pnl)
      backtest_pnl$pnl<-a*backtest_pnl$pnl
    }
    
    df<-rbind(backtest_pnl,dof_pnl)[,.(date=date,pnl=pnl),keyby="source"]
    
    g1<- df %>% ggplot() + geom_line(aes(x=date,y=pnl,col=source),size=2,alpha=0.75) + ggtitle(the_date) 
    
    plot(g1)
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)


