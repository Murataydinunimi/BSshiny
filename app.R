
install.packages("shiny")
install.packages("quantmod")
install.packages("ggplot")
install.packages("plotly")
install.packages("DT")


library(shiny)
library("quantmod")
library("ggplot2")
library("plotly")
library(DT)


#get the options

SPX<- getOptionChain("^SPX",from=Sys.Date()-30) # last month
SPXcalls<-SPX[[1]]
SPXputs<-SPX[[2]]




#Call options

SPXcalls<-SPXcalls[-c(2,3,4,5,6,7,10)]
riskfreeR<-matrix(rep(0.015,nrow(SPXcalls)))  # interest rate is constant and it is repeated number of rows of SPXcalls times because
#since the app extracts the data in real time, the dataset online has sometimes 122 rows, sometimes 170 or 175. I chose to do in this way
# to be consistent with any changes that might occur in the dataset.

SPXcalls$last_trade_time<-as.Date(SPXcalls$LastTradeTime)
SPXcalls<-cbind(SPXcalls,riskfreeR)
SPXcalls<-SPXcalls[,-2]


#get the stock price for calls

SP <- getSymbols("^GSPC",auto.assign = FALSE, from = "2019-01-02", to="2020-12-31")
SPprice<-na.omit(SP[,4])
Sppricenum<-as.numeric(SPprice)

SPXcalls = cbind(SPXcalls,Sppricenum[1:nrow(SPXcalls)]) 


#calculate maturity
timeofobservation<-as.numeric(index(SPprice))
daysInOneYear<-365 # actual convention
maturityinyears<-diff(timeofobservation)/(daysInOneYear)
SPXcalls = cbind(SPXcalls,maturityinyears[1:nrow(SPXcalls)])
SPXcalls <- SPXcalls[,-7]


colnames(SPXcalls)<-c("Strike-Price","Implied Volatility","LastTrade_Time","Interest-rate","Stock-Price","Maturity-in-years")


SPXcalls = SPXcalls[,c(5,1,6,4,2,3)] #reorder the columns according to index of inputs

#get the stock price for puts


SPXputs<-SPXputs[-c(2,3,4,5,6,7,10)]
riskfreeR<-matrix(rep(0.015,nrow(SPXputs)))
SPXputs<-cbind(SPXputs,riskfreeR)
SPXputs$Lastradetime<-as.Date(SPXputs$LastTradeTime)
SPXputs<-SPXputs[,-2]
SPXputs = cbind(SPXputs,Sppricenum[1:nrow(SPXputs)])
SPXputs = cbind(SPXputs,maturityinyears[1:nrow(SPXputs)])

colnames(SPXputs)<-c("Strike-Price","Implied Volatility","Interest-rate","Last_Trade_Time","Stock-Price","Maturity-in-years")
SPXputs = SPXputs[,c(5,1,6,3,2,4)]





# Define server logic
server <- function(input, output) {

 
   
  BS<-function(S,K,TimeToMat, volatility, rf){
    d1<-(log(S/K)+(rf+0.5*volatility^2)*(TimeToMat))/(volatility*sqrt(TimeToMat))
    d2<-d1-volatility*sqrt(TimeToMat)
    bscall<-S*pnorm(d1)-K*exp(-rf*TimeToMat)*pnorm(d2)
    bsput <- K*exp(-rf*TimeToMat)*pnorm(-d2)-S*pnorm(-d1)
    res <- c(bscall,bsput)
    
  }
 
  
  #Call option price
  output$BScall <- renderText({ 
    #Get inputs
    S = input$stockprice
    K = input$strike
    TimeToMat = input$maturity
    volatility = input$volatility
    rf = input$riskfreerate
    res = round(BS(S,K,TimeToMat,volatility,rf)[1],4)
  })
  
  #Put option price
  output$BSput <- renderText({ 
    #Get inputs
    S = input$stockprice
    K = input$strike
    TimeToMat = input$maturity
    volatility = input$volatility
    rf = input$riskfreerate
    res = round(BS(S,K,TimeToMat ,volatility,rf)[2],4)
  })
  
  #Call option prices plotted against their strikes
  output$plotCall <- renderPlot({
    S = input$stockprice
    K = input$strike
    TimeToMat = input$maturity
    volatility = input$volatility
    rf = input$riskfreerate
    price_call = NULL; price_put = NULL
    strikes = SPXcalls[,1]
    for (k in strikes) {
      price_call = c(price_call,BS(S,k,TimeToMat,volatility,rf)[1])
      price_put = c(price_put,BS(S,k,TimeToMat,volatility,rf)[2])
    }
    df = data.frame(strikes,price_call,price_put)
    ggplot(df,aes(x=strikes,y=price_call)) + geom_point(color=strikes)
  }, height = 600, width = 800)
  
  #Put option prices plotted against their strikes
  output$plotPut <- renderPlot({
    S = input$stockprice
    K = input$strike
    TimeToMat = input$maturity
    volatility = input$volatility
    rf = input$riskfreerate
    price_call = NULL; price_put = NULL
    strikes = SPXcalls[,1]
    for (k in strikes) {
      price_call = c(price_call,BS(S,k,TimeToMat,volatility,rf)[1])
      price_put = c(price_put,BS(S,k,TimeToMat,volatility,rf)[2])
    }
    df = data.frame(strikes,price_call,price_put)
    ggplot(df,aes(x=strikes,y=price_put)) + geom_point(color=strikes)
  }, height = 600, width = 800)
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(SPXcalls[, input$show_vars, drop = FALSE])
  })
 
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(SPXputs[, input$show_vars1, drop = FALSE])
  })
  
}




##### UI #####

ui <- shinyUI(fluidPage(
  
  titlePanel("S&P Option Pricing"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Finance"',
        checkboxGroupInput('show_vars', 'Choose columns to be displayed:',
                           names(SPXcalls), selected = names(SPXcalls)),
        checkboxGroupInput('show_vars1', '',
                           names(SPXputs), selected = names(SPXputs))),
      
      numericInput('stockprice','Stock-Price',"3732.04"),
      numericInput('strike','Strike Price',"3732.04"),
      numericInput('maturity','Maturity',"0.1616"),
      numericInput('riskfreerate','Risk free rate',"0.015"),
      numericInput('volatility','Volatility',"0.2795"),
      hr(),
      
      
    ),
    
    mainPanel(
      p('Call price'),
      textOutput("BScall"),
      hr(),
      p('Put price'),
      textOutput("BSput"),
      hr(),
      tabsetPanel(
        tabPanel("Calls", plotOutput("plotCall",width="100%")), 
        tabPanel("Puts", plotOutput("plotPut",width="100%")),
        tabPanel('S&P Calls', DT::dataTableOutput('mytable1')),
        tabPanel("S&P Puts" , DT::dataTableOutput("mytable2"))
        
        
        )
        
      )
    )  
  ))


##### Run #####
shinyApp(ui = ui, server = server)
