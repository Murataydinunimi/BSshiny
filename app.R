
library(shiny)
library("quantmod")
library("ggplot2")
library("plotly")
library(DT)


#clean data

SPX<- getOptionChain("^SPX",from=Sys.Date()-30) # last month
SPXcalls<-SPX[[1]]
SPXputs<-SPX[[2]]

colnames(SPXcalls)<-c("Strike-Price","Stock-Price","Change","Bid","Ask","Volume","Open-interest","Lastradetime","Volatility")
SPXcalls<-SPXcalls[-c(3,4,5,6,7,10)]
riskfreeR<-matrix(rep(0.015,122)) # interest rate is constant.
SPXcalls<-cbind(SPXcalls,riskfreeR)
SPXcalls$last_trade_time<-as.Date(SPXcalls$Lastradetime)
SPXcalls = SPXcalls[,-3]



#since maturity is not given for options above, I created a fake maturity from S&P stock index to use for the options above.


SP <- getSymbols("^GSPC",auto.assign = FALSE, from = "2019-01-02", to="2020-12-31")
SPprice<-na.omit(SP[,4])

timeofobservation<-as.numeric(index(SPprice))
daysInOneYear<-365 # actual convention
maturityinyears<-diff(timeofobservation)/(daysInOneYear)
SPXcalls = cbind(SPXcalls,maturityinyears[1:122])
colnames(SPXcalls)[6]<-"Maturity-in-years"




# Define server logic for random distribution application
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
 
 
}




##### UI #####

ui <- shinyUI(fluidPage(
  
  titlePanel("S&P Option Pricing"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "SPX Price"',
        checkboxGroupInput('show_vars', 'Choose data elements:',
                           names(SPXcalls), selected = names(SPXcalls))),
      
      numericInput('stockprice','Stock-Price',""),
      numericInput('strike','Strike Price',""),
      numericInput('maturity','Maturity',""),
      numericInput('volatility','Volatility',""),
      numericInput('riskfreerate','Risk free rate',""),
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
        tabPanel('S&P index', DT::dataTableOutput('mytable1'))
          
        )
        
      )
    )  
  ))


##### Run #####
shinyApp(ui = ui, server = server)
