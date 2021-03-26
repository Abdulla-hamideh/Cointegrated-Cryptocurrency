library(quantmod)
library(tidyverse)
library(tseries)
library(ggplot2)
library(forecast)
#install.packages("remotes")
library(remotes)
#install.packages("egcm")
library(egcm)
library(urca)
#install.packages("blotter")
library(blotter)
library(foreach)
#install.packages("quantstrat")
#remotes::install_github("braverock/quantstrat")
library(quantstrat)
library(data.table)
library(dplyr)
library(DT)
library(ggplot2)
library(htmltools)
library(htmlwidgets)

library(lattice)
#install.packages("pander")
library(pander)
#install.packages("devtools")
library(devtools)
#install_github("braverock/blotter") # dependency
#install_github("braverock/quantstrat")
#devtools::load_all()


##########################################
#backtesting

startDate <- "2020-05-20"
endDate <- "2020-12-31"

#get data
symbols <- c("BTC-USD", "ETH-USD", "LTC-USD", "BCH-USD")
getSymbols(symbols,  from=startDate, to=endDate, adjust=TRUE, src="yahoo")

#combining the adjusted data in one place to do all the tests
data <- cbind(`BTC-USD`$`BTC-USD.Adjusted`, `ETH-USD`$`ETH-USD.Adjusted`, `LTC-USD`$`LTC-USD.Adjusted`, `BCH-USD`$`BCH-USD.Adjusted`, fill = na.locf)


#correlationmatrix
corrdata <- as.data.frame(data)


corrdata <- corrdata %>% 
  rename(BTC = BTC.USD.Adjusted,
         ETH = ETH.USD.Adjusted,
         LTC = LTC.USD.Adjusted,
         BCH = BCH.USD.Adjusted)

correlation <- cor(corrdata, method = "pearson")

corrplot(correlation, method="number")

# johanse test to find cointegration
# lagselection
lagselect <- vars::VARselect(data,lag.max = 10, type ="both")
lagselect$selection 
# the model chose 5 is the optimal lag,
# lag selection is quite different in that we have to subtract one assuming there will be contigration relationship  
#5-1 = 4 lags
# k = 4 in johansen test 
  
# johansen test

model <- ca.jo(data, type="eigen", ecdet="none",K=9,spec="transitory", season = 12)
summary(model)

model # test staticts 
model@teststat[2] # test statistics H0 r=0, to be rejected 
model@teststat[1] # test statistics H0 R=1, should not be rejected 
# take critical value 
model@cval

#plot the test 
## our model looks better to the paper using our own coefficients 
spread <- data$BTC.USD -3.44*data$ETH.USD -29.57*data$LTC.USD + 0.73*data$BCH.USD
plot.ts(spread)

#GANGLE ENGLE 2 step approach
#salmost the same as paper
adf.test(diff(data$BTC.USD.Adjusted)[-1,])
adf.test(diff(data$ETH.USD.Adjusted)[-1,])
adf.test(diff(data$LTC.USD.Adjusted)[-1,])
adf.test(diff(data$BCH.USD.Adjusted)[-1,])


#fits the data in the paper
kpss.test(diff(data$BTC.USD.Adjusted)[-1,])
kpss.test(diff(data$ETH.USD.Adjusted)[-1,])
kpss.test(diff(data$LTC.USD.Adjusted)[-1,])
kpss.test(diff(data$BCH.USD.Adjusted)[-1,])
#just stationary because of differencing (diff)

pp.test
#not working
pp.test(diff(data$BTC.USD.Adjusted)[-1,])
pp.test(diff(data$ETH.USD.Adjusted)[-1,])
pp.test(diff(data$LTC.USD.Adjusted)[-1,])
pp.test(diff(data$BCH.USD.Adjusted)[-1,])



lm1 <- lm(BTC.USD.Adjusted~ETH.USD.Adjusted+LTC.USD.Adjusted+BCH.USD.Adjusted, data = data)
summary(lm1)


#if it does not work check the actual name in the regression: probably something like
data$spread <- data$BTC.USD.Adjusted - lm1$coefficients["ETH.USD.Adjusted"]*data$ETH.USD.Adjusted - lm1$coefficients["LTC.USD.Adjusted"]*data$LTC.USD.Adjusted - lm1$coefficients["BCH.USD.Adjusted"]*data$BCH.USD.Adjusted
head(data)

resid <- lm1$residuals

adf.test(resid)
pp.test(resid)
kpss.test(resid)

acf(resid)
pacf(resid)

#### plot seite 9
str(resid)
plot(data$spread)
plot.ts(data$spread)

### plot page 10

acf(data$spread)
pacf(data$spread)


## Arima page 10

ARIMA <- auto.arima(data$spread)
checkresiduals(ARIMA)







#regress

summary(lm1)
lm1$coefficients[1] * 10
abba <- summary(lm1)
abba$sigma



#backtesting

#preparing data

#JUST DO IT EACH TIME ONCE --> otherwise we overwrite the stuff way too often and the backtesting does not work again ... if so load in the data again
#implementing the ratios gathered by the regression
`BTC-USD`$`BTC-USD.Open` <- `BTC-USD`$`BTC-USD.Open` - lm1$coefficients["ETH.USD.Adjusted"]*`ETH-USD`$`ETH-USD.Open` -lm1$coefficients["LTC.USD.Adjusted"]*`LTC-USD`$`LTC-USD.Open` - lm1$coefficients["BCH.USD.Adjusted"]*`BCH-USD`$`BCH-USD.Open`
`BTC-USD`$`BTC-USD.High` <- `BTC-USD`$`BTC-USD.High` - lm1$coefficients["ETH.USD.Adjusted"]*`ETH-USD`$`ETH-USD.High` - lm1$coefficients["LTC.USD.Adjusted"]*`LTC-USD`$`LTC-USD.High` - lm1$coefficients["BCH.USD.Adjusted"]*`BCH-USD`$`BCH-USD.High`
`BTC-USD`$`BTC-USD.Low` <- `BTC-USD`$`BTC-USD.Low` - lm1$coefficients["ETH.USD.Adjusted"]*`ETH-USD`$`ETH-USD.Low` - lm1$coefficients["LTC.USD.Adjusted"]*`LTC-USD`$`LTC-USD.Low` - lm1$coefficients["BCH.USD.Adjusted"]*`BCH-USD`$`BCH-USD.Low`
`BTC-USD`$`BTC-USD.Close` <- `BTC-USD`$`BTC-USD.Close` - lm1$coefficients["ETH.USD.Adjusted"]*`ETH-USD`$`ETH-USD.Close` - lm1$coefficients["LTC.USD.Adjusted"]*`LTC-USD`$`LTC-USD.Close` -lm1$coefficients["BCH.USD.Adjusted"]*`BCH-USD`$`BCH-USD.Close`
`BTC-USD`$`BTC-USD.Volume` <- `BTC-USD`$`BTC-USD.Volume` - lm1$coefficients["ETH.USD.Adjusted"]*`ETH-USD`$`ETH-USD.Volume` - lm1$coefficients["LTC.USD.Adjusted"]*`LTC-USD`$`LTC-USD.Volume` - lm1$coefficients["BCH.USD.Adjusted"]*`BCH-USD`$`BCH-USD.Volume`
`BTC-USD`$`BTC-USD.Adjusted` <- `BTC-USD`$`BTC-USD.Adjusted` - lm1$coefficients["ETH.USD.Adjusted"]*`ETH-USD`$`ETH-USD.Adjusted` - lm1$coefficients["LTC.USD.Adjusted"]*`LTC-USD`$`LTC-USD.Adjusted` - lm1$coefficients["BCH.USD.Adjusted"]*`BCH-USD`$`BCH-USD.Adjusted`
`BTC-USD`$`Dwn` <- lm1$coefficients[1] - abba$sigma
`BTC-USD`$`Up` <- lm1$coefficients[1] + abba$sigma
`BTC-USD`$`Avg` <- lm1$coefficients[1]
lm1$coefficients["BCH.USD.Adjusted"]
#next step with different 

BTC_123 <- `BTC-USD`
names(BTC_123)[1]<-paste("Observed.Open")
names(BTC_123)[2]<-paste("High")
names(BTC_123)[3]<-paste("Low")
names(BTC_123)[4]<-paste("Close")
names(BTC_123)[5]<-paste("Volume")
names(BTC_123)[6]<-paste("Premium")
head(BTC_123)

Observed <- BTC_123
Observed <- Observed[,c(-1,-2,-3,-5)]
names(Observed)[1]<-paste("Observed.Close")
 

head(Observed)


#start backtesting
symbols <- "Observed"


.blotter <- new.env()
.strategy <- new.env()

initDate <- "2020-05-19"

currency('USD')
Sys.setenv(TZ = 'UTC')
initEq <- 100000

#remove old strats
quantstrat::rm.strat("FactorModelBT")


#I need to define symbols 
stock(symbols, currency = 'USD', multiplier = 1)




###################

## you only need to remove the old Prtfolios when you do a second Backtesting 
rm.strat(strat.name)
rm.strat(portfolio.st)
rm.strat(account.st)


strat.name <- "FactorModelBT"
portfolio.st <- "FactorModelBT"
account.st <- "FactorModelBT"


rm.strat(strat.name)
rm.strat(portfolio.st)

#maybe add initeq
initPortf(portfolio.st,
          symbols = symbols,
          initDate = initDate,
          currency = 'USD')

initAcct(account.st,
         portfolios = portfolio.st,
         initDate = initDate,
         currency = 'USD')

initOrders(portfolio.st,
           initDate = initDate)

strategy(strat.name, store = TRUE)

###################################### Signals ##############################################

# Add position constraints for all symbols traded


#for (sym in symbols)
#  addPosLimit(portfolio.st, sym, start(Observed), 30)

# Open Short

add.signal(strat.name,
           name = "sigCrossover",
           arguments = list(columns = c("Premium", "Up"), relationship = "gt"),
           label = "Premium.gt.Upper.Band")

# Close Short

add.signal(strat.name,
           name = "sigCrossover",
           arguments = list(columns = c("Premium", "Avg"), relationship = "lte"),
           label = "Premium.lte.Avg")

# Open Long

add.signal(strat.name,
           name = "sigCrossover",
           arguments = list(columns = c("Premium", "Dwn"), relationship = "lt"),
           label = "Premium.lt.Lower.Band")

# Close Long

add.signal(strat.name,
           name = "sigCrossover",
           arguments = list(columns = c("Premium", "Avg"), relationship = "gte"),
           label = "Premium.gte.Avg")

tmp <- applySignals(strategy = strat.name, mktdata = Observed)

################################ Trading Rules - Actions ###################################

# Open short Position

add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Premium.gt.Upper.Band",
                          sigval = TRUE,
                          orderqty = -1000,
                          ordertype = "market",
                          orderside = NULL,
                          #osFUN = osMaxPos, # Add position sizing function here
                          threshold = NULL),
         type = "enter",
         label = "OpenShort",
         path.dep = TRUE)

# Close short Position

add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Premium.lte.Avg",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "short",
                          replace = TRUE),
         type = "exit",
         label = "CloseShort",
         path.dep = TRUE)

# Open Long Position

add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Premium.lt.Lower.Band",
                          sigval = TRUE,
                          orderqty = 1000,
                          ordertype = "market",
                          orderside = NULL,
                          #osFUN = osMaxPos, # Add position sizing function here
                          threshold = NULL),
         type = "enter",
         label = "OpenLong",
         path.dep = TRUE)

# Close Long Position

add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Premium.gte.Avg",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = TRUE),
         type = "exit",
         label = "CloseLong",
         path.dep = TRUE)

################################ Apply Strategy ############################

out <- applyStrategy(strategy = strat.name, portfolios = portfolio.st)

updatePortf(strat.name)
updateAcct(strat.name)
updateEndEq(strat.name)




#taking the results
eq1 <- getAccount(strat.name)$summary$End.Eq
rt1 <- Return.calculate(eq1,"log") #wrong log calculation pls fix with normal returns
tstats <- t(tradeStats(strat.name))
ob <- getOrderBook(strat.name)
a <- getAccount(strat.name)

last(a$summary, 5)

library("lattice")
xyplot(a$summary, type = "h", col = 4)

equity <- a$summary$End.Eq
plot(equity, "EQT Curve")



rets.multi <- PortfReturns(strat.name)
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi, Return.calculate(a2$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "Total"
rets.multi <- rets.multi[,c("TOTAL", symbols)]
chart.CumReturns(rets.multi, colorset = rich6equal, legend.loc = "topleft", main="Returns Strat")

ar.tab <- table.AnnualizedReturns(rets.multi)
max.risk <- max(ar.tab2)
max.return <- max(ar.tab["Annualized Return",])
ar.tab


textplot(tstats[1:15,,drop=FALSE], show.colnames = FALSE, halign = "left")

chart.Posn(strat.name, tmp.ticker=`BTC-USD`)





#############################################################################
#############################################################################
#############################################################################

#############################################
#set up analytics

#updatePortf(portfolio.st)

#dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]

#updateAcct(portfolio.st,dateRange)

#updateEndEq(account.st)

#chart.Posn(portfolio.st, 'Observed')













#Monte-Carlo example code
install.packages("pbapply")
library(lubridate)
library(pbapply)

ticker <- "SPY"
stock <- getSymbols(ticker,auto.assign = FALSE)
tmp <- getQuote(ticker)
stock <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
tmp <- Ad(stock)
rets <- ROC(tmp,type="discrete")
rets[is.na(rets)]<-0
mean(rets)
sd(rets)

stk_ret = function(STK_PRC, N, MEAN, STDEV)
{
  delta_t = 1/N # for 1 period
  for(i in seq(N))
  {
    epsilon <- runif(n=1,min = 0,max=1) # random probs.
    STK_PRC <- STK_PRC * (1 + qnorm(epsilon, MEAN*delta_t, STDEV*sqrt(delta_t)))
  }
  
  STK_PRC
  
}

last(tmp)
simulations <- 1000
N = 20
STK_PRC <- as.numeric(coredata(tmp[Sys.Date() - days(20)]))
MEAN = mean(rets)
STDEV = sd(rets)


stock_prices <- c()
for(i in seq(simulations))
{
  stock_prices <- c(stock_prices,stk_ret(STK_PRC = STK_PRC, N=N,MEAN=MEAN,STDEV=STDEV))
}

quantile(stock_prices)


EXPIRY <- tmp[options.expiry(tmp)]
EXPIRY <- EXPIRY["2007::"]
IDX <- index(EXPIRY)
NEXT.EXPIRY <- as.Date("2020-06-19")
IDX<- c(IDX,NEXT.EXPIRY)

MEAN = function(calculateUNTIL)
{
  tmp <- tmp[paste0("::",calculateUNTIL)]
  tmp <- ROC(tmp,type="discrete")
  tmp[is.na(tmp)]<-0
  mean(tmp)
}
STDEV = function(calculateUNTIL)
{
  tmp <- tmp[paste0("::",calculateUNTIL)]
  tmp <- ROC(tmp,type="discrete")
  tmp[is.na(tmp)]<-0
  sd(tmp)
}

means <- do.call(rbind,lapply(as.list(IDX), MEAN))
stdevs <- do.call(rbind,lapply(as.list(IDX), STDEV))
days = as.numeric(diff(IDX))


MONTE.CARLO = function(sim,iter,LastIter)
{
  simulations <- sim
  N <- days[iter]
  STK_PRC <- as.numeric(EXPIRY[iter])
  MEAN <- means[iter]
  STDEV <- stdevs[iter]
  stock_prices <- c()
  for(i in seq(simulations))
  {
    stock_prices <- c(stock_prices, stk_ret(STK_PRC = STK_PRC,N=N,MEAN=MEAN,STDEV = STDEV))
  }
  
  START <- as.data.frame(round(STK_PRC,2))
  START.DATE = index(EXPIRY[iter])
  PROBS = as.data.frame(t(round(quantile(stock_prices,probs = seq(0,1,0.05)),2)))
  
  if(iter == LastIter)
  {
    END <- as.data.frame(NA)
    END.DATE = as.data.frame(NA)
  }else{
    END <- as.data.frame(as.numeric(round(EXPIRY[iter+1],2)))
    END.DATE = index(EXPIRY[iter+1])
  }
  all <- cbind(START,START.DATE,PROBS,END,END.DATE)
  colnames(all) <- c("START.PRC","START.DATE","0%","5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%",
                     "60%","65%","70%","75%","80%","85%","90%","95%","100%","END.PRC","END.DATE")
  
  all
  
}

p <- pblapply(as.list(1:length(days)), function(x){
  MONTE.CARLO(sim=10000,iter = x, LastIter = length(days))
})
p <- do.call(rbind,p)

plot(p$END.PRC, type="l")
lines(p$`0%`, col='red')
lines(p$`100%`,col='green')

# number of months
nMo <- nrow(p)

# numbers of times it closes above 100%
sum(as.numeric(na.omit(ifelse(p$END.PRC > p$`100%`,1,0))))/nMo

# numbers of times it closes below 0%
sum(as.numeric(na.omit(ifelse(p$END.PRC < p$`0%`,1,0))))/nMo

write.csv(p,"SPY.csv")






























































































##############################################
#Backup stuff

#myStocks <- c('BTC-USD', 'ETH-USD', 'LTC-USD', 'BCH-USD')

#get train & test data
#getSymbols(myStocks, src="yahoo", from="2017-12-20", to="2018-06-20")

log <- read.csv("log.csv")
data <- read.csv("data.csv")
daily <- read.csv('daily.csv')

daily


head(data)

plot(log)
### Johansen test # @Sota & Abdulla pls put in your part

model <- ca.jo(data, type="eigen", ecdet="none",K=2,spec="transitory", season = 6)
summary(model)

## still neede to be fixed
data$Spread = data$BTC.USD  -2.22*data$ETH.USD - 29.43*data$LTC.USD - 0.19*data$BCH.USD
plot.ts(data$Spread)

library(blotter)


#Engle Granger approach



#############my

?adf.test
#salmost the same as paper
adf.test(data$BTC.USD)
adf.test(data$ETH.USD)
adf.test(data$LTC.USD)
adf.test(data$BCH.USD)

adf.test(log$BTC.USD)
adf.test(log$ETH.USD)
adf.test(log$LTC.USD)
adf.test(log$BCH.USD)

adf.test(daily$BTC.USD)
adf.test(daily$ETH.USD)
adf.test(daily$LTC.USD)
adf.test(daily$BCH.USD)

#fits the data in the paper
kpss.test(data$BTC.USD)
kpss.test(data$ETH.USD)
kpss.test(data$LTC.USD)
kpss.test(data$BCH.USD)

#not working
pp.test(data$BTC.USD)
pp.test(data$ETH.USD)
pp.test(data$LTC.USD)
pp.test(data$BCH.USD)


lm1 <- lm(BTC.USD~ETH.USD+LTC.USD+BCH.USD, data = data)
summary(lm1)

spread123 <- data$BTC.USD - 1.1107*data$ETH.USD - 26.1382*data$LTC.USD - 1.7617*data$BCH.USD


resid <- lm1$residuals
adf.test(resid)
acf(resid)
pacf(resid)

adf.test(resid)
pp.test(resid)
kpss.test(resid)

#### plot seite 9
str(resid)
plot(spread123)
plot.ts(spread123)

### plot page 10

acf(spread123)
pacf(spread123)


## Arima page 10

ARIMA <- auto.arima(spread)
checkresiduals(ARIMA)

















initPortf(portfolio.st,
          symbols = symbols,
          initDate = initDate,
          currency = 'USD')

initAcct(account.st,
         portfolios = portfolio.st,
         initDate = initDate, initEq = initEq,
         currency = 'USD')

initOrders(portfolio.st,
           initDate = initDate)
#############
for (sym in symbols)
  addPosLimit(portfolio.st, sym, start(BTC_123), 30)

strategy(strat.name, store= TRUE ) #maType
#add.indicator("bbands", name="BBands", arguments = list(HLC = quote(HLC(mktdata)), maType = 'SMA'), label = 'BBands')

#Open Short  #Close instead of Cl
add.signal(strat.name, name="sigCrossover", arguments = list(columns= c("Avg", "Up"), relationship="gt"), label="Close.gt.UpperBand")

#Open Long
add.signal(strat.name, name="sigCrossover", arguments = list(columns= c("Avg", "Dwn"), relationship="lt"), label="Close.lt.LowerBand")

#Close Short
add.signal(strat.name, name="sigCrossover", arguments = list(columns= c("Avg", "Avg"), relationship="lte"), label="Close.lte.Avg")


#Close Long
add.signal(strat.name, name="sigCrossover", arguments = list(columns= c("Close", "Avg"), relationship="gte"), label="Close.gte.Avg")



tmp <- applySignals(strategy = strat.name, mktdata = BTC_123)

#add.rule("bbands", name= "ruleSignal", arguments = list(sigcol="Cl.gt.UpperBand", sigval=TRUE, orderqty=-100, ordertype='market', oderside=NULL), type='enter')
#add.rule("bbands", name= "ruleSignal", arguments = list(sigcol="Cl.lt.LowerBand", sigval=TRUE, orderqty=-100, ordertype='market', oderside=NULL), type='enter')
#add.rule("bbands", name= "ruleSignal", arguments = list(sigcol="Cross.Mid", sigval=TRUE, orderqty='all', ordertype='market', oderside=NULL), type='exit')

# Open short Position
add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Close.gt.UpperBand",
                          sigval = TRUE,
                          orderqty = -1000,
                          ordertype = "market",
                          orderside = NULL,
                          #osFUN = osMaxPos, # Add position sizing function here
                          threshold = NULL),
         type = "enter",
         label = "OpenShort",
         path.dep = TRUE)

# Close short Position
add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Close.lte.Avg",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "short",
                          replace = TRUE),
         type = "exit",
         label = "CloseShort",
         path.dep = TRUE)


# Open Long Position
add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Close.lt.LowerBand",
                          sigval = TRUE,
                          orderqty = 1000,
                          ordertype = "market",
                          orderside = NULL,
                          osFUN = osMaxPos, # Add position sizing function here
                          threshold = NULL),
         type = "enter",
         label = "OpenLong",
         path.dep = TRUE)

# Close Long Position
add.rule(strat.name,
         name = "ruleSignal",
         arguments = list(sigcol = "Close.gte.Avg",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = TRUE),
         type = "exit",
         label = "CloseLong",
         path.dep = TRUE)

out <- applyStrategy(strategy = strat.name, portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
out
chart.Posn(portfolio.st)

#Doing the strat with SD of 1 and mean based on the last 20 days (0.5, 1)   ~PLS DO 1.5 and 2 
SD <- 1
N <- 20

out <- applyStrategy("bbands", portfolios = "BBandStrat", parameters=list(sd=SD, n=N))

updatePortf("BBandStrat")
updateAcct("BBandStrat")
updateEndEq(account.st)
chart.Posn("BBandStrat", tmp.ticker=`BTC-USD`, TA="add_BBands(n=20,sd=2)")

#taking the results
eq1 <- getAccount("BBandStrat")$summary$End.Eq
rt1 <- Return.calculate(eq1,"log") #wrong log calculation pls fix with normal returns
tstats <- t(tradeStats("BBandStrat"))
ob <- getOrderBook("BBandStrat")
a <- getAccount("BBandStrat")


a <- getAccount("BBandStrat")
last(a$summary, 5)

require("lattice")
xyplot(a$summary, type = "h", col = 4)

equity <- a$summary$End.Eq
plot(equity, "BBand EQT Curve")

rets.multi <- PortfReturns("BBandStrat")
colnames(rets.multi) <- symbols
rets.multi <- na.omit(cbind(rets.multi, Return.calculate(a2$summary$End.Eq)))
names(rets.multi)[length(names(rets.multi))] <- "Total"
rets.multi <- rets.multi[,c("TOTAL", symbols)]
chart.CumReturns(rets.multi, colorset = rich6equal, legend.loc = "topleft", main="BBand Returns Strat")

ar.tab <- table.AnnualizedReturns(rets.multi)
max.risk <- max(ar.tab2)
max.return <- max(ar.tab["Annualized Return",])
ar.tab


#SD 1.5  #should repeat it with different SD as well

rm.strat(strat.name)
rm.strat(portfolio.st)
rm.strat(account.st)

rm.strat("BBandStrat")
rm.strat("BBandstrat2")
portfolio.st <- "BBandStrat2"
account.st <- "BBandStrat2"

initPortf(portfolio.st,
          symbols = symbols,
          initDate = initDate,
          currency = 'USD')

initAcct(account.st,
         portfolios = portfolio.st,
         initDate = initDate, initEq = initEq,
         currency = 'USD')

initOrders(portfolio.st,
           initDate = initDate)

SD = 2
out <- applyStrategy("bbands", portfolios = "BBandStrat2", parameters=list(sd=SD,n=N))

updatePortf("BBandStrat2")
updateAcct("BBandStrat2")
updateEndEq("BBandStrat2")


eq2 <- getAccount("BBandStrat2")$summary$End.Eq
rt2 <- Return.calculate(eq2,"log")

returns <- cbind(rt1, rt2)
colnames(returns) <- c("SD=2", "SD=3")
chart.CumReturns(returns,colorset = c(2,4), legend.loc ="topleft", main="BBand STDEV Comparison",
                 ylab="cum. return", xlab="")

tstats2 <- t(tradeStats("BBandStrat2"))


textplot(tstats[1:15,,drop=FALSE], show.colnames = FALSE, halign = "left")
textplot(tstats2[16:30,,drop=FALSE], show.colnames = FALSE, halign = "left")


ob2 <- getOrderBook("BBandStrat2")
View(ob$BBandStrat2$`BTC-USD`)
ob2

a2 <- getAccount("BBandStrat2")
last(a2$summary, 5)

require("lattice")
xyplot(a2$summary, type = "h", col = 4)

equity2 <- a2$summary$End.Eq
plot(equity2, "BBand EQT Curve")

rets.multi2 <- PortfReturns("BBandStrat2")
colnames(rets.multi2) <- symbols
rets.multi2 <- na.omit(cbind(rets.multi2, Return.calculate(a2$summary$End.Eq)))
names(rets.multi2)[length(names(rets.multi2))] <- "Total"
rets.multi2 <- rets.multi2[,c("TOTAL", symbols)]
chart.CumReturns(rets.multi2, colorset = rich6equal, legend.loc = "topleft", main="BBand Returns Strat")

ar.tab2 <- table.AnnualizedReturns(rets.multi2)
max.risk2 <- max(ar.tab2)
max.return2 <- max(ar.tab2["Annualized Return",])
ar.tab2

#chart.RiskReturnScatter(rets.multi2$BTC.USD, main = "Performance", colorset = rich6equal)
#does not work dunno 




















































