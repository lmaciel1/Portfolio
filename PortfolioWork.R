
####################
#Portfolio Analysis#
####################
#data/date#
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(data.table)
library(reshape2)
date <- "2016-09-09"        


#companies/portfolio weights#
tickers <- c("MSFT", "DIA", "BABA", "TMO", "NFLX", "SLV")
weights <- c(.1, .2, .15, .25, .2, .1)   


# Retrieve company daily prices and returns#
portfoliostockPrices <- NULL
for(ticker in tickers){
  portfoliostockPrices <- cbind(portfoliostockPrices,
                                getSymbols.yahoo(ticker, from = date, periodicity = "daily", auto.assign=FALSE)[,4])}
portfoliostockReturns <- na.omit(ROC(portfoliostockPrices))
#missing data test#
colSums(is.na(portfoliostockPrices))

#Retrieve data for benchmark price/return (s&p 500)#
benchmarkPrices <- getSymbols.yahoo("^GSPC", from = date, periodicity = "daily", auto.assign=FALSE)[,4]
benchmarkROC <- na.omit(ROC(benchmarkPrices))
#Missing data test#
colSums(is.na(benchmarkPrices))

#Daily Portfolio and benchmark Geometric Returns#             
PortfolioReturn <- Return.portfolio(portfoliostockReturns)
benchmarkReturn <- Return.portfolio(benchmarkROC)


#CAPM#
CAPM.beta(PortfolioReturn, benchmarkROC, .021/252)

#Sharpe Ratio#
SharpeRatio(PortfolioReturn, .021/252)
SharpeRatio(benchmarkReturn, .021/252)

#Annualized Portfolio Returns/Calendar Returns#
table.AnnualizedReturns(PortfolioReturn)
calret <- table.CalendarReturns(PortfolioReturn)
calret <- setDT(calret, keep.rownames = TRUE)[]

Annualretport<-calret$portfolio.returns %>% as.data.frame()
Annualretport$date <- row.names(Annualretport)
Annualretport <- setnames(Annualretport, old = c(".", "date"), new= c("Portfolio Annual Returns", "Years"), skip_absent = TRUE)


#Annualized benchmark Returns/Calendar Returns#
table.AnnualizedReturns(benchmarkReturn)
benchret <- table.CalendarReturns(benchmarkReturn)
benchret <- setDT(benchret, keep.rownames = TRUE)[]

Annualretbench <- benchret$portfolio.returns%>% as.data.frame()
Annualretbench$date <- row.names(Annualretbench)
Annualretbench <- setnames(Annualretbench, old = c(".", "date"), new= c("Benchmark Annual Returns", "Years"), skip_absent = TRUE)

#Comparing both strategies Annually#
compare <- merge(Annualretport, Annualretbench, by='Years')


#Visualization#
plot(compare$Years, compare$`Portfolio Annual Returns`, type="l", col=1, 
     ylim=c(-7,7), 
     xlab='Years', 
     ylab='Return', 
     main = 'Portfolio vs Benchmark Returns')
lines(compare$Years, compare$`Benchmark Annual Returns`,type="l", col=2)


########################
#Portfolio Optimization#
######################## 
library(PortfolioAnalytics)
library(ROI.plugin.quadprog)


port <- portfolio.spec(colnames(portfoliostockReturns))

#Constraints#

port <- add.constraint(port, type = "weight_sum", min_sum=1, max_sum=1)
port <- add.constraint(port, type ="box", min=.10, max=.40)


#Objective#

port <- add.objective(port, type ="return", name = "mean")
port <- add.objective(port, type ="risk", name = "StdDev")

#Optimized Portfolio#
optimize.portfolio(portfoliostockReturns, port, optimize_method = "ROI")


print('done')
