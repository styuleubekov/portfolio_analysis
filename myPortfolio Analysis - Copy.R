#for git try
rm(list=ls())

library(quantmod)
library(data.table)

setwd("C:/Users/probook")

#import of trades (downloaded from yahoo)
quotes = fread('Downloads/quotes.csv') 
#new variable added
quotes = quotes[, TradeDate := as.Date(as.character(`Trade Date`), format = '%Y%m%d')] 

#list of unique stocks is created
stocks = unique(quotes$Symbol) 

#empty table with stock prices data is created
#the table is further populated with data from yahoo finance
stockPrices = data.table(index=character(), # index = date
                         symbol=character(),# symbol = ticker
                         Open=numeric(),
                         High=numeric(),
                         Low=numeric(),
                         Close=numeric(), 
                         Volume=numeric(),
                         Adjusted=numeric())

class(stockPrices$index) = 'Date'

#start and end dates of the analysis are created
startDate = min(quotes$TradeDate)
endDate = Sys.Date()

for(i in stocks){
  priceData = as.data.table(getSymbols(i, auto.assign = F, from = startDate))
  colnames(priceData) = c('index','Open','High','Low','Close','Volume','Adjusted')
  priceData$symbol = i
  setcolorder(priceData, neworder = colnames(stockPrices))
  stockPrices = rbind(stockPrices, priceData)
}

#stockPrices = as.data.table(stockPrices)
#redundant table is removed
rm(priceData)

#empty table for my portfolio is created
#only dates are populated
myPortfolio = data.table(Date = seq.Date(from = startDate,
                                         to = endDate,
                                         by = 'day'),
                         PortfolioCost = numeric(),
                         PortfolioValue = numeric(),
                         TotalReturn = numeric(),
                         DailyReturn = numeric())

#new variable in quotes is created
quotes[, Cost := `Purchase Price`*Quantity]
#reordering according to Trade Date (from first to latest)
quotes = quotes[order(TradeDate)]

#each day-stock combination is created
dailyPosition = merge(as.data.frame(myPortfolio[,.(Date)]),as.data.frame(stocks),
                      all = T)
#column renamed
colnames(dailyPosition)[2] = 'Symbol'

#left join of each day-stock combination with trades
dailyPosition2 = merge(dailyPosition, quotes[,.(Symbol,
                                                Quantity,
                                                TradeDate,
                                                Cost)], 
                       by.x = c('Symbol','Date'),
                       by.y = c('Symbol','TradeDate'),
                       all.x = T)

dailyPosition2 = as.data.table(dailyPosition2)

#dailyPosition2[is.na(Quantity), list('Quantity', 'Cost')] = 0 # does not work

#setting NAs to zero, since no trades happened
dailyPosition2[is.na(dailyPosition2$Quantity), c('Quantity','Cost')] = 0

#cumulative position is calculated
dailyPosition2[, `:=`(cumQuantity = cumsum(Quantity),
                      cumCost = cumsum(Cost)), by = Symbol]

#index and symbol are renamed to Date and Symbol resp.
colnames(stockPrices)[1:2] = c('Date','Symbol')
#setting Symbol (ie ticker) as factor
stockPrices$Symbol = as.factor(stockPrices$Symbol)

#dailyPosition2$Date = as.Date(dailyPosition2$Date)
#since stockPrices$Date has typeof = character, we need
#to convert it to double
stockPrices$Date = as.double(stockPrices$Date)
#and then convert it back to Date
stockPrices$Date = as.Date(stockPrices$Date)

#left join in order to add closing price to dailyPosition2
dailyPosition3 = merge(dailyPosition2, 
                       stockPrices[, .(Date, Symbol, Close)],
                       by.x = c('Date', 'Symbol'),
                       by.y = c('Date', 'Symbol'),
                       all.x = T)

#removal of redundant data (ie data when we did not have
#open position)
dailyPosition3 = dailyPosition3[cumQuantity>0]

#due to weekends and other non-working days, we have
#NAs in closing price data, thus we impute them via 
#last observation carried forward
dailyPosition3[, Close := nafill(Close, 'locf'), Symbol]

#adding position value on daily level
dailyPosition3[, value := Close*cumQuantity]

#absolute return calculation
dailyPosition3[, `:=`(absRet = value - cumCost,
                      Ret = (value/cumCost-1))]

#for each stock, portfolio share as of that date is calculated
dailyPosition3[, portfolioShare := value/sum(value),Date]

#daily position is aggregated (step 1)
aggregatedDailyPosition = dailyPosition3[, .(Date, cumCost, value, Ret, portfolioShare)]
#daily position is aggregated (step 2)
aggregatedDailyPosition[, `:=`(cumCost=sum(cumCost),
                               value=sum(value),
                               portfolioRet=sum(Ret*portfolioShare)),Date]

#duplicates removal via keeping only unique Dates
aggregatedDailyPosition = unique(aggregatedDailyPosition, by = 'Date')
#removal of redundant columns, since they were stock level
aggregatedDailyPosition$Ret = NULL
#removal of redundant columns, since they were stock level
aggregatedDailyPosition$portfolioShare = NULL

plot(aggregatedDailyPosition$Date, aggregatedDailyPosition$portfolioRet,
     type = 'l',
     main = 'My Portfolio return performance',
     ylab = 'Return (eg 0.1 = 10%)',
     xlab = 'Date')
abline(h = 0, col = 'grey')

#downloading S&P500 index data
getSymbols('^GSPC', from = startDate)

plot(GSPC$GSPC.Close)

#return calculation of the index
#the return is since the start date of the analysis
GSPC$RETURN = GSPC$GSPC.Close/as.numeric(GSPC$GSPC.Close[1]) - 1

GSPC = as.data.table(GSPC)

#creation of a new table with my portfolio and market portfolio
aggregatedDailyPositionAndMarket = merge(aggregatedDailyPosition,
                                         GSPC[,.(index,RETURN)],
                                         by.x = 'Date',
                                         by.y = 'index',
                                         all.x=T)
#due to weekends and other non-business days, locf is used for NAs
aggregatedDailyPositionAndMarket[,RETURN:=nafill(RETURN, 'locf')]

plot(x=aggregatedDailyPositionAndMarket$Date,
     y=aggregatedDailyPositionAndMarket$portfolioRet, type = 'l',
     ylab = 'Return since Apr 1, 2020',
     xlab = '')

lines(x=aggregatedDailyPositionAndMarket$Date,
      y=aggregatedDailyPositionAndMarket$RETURN, col = 'blue')

abline(h = 0, col = 'grey')

legend('topleft', 
       legend = c('My Portfolio',
                             'S&P 500 index',
                             'zero return'),
       col = c('black','blue','grey'),
       lty = 1,
       lwd = 1)

myPortfolio_dailyreturns = na.omit(ifelse(diff(aggregatedDailyPosition$portfolioRet) == 0,
       NA, 
       diff(aggregatedDailyPosition$portfolioRet)))

plot(myPortfolio_dailyreturns, type = 'h',
     main = 'Returns')
abline(h = 0, col = 'grey')



hist(myPortfolio_dailyreturns, breaks = 40,
     main = 'My Portfolio Returns',
     xlab = 'Absolute returns',
     prob = T)
# 
# # lines(dnorm(myPortfolio_dailyreturns,
# #             mean = mean(myPortfolio_dailyreturns),
# #             sd = sd(myPortfolio_dailyreturns)),
# #       col = 'darkblue',
# #       lwd = 2)
# 
# # curve(dnorm(myPortfolio_dailyreturns,
# #             mean = mean(myPortfolio_dailyreturns),
# #             sd = sd(myPortfolio_dailyreturns)),
# #       col = 'darkblue',
# #       lwd = 2,
# #       add = T)
# 
# lines(density(myPortfolio_dailyreturns, kernel = 'gaussian'),
#       col = 'red',
#       lwd = 2)






