library(quantmod)
getSymbols('EFX', from="2015-11-01",to="2017-11-05")
dim(EFX)
plot(EFX$EFX.Close)
lineChart(EFX,line.type = 'l',theme = 'white')
barChart(EFX,bar.type = 'hlc')
candleChart(EFX,multi.col = TRUE)
chartSeries(EFX$EFX.Close, theme = 'white',
            TA ="addEMA(10,col='black');addEMA(50,col='blue')")
library(TTR)
EFX.EMA.10 <- EMA(EFX$EFX.Close, n=5)
EFX.EMA.20 <- EMA(EFX$EFX.Close, n=10)
EFX.EMA.50 <- EMA(EFX$EFX.Close, n=20)
Fast.diff  <- EFX.EMA.10 - EFX.EMA.20
Slow.diff  <-  EFX.EMA.20 - EFX.EMA.50
addTA(Fast.diff, col='blue',type='h',legend = "10-20 MA")
addTA(Slow.diff, col='red',type='h',legend = "20-50 MA")

install.packages('binhf')
library(binhf)
  
tail(as.numeric(Fast.diff))
Long_Trades <- ifelse(Slow.diff > 0 & Fast.diff >0 & 
              shift(v=as.numeric(Fast.diff),places = 1,dir = 'right') < 0,
              EFX$EFX.Close,NA)
Short_Trades <- ifelse(Slow.diff < 0 & Fast.diff <0 & 
                shift(v=as.numeric(Fast.diff),places = 1,dir = 'right') > 0,
                EFX$EFX.Close,NA)
plot(EFX$EFX.Close)
points(Long_Trades,col = 'blue',cex=1.5,pch = 18)
points(Short_Trades,col = 'red',cex=1.5,pch = 18)
chartSeries(EFX, theme = 'white',
            TA ="addADX(n=14,maType = 'EMA',wilder='TRUE')")
--------------------
  library('quantmod')
library('forecast')
getSymbols('EFX', from="2015-11-01",to="2017-11-05")
names(EFX)
plot(EFX$EFX.Close)
fit <- auto.arima(EFX$EFX.Close,ic='bic')
fit
plot(as.ts(EFX$EFX.Close))

lines(fitted(fit),col='red')

fit.arima <- forecast(fit)
fit.arima
plot(fit.arima)
EFX_return <- diff(EFX$EFX.Close)/lag(EFX$EFX.Close,k=1)*100
head(fb_return)
tail(fb_return)
