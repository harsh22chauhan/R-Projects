stock  <- read.csv("E:/R/stock_pred.csv")
View(stock)

#create MAPE function

mape = function(act,pred){
  res = mean(abs((act-pred)/act))
  return(res)
}

#convert data to a time series object.
 stock_ts <- ts(stock[,1],start = 2008,freq = 12)
head(stock_ts) 

plot(stock_ts)

#various components of time series
stock_1 <- stl(stock_ts,s.window = "periodic")

stock_1

plot(stock_1)

#predicting the alpha, beta nd gamma values

stocks_hws <- HoltWinters(stock_ts)
stocks_hws

#calculation for MAPE
pred = as.numeric(stocks_hws$fitted[,1])
act = as.numeric(stock_ts[(1:96)])
mape(act,pred)

pred = predict(stocks_hws,n.ahead = 12, prediction.interval = T,level = 0.95)
pred

plot(stocks_hws,pred)

#check for the assumption.
err = act - as.numeric(pred[,1])
err

#errors should follow normal distribution

par(mfrow = c(1,2))
hist(err)

qqnorm(err)

par(mfrow = c(1,1))
acf(err,lag.max = 30)


#H0: independence in a given time series
Box.test(err,lag = 30,type = "Ljung-Box")
