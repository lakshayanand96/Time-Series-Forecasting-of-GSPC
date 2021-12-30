install.packages("quantmod")
library(quantmod)
#Obj 1: Load stock prices by symbol

getSymbols("^GSPC", src = "yahoo", 
           from = as.Date("2009-01-01"), 
           to = as.Date("2020-12-31"), 
           periodicity = "daily")
View(GSPC)

price_Adj = GSPC$GSPC.Adjusted
ret = na.omit(diff(log(price_Adj)))



main_title = "S&P 500: Returns"
plot(ret,lty="solid", main ="Log-Returns for GSPC",xlab = "Months",ylab = "Returns",major.format = "%Y-%m",ann = FALSE)

summary(ret)
#As we can see, the mean of log returns are almost 0 and the min is negative  and max is approximately around zero.

# ADF test for p values 
install.packages("tseries")
library(tseries)

acf(ret)
pacf(ret)

# both ACF and PACF tails off i.e ARMA model

Box.test(ret, type = "Ljung-Box")
# X-squared = 64.8, df = 1, p-value = 7.772e-16\


#Question 5
adf.test(ret,alternative = c("stationary"))  
adf.test(ret,alternative = c("explosive"))
# series is not stationary
tseries::kpss.test(ret, null = "Level")
tseries::kpss.test(ret, null = "Trend")


#Question 6
shapiro.test(as.vector(ret))


# question 7



library(forecast)
m <- auto.arima(ret)
m
arima(ret, order = c(0, 0, 1))
arima(ret, order = c(1, 0, 2))
arima(ret, order = c(2, 0, 1))
arima(ret, order = c(2, 0, 1))
confint(m, level = 0.95)
acf(ts(m$residuals))
pacf(ts(m$residuals))

mforecast = forecast(m,level = c(95),h=10)
mforecast
plot(mforecast)

Box.test(mforecast$residuals,lag = 5, type = "Ljung-Box")
Box.test(mforecast$residuals,lag = 10, type = "Ljung-Box")
Box.test(mforecast$residuals,lag = 15, type = "Ljung-Box")
Box.test(mforecast$residuals,lag = 20, type = "Ljung-Box")

checkresiduals(mforecast)
