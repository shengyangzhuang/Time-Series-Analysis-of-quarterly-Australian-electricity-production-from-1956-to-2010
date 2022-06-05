library(itsmr)
library(astsa)
library(forecast)
library(ggplot2)
library(fpp2)
library(gridExtra)
library(reshape2)
library(tseries)
library(urca)


data <- read.csv("F:\\KTH\\SF2943\\project\\ele.csv", header = T)

ele <- data$ele
ele <- ts(ele, frequency = 4)
plotc(ele)

ggseasonplot(ele, polar=TRUE)
ggseasonplot(ele, year.labels=TRUE, year.labels.left=TRUE)
ggsubseriesplot()+
  ylab("billion kWh")+
  ggtitle("Seasonal subseries plot : Quarterly electricity production in Australia.")

acf(ele,lag.max = 48)

## fourth-difference the elec data to remove seasonality
ele_d4 <- diff(ele, lag = 4)
## plot the differenced data
plot(ele_d4, ylab = expression(paste(nabla^4, "X"[t])))

## difference the differenced elec data
ele_dd4 <- diff(ele_d4, differences = 1)
## plot the newly differenced data
plot(ele_dd4, ylab = expression(paste(nabla, "(", nabla^4, 
                                       "X"[t], ")")))
plotc(ele_dd4)

## quicker way
#autoplot(ele)
#ele %>% diff(lag=4) %>% ggtsdisplay()

## classical decomposition to estimate the trend and seasonality
ele_dd4 %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of quarterly electricity 
producition in Australia")

## STL
ele_dd4 %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

acf(ele_dd4, lag.max = 36)
pacf(ele_dd4, lag.max = 36)

## better ACF plot
plot.acf <- function(ACFobj) {
  rr <- ACFobj$acf[-1]
  kk <- length(rr)
  nn <- ACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "Correlation", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}
## acf of the unprocessed ele data
ele_acf <- acf(ele, lag.max = 36)
## correlogram of the unprocessed ele data
plot.acf(ele_acf)

## acf and correlogram of the processed ele_dd4 data
ele_dd4_acf <- acf(ele_dd4, lag.max = 72)
plot.acf(ele_dd4_acf)

## better PACF plot
plot.pacf <- function(PACFobj) {
  rr <- PACFobj$acf
  kk <- length(rr)
  nn <- PACFobj$n.used
  plot(seq(kk), rr, type = "h", lwd = 2, yaxs = "i", xaxs = "i", 
       ylim = c(floor(min(rr)), 1), xlim = c(0, kk + 1), xlab = "Lag", 
       ylab = "PACF", las = 1)
  abline(h = -1/nn + c(-2, 2)/sqrt(nn), lty = "dashed", col = "blue")
  abline(h = 0)
}
## PACF of the ele_dd4 data
ele_dd4_pacf <- pacf(ele_dd4)
## correlogram of the ele_dd4 data
plot.acf(ele_dd4_pacf)

ggtsdisplay(ele_dd4)

## stationarity test
adf.test(ele_dd4)


## find best ARIMA(p,d,q) model automatically
auto.arima(ele, start.p = 0, max.p = 3, start.q = 0, max.q = 3)
fit2<-auto.arima(ele)
checkresiduals(fit2)

## find the best ARIMA(p,d,q) mannually
ele %>%
  Arima(order=c(3,1,3), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()
forecast::Arima(ele, order = c(1, 0, 1), seasonal=c(1,1,2), include.constant = TRUE)

fit3 <- Arima(ele, order=c(1,0,1), seasonal=c(1,1,2))
checkresiduals(fit3)
fit3 %>% forecast(h=12) %>% autoplot()

## forecast
model <- auto.arima(ele)
plot(forecast(model,h=8))

fit <- Arima(ele, order=c(1,0,1))
checkresiduals(fit)
autoplot(forecast(fit))

fr <- forecast::forecast(model, h = 12)
plot(fr)
points(testdat)