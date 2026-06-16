library("forecast")
library("fpp2")
library("pracma")
library("ggplot2")

data <- read.table("arma_series_group_Jasper_Lieke.txt")
autoplot(ts(data$V1[200:500]))+xlab("Time")+ylab("Value")

x <- ts(data)
x.detrend <- detrend(x, tt = "linear", bp = c())
N <- length(x.detrend)
x.detrend.scaled <- x.detrend / 1
autoplot(x.detrend.scaled)+xlab("Time")+ylab("Value")
acfx <- acf(x.detrend.scaled)
acfx <- ts(acfx$acf)
ac <- as.numeric(acfx)
acs <- t(ac) * ac
N.acfx <- length(acfx)
a <- rep(NA, N.acfx)
for (j in 2:N.acfx){
  a[j] = 0
  for (i in 1:j){
    a[j] = acs[i] + a[j]
  }
}
a[1] <- 0
b <- sqrt((rep(1, N.acfx)+2*a)/N)*qnorm(1-0.05/2)
autoplot(acfx)+xlab("lag") + ylab("autocorrelation")+theme(legend.position = "None")+
  theme(axis.title=element_text(size=18))+
  geom_line(aes(y = b), linetype='dashed', col = 'red')+
  geom_line(aes(y = -b), linetype='dashed', col = 'red')

pacfx = pacf(x.detrend.scaled)
pacfx = ts(pacfx$acf)
N.pacfx <- length(pacfx)
autoplot(pacfx)+xlab("lag") + ylab("partial correlation")+theme(legend.position = "None")+
theme(axis.title=element_text(size=18))+
  geom_line(aes(y = b[1:N.pacfx]), linetype='dashed', col = 'red')+
  geom_line(aes(y = -b[1:N.pacfx]), linetype='dashed', col = 'red')

N.arma = 15
aic.ar = rep(NA, N.arma)
for (n in 5:N.arma) {
  fit <- arima(x.detrend.scaled, order = c(n,0,0), include.mean=FALSE)
  aic.ar[n] <- fit$aic
}
plot(5:N.arma, aic.ar[5:N.arma], type="l")

arima(x.detrend.scaled, order = c(8,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(8,0,1), include.mean=FALSE)

K.arma = 30
aic.ma = rep(NA, K.arma)
for (k in 1:K.arma) {
  fit <- arima(x.detrend.scaled, order = c(0,0,k), include.mean=FALSE)
  aic.ma[k] <- fit$aic
}
plot(1:K.arma, aic.ma, type="l")

aic(arima(x.detrend.scaled, order = c(0,0,1), include.mean=FALSE)) # 6474.29
arima(x.detrend.scaled, order = c(0,0,2), include.mean=FALSE) # 5892.73
arima(x.detrend.scaled, order = c(0,0,3), include.mean=FALSE) # 5833.16
arima(x.detrend.scaled, order = c(0,0,4), include.mean=FALSE) # 5857.36
arima(x.detrend.scaled, order = c(0,0,5), include.mean=FALSE) # 5704.51
arima(x.detrend.scaled, order = c(0,0,6), include.mean=FALSE) # 5460.69
arima(x.detrend.scaled, order = c(0,0,7), include.mean=FALSE) # 5454.32
arima(x.detrend.scaled, order = c(0,0,8), include.mean=FALSE) # 5386.22
arima(x.detrend.scaled, order = c(0,0,9), include.mean=FALSE) # 5369.43
arima(x.detrend.scaled, order = c(0,0,10), include.mean=FALSE) # 5297.06
arima(x.detrend.scaled, order = c(0,0,11), include.mean=FALSE) # 5277.56
arima(x.detrend.scaled, order = c(0,0,12), include.mean=FALSE) # 5233.7
arima(x.detrend.scaled, order = c(0,0,13), include.mean=FALSE) # 5235.59
arima(x.detrend.scaled, order = c(0,0,14), include.mean=FALSE) # 5213.55
arima(x.detrend.scaled, order = c(0,0,15), include.mean=FALSE) # 5190.74
arima(x.detrend.scaled, order = c(0,0,16), include.mean=FALSE) # 5172.24


arima(x.detrend.scaled, order = c(1,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(2,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(3,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(1,0,1), include.mean=FALSE)

