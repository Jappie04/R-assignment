library("forecast")
library("fpp2")
library("pracma")
library("ggplot2")

data <- read.table("arma_series_group_Jasper_Lieke.txt")
plot(data$V1, type="l")

x <- ts(data)
x.detrend <- detrend(x, tt = "linear", bp = c())
N <- length(x.detrend)
x.detrend.scaled <- x.detrend / N
autoplot(x.detrend.scaled)+xlab("x-axis")+ylab("y-axis")
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

arima(x.detrend.scaled, order = c(7,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(8,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(8,0,1), include.mean=FALSE)
arima(x.detrend.scaled, order = c(9,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(10,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(30,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(1,0,0), include.mean=FALSE)
arima(x.detrend.scaled, order = c(1,0,1), include.mean=FALSE)
