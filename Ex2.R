library("forecast")
library("fpp2")
library("pracma")
library("ggplot2")

data <- read.table("arma_series_group_Jasper_Lieke.txt")
autoplot(ts(data$V1))+xlab("Time")+ylab("Value")

x <- ts(data)
mean(x)
x.detrend <- detrend(x, tt = "linear", bp = c())
N <- length(x.detrend)
mean(x.detrend)
x.detrend.scaled <- x.detrend / 20
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
for (n in 1:N.arma) {
  fit <- arima(x.detrend.scaled, order = c(n,0,0), include.mean=FALSE)
  aic.ar[n] <- fit$aic
}
plot(1:N.arma, aic.ar[1:N.arma], type="l", ylab="Aikake Information Criterion", xlab="order of AR")
plot(5:N.arma, aic.ar[5:N.arma], type="l", ylab="Aikake Information Criterion", xlab="order of AR")

K.arma = 5
aic.ma = rep(NA, K.arma)
for (k in 1:K.arma) {
  fit <- arima(x.detrend.scaled, order = c(0,0,k), include.mean=FALSE)
  aic.ma[k] <- fit$aic
}
plot(1:K.arma, aic.ma, type="l")

arima(x.detrend.scaled, order = c(6,0,0), fixed=c(NA,0,NA,NA,NA,NA), include.mean=FALSE)

max.order <- 5
1:1
for (n in 0:max.order) {
  for (k in 0:max(0, max.order-n)) {
    c(n,0,k)
    print(n)
    print(k)
    print(arima(x.detrend.scaled, order = c(n,0,k), include.mean = FALSE)$aic)
  }
}

model <- arima(x.detrend.scaled, order=c(5,0,0), include.mean=FALSE)
fitted.values <- fitted(model)

residuals <- residuals(model)
mean(residuals^2)

plot(residuals, type="p", ylab = "Residuals")
abline(h = 0, col = "red")
plot(residuals[1:100], ylab = "Residuals")

plot(x.detrend.scaled, type="l", ylab = "Value", xlab = "Time")
lines(fitted.values, col = "hotpink", lwd = 2)


plot(x.detrend.scaled[1:100], type="l", ylab = "Value", xlab = "Time")
lines(fitted.values[1:100], col = "hotpink", lwd = 2)


