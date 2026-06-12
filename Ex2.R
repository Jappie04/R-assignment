library("forecast")
library("fpp2")
library("pracma")
library("ggplot2")

data <- read.table("arma_series_group_Jasper_Lieke.txt")
plot(data$V1, type="l")

x <- ts(data)
x.detrend <- detrend(x, tt = "linear", bp = c())
x.detrend.scaled <- x.detrend / 20
autoplot(x.detrend.scaled)+xlab("x-axis")+ylab("y-axis")
acfx <- acf(x.detrend.scaled)
acfx <- ts(acfx$acf)
ac <- as.numeric(acfx)
acs <- t(ac) * ac
N <- length(x.detrend.scaled)
a <- rep(N, 20)
for (j in 2:20){
  a[j] = 0
  for (i in 1:j){
    a[j] = acs[i] + a[j]
  }
}
a[1] <- 0
b <- sqrt((rep(1, 20)+2*a)/N)*qnorm(1 - 0.25 / 2)
autoplot(acfx)+xlab("lag") + ylab("autocorrelation")+theme(legend.position = "None")+
  theme(axis.title=element_text(size=18))+
  geom_line(aes(y = b), linetype='dashed', col = 'red')+
  geom_line(aes(y = -b), linetype='dashed', col = 'red')


