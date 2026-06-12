library("forecast")
library("ggplot2")


sales <- read.csv("Sales.csv")

xlab.sales <- "Time"
ylab.sales <- "Number of Sales"
start.sales = c(1, 1)
end.sales = c(12, 12)
frequency.sales = 12

sales.ts <- ts(sales$Number.of.Products.Sold, frequency = frequency.sales, start = start.sales, end = end.sales)
autoplot(sales.ts)+xlab(xlab.sales)+ylab(ylab.sales)
         
sales$Log.Number.Of.Products.Sold <- log(sales$Number.of.Products.Sold)
log.sales <- log(sales$Number.of.Products.Sold)
log.sales.ts <- ts(log.sales, frequency = frequency.sales, start = start.sales, end = end.sales)
autoplot(log.sales.ts)+xlab("Time")+ylab("Number of Sales")
trend.log.sales <- ts(ma(log.sales.ts, order = 12, centre = T), frequency = frequency.sales, start = start.sales, end = end.sales)
autoplot(log.sales.ts)+xlab("Time") + ylab("Number of Sales")+autolayer(
  trend.log.sales, color="hotpink")+ theme(legend.position = "None")
detrend.log.sales <- ts(log.sales - trend.log.sales, frequency = frequency.sales, start = start.sales, end = end.sales)
autoplot(detrend.log.sales)+xlab(xlab.sales)+ylab(ylab.sales)
m.log.sales <- t(matrix(data = detrend.log.sales, nrow = frequency.sales))
seasonal.log.sales <- m.log.sales |>
  colMeans(na.rm = T) |> 
  rep() |> 
  ts(frequency = frequency.sales, start = start.sales, end = end.sales)
autoplot(detrend.log.sales)+xlab("Time")+ylab("Number of Sales")+autolayer(seasonal.log.sales)
+theme(legend.position = "None")

incidental.log.sales = ts(detrend.log.sales - seasonal.log.sales, frequency = frequency.sales, start = start.sales, end = end.sales)
autoplot(incidental.log.sales)+xlab("Time")+ylab("Number of Sales")  

trend.sales <- ts(exp(trend.log.sales), frequency = frequency.sales, start = start.sales, end = end.sales)
detrend.sales <- ts(exp(detrend.log.sales), frequency = frequency.sales, start = start.sales, end = end.sales)
seasonal.sales <- ts(exp(seasonal.log.sales), frequency = frequency.sales, start = start.sales, end = end.sales)
incidental.sales <- ts(exp(incidental.log.sales), frequency = frequency.sales, start = start.sales, end = end.sales)
autoplot(detrend.sales)+xlab(xlab.sales)+ylab(ylab.sales)
autoplot(trend.sales)+xlab(xlab.sales)+ylab(ylab.sales)+autolayer(trend.log.sales)
