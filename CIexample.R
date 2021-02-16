# CIexampl.R
install.packages("CausalImpact")
rm(list = ls())
library(CausalImpact)
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.5), n = 100)
x2 <- 110 + arima.sim(model = list(c(ar = 0.999,d = 1, ma = 1)), n = 100)
y <- (1.2 * x1 + rnorm(100))+(3.01*x2+rnorm(100))
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1,x2)
matplot(data, type = "l")
pre.period <- c(1, 70)
post.period <- c(71, 100)
impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
#Work with dates
time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(y, x1,x2), time.points)
head(data)
#Work with dates
pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))
impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
summary(impact,"report")
impact$summary


