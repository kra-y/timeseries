# Causal Impact Example
rm(list = ls())
list.of.packages<-c("quantmod","CausalImpact","tidyverse","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(CausalImpact)
library(quantmod)
library(tidyverse)
library(tseries)

# We want to use Google's Causal Impact R package to investigate how the GME short impacted Disney stock.

# first we will need the three series
getSymbols("DIS;GME;AAPL;SPY" )

# Since we can't say for certain that any of these closing prices are independent of GME since they
# all exist in the same stock market, and the dynamics are going to be much more complex,
# the an alysis is probably invalid. This is just by way of example implementing the causal impact
# method in R. At any rate we are interested in how 2021 impacted disney stock closing prices
# assuming they are not endogenous to other stock market fluctuations

# create the data frame
STONKS<-data.frame("DIS" = DIS$DIS.Close,
                      "AAPL" = AAPL$AAPL.Close,
                      "SPY" = SPY$SPY.Close,
                      "GME" = GME$GME.Close,
                   "DATE" = index(GME$GME.Close))

#plot series together setting the first of june as the beginning of the market manipulation in GME
STONKS%>%
  gather("STONK","Close", DIS.Close:GME.Close)%>%
  mutate(STONK = gsub(".Close","",STONK),
         d.SHORT = ifelse(as.Date(DATE)>=as.Date("2020-06-01"),1,0))%>%
  ggplot(aes(x = DATE, y = Close))+
  geom_line(aes(colour = STONK))+
  geom_rect(aes(xmin = as.Date("2021-01-22"),
                xmax = as.Date("2021-02-12"),
                ymin = 0, 
                ymax = 400),alpha = 0.025, fill = "lightblue")

# now let's make the data set we'll use for running the analysis
y<-as.numeric(DIS$DIS.Close)
spy<-as.numeric(SPY$SPY.Close)
gme<-as.numeric(GME$GME.Close)
aapl<-as.numeric(AAPL$AAPL.Close)
data <- cbind(y, spy,gme,aapl)
time.points <- seq.Date(from = as.Date("2007-01-03"),to = as.Date("2021-02-12"), by = "1 day" )
data <- zoo(cbind(y, spy,gme,aapl), time.points)
head(data)

###### specify pre and post periods #######
pre.period <- as.Date(c("2007-01-03", "2021-01-11"))
post.period <- as.Date(c("2021-01-12", "2021-02-12"))
impact <- CausalImpact(data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 4))
plot(impact)
summary(impact,"report")
##############################################
post.length = as.numeric(post.period[2]-post.period[1])
post.period.response <- y[(length(y)-post.length):length(y)]
y[(length(y)-post.length):length(y)]<-NA
ss <- AddLocalLevel(list(), y)
bsts.model <- bsts(y ~ gme, ss, niter = 1000)
impact1 <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
plot(impact1)
summary(impact1, "report")
