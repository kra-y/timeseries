# NYSE Disney SARIMA models #
library(ggplot2)
library(vars)
library(plyr)
library(dplyr)
library(tseries)
library(reshape2)
library(forecast)
library(Amelia)
library(zoo)
library(scales)
library(egcm)
library(RColorBrewer)
library(quantmod)
library(MTS)

acf(na.omit(diff(DIS$DIS.Close)))


rm(list=ls())

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
?getQuote
getSymbols("DIS")
setSymbolLookup(DIS="DIS")
saveSymbolLookup(file='DISNEY')


str(DIS)

DIS.BICs<- apply(expand.grid(0:10,1,0:10),1L,
                  function(rw)
                    tryCatch(BIC(arima(DIS$DIS.Close,order=rw[1:3])),
                             error=function(e)NA))
DIS.ARIMA<-expand.grid(0:10,1,0:10)
names(DIS.ARIMA)<-c("p","d","q")

DIS.ARIMA$DIS.BICs<-DIS.BICs
DIS.ARIMA$d<-paste("d=",DIS.ARIMA$d,sep="")
DIS.ARIMA$d<-as.factor(DIS.ARIMA$d)
DIS.best<-data.frame("p"=DIS.ARIMA[which(DIS.ARIMA$DIS.BICs==min(na.omit(DIS.ARIMA$DIS.BICs))),1],
                      "d"=DIS.ARIMA[which(DIS.ARIMA$DIS.BICs==min(na.omit(DIS.ARIMA$DIS.BICs))),2],
                      "q"=DIS.ARIMA[which(DIS.ARIMA$DIS.BICs==min(na.omit(DIS.ARIMA$DIS.BICs))),3],
                      "BIC"=DIS.ARIMA[which(DIS.ARIMA$DIS.BICs==min(na.omit(DIS.ARIMA$DIS.BICs))),4])



ggplot(data=DIS.ARIMA, aes(x=q, y=p)) +
  geom_tile(aes(fill = DIS.BICs)) +
  facet_wrap(~d,nrow=2)+
  scale_fill_continuous(low = "cyan", high = "blue4")+ 
  geom_text(aes(label = round(DIS.BICs,2)),colour="yellow",fontface="bold") +
  ylab("Autoregressive\nParameter : p")+
  xlab("Moving Average\nParameter : q")+
  ggtitle("Bayesian Information Criteria of\n ARIMA(p,d,q)x(P,D,Q) Specifications\nfor Daily Closing Price of Disney")+
  theme(legend.position="none")+
  theme(axis.title.y = element_text(angle=0))+
  geom_rect(data=DIS.best,aes(xmin=q-.5,
                               xmax=q+.5,
                               ymin=p-.5,
                               ymax=p+.5),size=1.25,alpha=0,colour="yellow")
arima.disney<-arima(DIS$DIS.Close,order=c(0,1,0))
plot(VAR(DIS,5))
