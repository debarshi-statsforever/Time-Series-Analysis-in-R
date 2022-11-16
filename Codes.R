#Let us first call the libraries we may need
library(fpp2)
library(expsmooth)
library(fable)
library(tsibble)
library(itsmr)
library(forecast)
#preparing the data
data<-as.data.frame(djiclose)
data<-data$close
x<-ts(data=data,start=c(1928,10),frequency=12)
head(x)
#This is our original data stored as a "ts" object
#Plot the data
autoplot(x,ylab="Close Price")
#par(mfrow=c(1,2))
boxplot(x,col="red")
boxplot(log(x),col="green")
#The seasonal fluctuations are increasing in magnitude
#as the graph goes up rapidly
#Indication that T and S are related, hence should prefer
#multiplicative model, so we take log and fit additive model
#Also, there are a lot of outliers, taking log will help to control this
Decomp<-decompose(log(x))
autoplot(Decomp)
par(mfrow=c(1,1))
plot(Decomp$x,col="green",ylab="Trend")
lines(Decomp$trend,col="black")
plot(Decomp$seasonal,col="blue",ylab="seasonal")
plot(Decomp$random,,col="orange",ylab="Random Comp")
abline(h=0,col="blue")
autoplot(Decomp$random,ylab="Random Comp")
#Now once we have separated out trend, seasonality and random component,
#Let us take a look at the detrended and deseasonalized data

y <- na.omit(ts(Decomp$random,frequency = 1))
length(y)
length(x)
#6 points on either side are lost due to moving average
#Now we will fit ARMA to this detrended and deaseasonalized data.
#Let us take a look at ACF and PACF plot.
par(mfrow=c(2,1))
acf(y,main="Detrended and Deseasonalized Data")
pacf(y,main="")
#It seems that AR(5/6)should be apt, since PACF cuts off there.
arma_aic<-auto.arima(y,trace=T,d=0,seasonal=F,ic="aic")
arma_bic<-auto.arima(y,trace=T,d=0,seasonal=F,ic="bic")
best.mod.arma<-arima(y,order = c(5,0,0))
summary(best.mod.arma)
myarma1<-arima(y,order = c(0,0,6))
myarma2<-arima(y,order=c(3,0,0))
newarma<-auto.arima(y,trace=T,seasonal=F,
                     stepwise=F,ic="bic",max.p=6,max.q=6,
                    max.order=12)
#Do portmanteau tests
Box.test(resid(best.mod.arma),type="Box-Pierce")
Box.test(resid(best.mod.arma),type="Ljung-Box")
acf(resid(best.mod.arma))
pacf(resid(best.mod.arma))
Box.test(resid(myarma1),type="Box-Pierce")
Box.test(resid(myarma1),type="Ljung-Box")
acf(resid(myarma1))
pacf(resid(myarma1))
Box.test(resid(myarma2),type="Box-Pierce")
Box.test(resid(myarma2),type="Ljung-Box")
acf(resid(myarma2))
pacf(resid(myarma2))
AIC(best.mod.arma)
AIC(myarma1)
AIC(myarma2)
BIC(best.mod.arma)
BIC(myarma1)
BIC(myarma2)
AIC(newarima)
BIC(newarima)
Box.test(resid(newarma),type="Box-Pierce")
Box.test(resid(newarma),type="Ljung-Box")
acf(resid(newarma))
pacf(resid(newarma))
#Now fit SARIMA to original data
mod_sarima1<-auto.arima(y=log(x),trace=T,ic="aic")
summary(mod_sarima1)
Box.test(resid(mod_sarima1),type="Box-Pierce")
Box.test(resid(mod_sarima1),type="Ljung-Box")
acf(resid(mod_sarima1))
pacf(resid(mod_sarima1))
test(resid(mod_sarima1))
shapiro.test(resid(mod_sarima1))
checkresiduals(mod_sarima1)
mod_sarima2<-auto.arima(y=log(x),trace=T,ic="bic")
summary(mod_sarima2)
Box.test(resid(mod_sarima2),type="Box-Pierce")
Box.test(resid(mod_sarima2),type="Ljung-Box")
acf(resid(mod_sarima2),main="Residuals of SARIMA(1,1,0,1,0,1)")
pacf(resid(mod_sarima2),main="Residuals of SARIMA(1,1,0,1,0,1)")
test(resid(mod_sarima2))
shapiro.test(resid(mod_sarima2))
checkresiduals(mod_sarima2)
acf(rnorm(1000))
pacf(rnorm(1000))
#Completed
