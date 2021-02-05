library(fBasics)
library(forecast) 


# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
data<-read.csv("ARIMA.csv",header=TRUE,sep=";",dec=",")

y<-data[,2][1:90] # leave the last 6 observations to compare with the forecasted values

# achieving stationarity and identifying the model

par(mar=c(1,1,1,1)) # to adjust graphic size

ts.plot(y)  # Stationary Model, no need for transformation

par(mfrow=c(2,1))
acf(y)  
pacf(y)

ndiffs(y, alpha=0.05, test=c("adf"))
Box.test(y,lag=10)


# estimating the model
fit<-arima(y,order=c(0,0,0))    # we go with AR(0). then check for WN in residuals
fit # we find the information about the estimated parameters

par(mfrow=c(3,1))
ts.plot(fit$residuals)
acf(fit$residuals)
pacf(fit$residuals)    

Box.test(fit$residuals,lag=10)
#already white noise?

#Do we need a non-linear model?
par(mfrow=c(3,1))
ts.plot(fit$residuals^2)
acf(fit$residuals^2)
pacf(fit$residuals^2) 

Box.test(fit$residuals^2,lag=10)

# testing for normality 
shapiro.test(fit$residuals)  # 95% confidence intervals are robust for any kind of distribution

hist(fit$residuals,prob=T,ylim=c(0,0.25),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")


# Forecasting
y.pred<-predict(fit,n.ahead=6)
y.pred$pred # point predictions
y.pred$se  # standard error for the point predictions to compute confidence intervals

ts.plot(y)
lines(y.pred$pred,col="red")
lines(y.pred$pred+1.96*y.pred$se,col="red",lty=3)
lines(y.pred$pred-1.96*y.pred$se,col="red",lty=3)

# Comparing the real values with the predicted ones
real<-data[,2][91:96] 
c=1:6
plot(c,real,type="b",col="red")
lines(c,y.pred$pred,col="blue",type="b")
legend("bottomleft",c("real","forecast"),
       col = c("red","blue"),pch = c(1,1),bty ="n" )

# computing quantiles to the residual distribution

quantile(fit$residuals, probs=c(0.025,0.975)) # 95% confidence interval
-1.96*sd(fit$residuals)

quantile(fit$residuals, probs=c(0.1,0.9)) # 80% confidence interval
-1.28*sd(fit$residuals)

quantile(fit$residuals, probs=c(0.2,0.8)) # 60% confidence interval
-0.84*sd(fit$residuals)

