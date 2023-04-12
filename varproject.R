library(readr)
library(xts)
dat = read.csv("C:/Users/annaf/Downloads/varData.csv")
inc= read.csv("C:/Users/annaf/Downloads/inc.csv")

y= ts(dat$realOverall, start=c(2000,2), end=c(2022,3), frequency= 4)
pce= ts(dat$realpcedur, start= c(2000,2), end= c(2022,3), frequency=4)

#compute deseasonalized data
ys= decompose(y, type="additive")
y=y-ys$seasonal

df= data.frame(y, pce, inc)

#testing for unit root
library(urca)

summary(ur.df(y, type="none", lags=10, selectlags= "AIC")) #don't reject
summary(ur.df(y, type= "drift", lags=10, selectlags= "AIC")) #don't reject
summary(ur.df(y, type="trend", lags= 10, selectlags= "AIC")) #don't reject

#testing for unit root in first differences
summary(ur.df(diff(y), type="none", lags=10, selectlags= "AIC")) #reject
summary(ur.df(diff(y), type= "drift", lags=10, selectlags= "AIC")) #reject
summary(ur.df(diff(y), type="trend", lags= 10, selectlags= "AIC")) #reject

y= diff(y)

#testing for unit root in pce
summary(ur.df(pce, type="none", lags=10, selectlags= "AIC")) #don't reject
summary(ur.df(pce, type= "drift", lags=10, selectlags= "AIC")) #don't reject
summary(ur.df(pce, type="trend", lags= 10, selectlags= "AIC")) #don't reject

#first differences?
summary(ur.df(diff(pce), type="none", lags=10, selectlags= "AIC"))
summary(ur.df(diff(pce), type= "drift", lags=10, selectlags= "AIC"))
summary(ur.df(diff(pce), type="trend", lags= 10, selectlags= "AIC"))

pce= diff(pce)

#select lags
### select optimal lags
#STEP 2 is to select optimal number of lags. To do this,
#we first have to declare an ORDERING.
library(vars)

#declare vector of y:
yvector= na.omit(ts.union(y, pce))

#select optimal lag:
VARselect(yvector, lag.max= 8, type=c("const"))

#estimate VAR(3)
varmodel= VAR(yvector, p=3, type=c("const"))
summary(varmodel)


causality(varmodel, cause="y")$Granger
causality(varmodel, cause= "pce")$Granger

library(forecast)
fcast= forecast(varmodel, h=6)
plot(fcast, include=50)

#residuals for each variable of the var model
e= residuals(varmodel)

#manually compute RMSE-insample for each variable
sqrt(colMeans(e^2))

impulse_y=irf(varmodel,impulse="y",ortho= TRUE,n.ahead=24,response=c("pce"))
impulse_u=irf(varmodel,impulse="pce",ortho=TRUE,n.ahead=24,response=c("y"))

#plot IFR
plot(impulse_y)
plot(impulse_u)

impulse_u

#finally, we can compute variance decomposition for each variable
fevd(varmodel, n.ahead=12)
