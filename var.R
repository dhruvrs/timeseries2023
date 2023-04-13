library(readr)

## importing data
dat <- read_csv('C:/Users/dhruv/Documents/School/Time Series/code/varData.csv')
inc <- read_csv('C:/Users/dhruv/Documents/School/Time Series/code/income.csv')

y= ts(dat$realOverall, start=c(2000,2), end=c(2022,3), frequency= 4)
pce= ts(dat$realpcedur, start= c(2000,3), end= c(2022,3), frequency=4)
inc= ts(inc$DSPIC96, start= c(2000,2), end= c(2022,3), frequency=4)
inc=diff(log(inc))*100
y= diff(log(y))*100

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

#testing for stationarity in income
summary(ur.df(inc, type="none", lags=10, selectlags= "AIC"))
summary(ur.df(inc, type= "drift", lags=10, selectlags= "AIC")) 
summary(ur.df(inc, type="trend", lags= 10, selectlags= "AIC"))

summary(ur.df(diff(inc), type="none", lags=10, selectlags= "AIC")) #reject
summary(ur.df(diff(inc), type= "drift", lags=10, selectlags= "AIC")) #reject
summary(ur.df(diff(inc), type="trend", lags= 10, selectlags= "AIC"))

plot(y)
acf(y)
pacf(y)

#select lags
### select optimal lags
#STEP 2 is to select optimal number of lags. To do this,
#we first have to declare an ORDERING.
library(vars)

#declare vector of y:
yvector= na.omit(ts.union(y, pce, inc))
xvector = na.omit(ts.union(y,pce))
#select optimal lag:
VARselect(yvector, lag.max= 8, type=c("const"))
VARselect(xvector, lag.max= 8, type=c("const"))

#estimate VAR(3) and VAR(4)
varmodel= VAR(yvector, p=3, type=c("const"))
summary(varmodel)
withoutIncome = VAR(xvector, p=4, type=c('const'))
summary(withoutIncome)

causality(varmodel, cause="y")$Granger
causality(varmodel, cause= "pce")$Granger
causality(varmodel, cause= "inc")$Granger

causality(withoutIncome, cause="y")$Granger
causality(withoutIncome, cause= "pce")$Granger


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


#finally, we can compute variance decomposition for each variable
fevd(varmodel, n.ahead=12)