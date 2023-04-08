
library(tseries)
library(forecast)
library(ggplot2)

### Estimate AR(P) or ARMA(p,0) 

# get gdp quarterly data from fred stat
data=read.csv("C:/Users/dhruv/Documents/School/Time Series/code/gdp.csv")

y=ts(data$GDP, start=c(1947,1), end=c(2022,4), frequency=4)

### compute gdp growth: GDP non-stationary but growth
## stationary

y=diff(log(y))*100


## drop NA from first quarter of missing growth

y=na.omit(y)

### visualize data
plot(y)

acf(y)

pacf(y)

### select optimal lags

T=length(y)
pmax=8
p = seq(1,pmax, 1)
aic =double(length(p))
bic = double(length(p))

for (i in seq(along = p)) {
  k = p[i]
  out = arima(y,order=c(k,0,0))
  aic[i] = AIC(out)
  bic[i] = AIC(out, k = log(T))

}

select <- cbind(p,aic, bic)
dimnames(select) <- list(NULL, c("lag", "AIC","BIC"))
select


plot(aic,type="l")
plot(bic,type="l")

#### Final Model AR(2) as per bic and  AIC

fit1=arima(y,order=c(2,0,0))
summary(fit1)




###### forecast based on the AR(2)

fcast=forecast(fit1,h=8)
fcast


plot(fcast,include=8)


###### add horizonatl line at long run mean of mu_y=intercept of fitted model
abline(h=coef(fit1)[2],lty=3,lwd=2,col="red")

