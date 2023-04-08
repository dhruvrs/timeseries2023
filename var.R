library(readr)

## importing data
data <- read_csv('C:/Users/dhruv/Documents/School/Time Series/code/varData.csv')

## declaring time series for each variable
overall = ts(data$realOverall, start=c(2000,2), end=c(2022, 4), frequency=4)
pce = ts(data$realpcedur, start=c(2000,2), end=c(2022, 4), frequency=4)

## plot in same plot
ts.plot(overall)
ts.plot(pce)
ts.plot(cbind(overall, pce), col=c('red', 'blue'), lty=c('solid', 'dashed'))

library(urca)
# the overall variable is looking weird now
# but i think that they are both still non stationary
summary(ur.df(overall,type="drift", lags=10, selectlags="AIC"))
summary(ur.df(pce,type="drift", lags=10, selectlags="AIC"))

# make stationary
summary(ur.df(na.omit(diff(pce)),type="drift", lags=10, selectlags="AIC"))
summary(ur.df(na.omit(diff(overall)),type="drift", lags=10, selectlags="AIC"))
# I am pretty sure that made the variable stationary

# Ordering of var model
library(vars)

yvector = ts.union(overall, pce)

VARselect(yvector, lag.max=8,type=c('const'))
# optimal numbers of lags is 2 according to SC
varmodel=VAR(yvector, p=4, type=c('const'))
summary(varmodel)

causality(varmodel,cause="pce")$Granger
causality(varmodel,cause="overall")$Granger

library(forecast)
fcast = forecast(varmodel, h = 4)
plot(fcast,include=24)

### residuals for each variable of the var model
e=residuals(varmodel)
### manually compute RMSE-insample for each variable
sqrt(colMeans(e^2))

### compute response for 24 quarters to an orthogonal shock to ffr
impulse_inf=irf(varmodel,impulse="overall",ortho = TRUE, n.ahead=24, response=c("pce"))
## plot IRF
plot(impulse_inf)
