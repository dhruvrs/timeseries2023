library(readr)
library(stargazer)
# importing data
data <- read_csv('C:/Users/dhruv/Documents/School/Time Series/code/varData.csv')
income <- read_csv('C:/Users/dhruv/Documents/School/Time Series/code/income.csv')

# convert data points to time series objects
overall = ts(data$realOverall, start=c(2000,2), end=c(2022,3), frequency= 4)
pce= ts(data$realpcedur, start= c(2000,2), end= c(2022,3), frequency=4)
income = ts(income$DSPIC96, start= c(2000,2), end= c(2022,3), frequency=4)



# diffOverall = na.omit(diff(log(overall)))
# diffPce = na.omit(diff(log(pce)))
# diffIncome = na.omit(diff(log(income)))

library(seasonal)
fit = seas(overall, x11='')
seasonalOverall = fit$data[,3]

plot(seasonalOverall)
library(vars)

# vector of y: this will be without income
yvector = na.omit(ts.union(seasonalOverall, diffPce))

xvector = na.omit(ts.union(seasonalOverall, diffPce, diffIncome))

# selecting the optimal lags
VARselect(yvector, lag.max=8, type=c('const'))
# without income it seems that three lags is optimal
VARselect(xvector, lag.max=8, type=c('const'))

# estimating without income model: 3 lags
noIncome = VAR(yvector, p=3, type=c('const'))
summary(noIncome)

# estimating with income model: 3 lags
withIncome = VAR(xvector, p=3, type=c('const'))
summary(withIncome)


# determine causality for without income model
causality(noIncome, cause='seasonalOverall')$Granger
causality(noIncome, cause='diffPce')$Granger
# determine causality for with income model
causality(withIncome, cause='seasonalOverall')$Granger
causality(withIncome, cause='diffPce')$Granger
causality(withIncome, cause='diffIncome')$Granger

# residuals for each varaible of the var model
e = residuals(noIncome)
x = residuals(withIncome)

# compute rmse-insample for each variable
sqrt(colMeans(e^2))

# compute impulse response only for the without income model
impulseOverall = irf(noIncome, impulse='seasonalOverall', ortho=TRUE, n.ahead=24, response=c('diffPce'))
impulsePce = irf(noIncome, impulse='diffPce', ortho=TRUE, n.ahead=24, response=c('seasonalOverall'))

# plotting the impulse response
plot(impulseOverall)
plot(impulsePce)

# variance decomposition
fevd(noIncome, n.ahead=12)

