library(readr)

#import data
data <- read.csv('C:/Users/dhruv/Documents/School/Time Series/code/final code/final.csv')
recDf <- read.csv('C:/Users/dhruv/Documents/School/Time Series/code/final code/rec.csv')

real=ts(data$realGross, start=c(2002,1), end=c(2022,3), frequency=4)
rec=ts(recDf$rec, start=c(2002,1), end=c(2022,3), frequency=4)

# logging both variables before checking for stationarity
rec= log(rec)
real = log(real)
plot(real)
# unit root testing, rec is non-stationary until the first difference
library(urca)

summary(ur.df(rec,type="none", lags=10, selectlags ="AIC"))#non
summary(ur.df(rec,type="drift", lags=10, selectlags ="AIC"))#non
summary(ur.df(rec,type="trend", lags=10, selectlags ="AIC"))#non
plot(rec)
summary(ur.df(diff(rec),type="none", lags=10, selectlags ="AIC"))#st at 10%
summary(ur.df(diff(rec),type="drift", lags=10, selectlags ="AIC"))#st at 10%
summary(ur.df(diff(rec),type="trend", lags=10, selectlags ="AIC"))#st

#difference the real value

library(seasonal)
y = real
fit = seas(y, x11='')
sreal = fit$data[,3]
plot(sreal)
dreal= sreal
drec = diff(rec)
# inc = diff(log(inc))
plot(real)
library(vars)
# declare vector of y
# withIncome = ts.union(inc, rec, real)
noIncome = na.omit(ts.union(drec, dreal))
# select optimal lag
VARselect(noIncome, lag.max=8, type=c('const'))

# choose aic 7 lags with no income, with income is one lag
firstModel = VAR(noIncome, p=1, type=c('const'))
summary(firstModel)
# secondModel = VAR(withIncome, p=1, type=c('const'))
# summary(secondModel)

# causality for no income
causality(firstModel,cause="drec")$Granger
causality(firstModel,cause="dreal")$Granger

# final forecasting
library(forecast)
fcast = forecast(firstModel, h = 4)
plot(fcast,include=24)


# library(forecast)
# fcast = forecast(firstModel, h = 4)
# plot(fcast,include=24)
e=residuals(firstModel)
sqrt(colMeans(e^2))

# orthogonal impulse response
impulseReal=irf(firstModel,impulse="dreal",ortho = TRUE, n.ahead=12, response=c("drec"))
impulseRec=irf(firstModel,impulse="drec",ortho = TRUE, n.ahead=12, response=c("dreal"))

plot(impulseRec, main='Orthogonal Impulse Response from Growth in Recreational Spending',
     ylab='Box Office Revenues')

# Split the data into training and testing sets
train_size <- floor(0.8 * nrow(noIncome))
train_data <- noIncome[1:train_size, ]
test_data <- noIncome[(train_size + 1):nrow(noIncome), ]

# Fit the VAR model to the training data
model <- VAR(train_data, p = 7, type = "const")

# Forecast the next 4 quarters using the test data
forecast <- predict(model, n.ahead = 4, newdata = test_data)

# Extract the predicted values from the forecast object
predicted <- as.matrix(forecast$fcst[, c("drec_growth", "drealGross")])

# Create a data frame with the same dimensions as the test data
predicted_df <- data.frame(
  drec = predicted[, 1],
  dreal = predicted[, 2]
)

# Plot the actual vs. predicted values for the test data
actual <- test_data[, c("drec", "dreal")]