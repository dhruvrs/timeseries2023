library(readr)
library(xts)
library(stargazer)
# reading in the data and then plotting it
df=read.csv('C:/Users/dhruv/Documents/School/Time Series/code/final code/final.csv')

real=ts(df$realGross, start=c(2000,1), end=c(2022,3), frequency=4)

plot.xts(as.xts(real))

#seasonalize with new function
library(seasonal)
y = log(real)
fit = seas(y, x11='')
sy = fit$data[,1]
# plot(sy)

# check for stationarity
library(urca)
summary(ur.df(sy,type="drift", lags=10, selectlags ="AIC"))#stationary at 5%
# plot(sy)

diffReal = na.omit(diff(sy))
# determining the optimal lag structure
pars = expand.grid(ar=1:6,d=0,ma=1:6)

# declaring vectors to hold the aic and bic values
aic <- rep(0, nrow(pars))
bic <- rep(0, nrow(pars))

# why is aic listed twice in this loop?
# for (i in seq(along=bic)) {
#   aic[i] <- AIC(arima(diffReal, unlist(pars[i,1:3]), method = 'ML', optim.control=list(maxit=5000)))
#   bic[i] <- BIC(arima(diffReal, unlist(pars[i,1:3]), method = 'ML', optim.control=list(maxit=5000)))
#   k=length(diffReal)
# }

pars[which.min(aic),]
pars[which.min(bic),]

# figuring out optimal degree
# p=seq(1,6,0)
# q=seq(1,6,0)
# group=expand.grid(p=p,q=q)
# cbind(group,aic,bic)

# forecast with BIC (1,1,2)
library(forecast)
final=Arima(diffReal,order=c(1,0,1), include.mean = T)

# Model evaluation
library(lmtest)

fe = lm(residuals(final) ~ 1)
a=rep(0,12)
b=rep(0,12)
### look for serial correlation from order 1 to 12
for (p in seq(along = rep(1,12))) {
  a[p]=bgtest(fe,order=p)$statistic
  b[p]=bgtest(fe,order=p)$p.value
}
results=data.frame(cbind(1:12, a,b))
#up till the twelfth lag there is no serial correlation

# final forecast
yf = forecast(final,h=6)
plot(yf)


library(readr)
library(xts)
library(stargazer)
# reading in the data and then plotting it
df=read.csv('C:/Users/dhruv/Documents/School/Time Series/code/final code/final.csv')

real=ts(df$realGross, start=c(2000,1), end=c(2022,3), frequency=4)

plot.xts(as.xts(real))

#seasonalize with new function
library(seasonal)
y = log(real)
fit = seas(y, x11='')
sy = fit$data[,1]
# plot(sy)

# check for stationarity
library(urca)
summary(ur.df(sy,type="drift", lags=10, selectlags ="AIC"))#stationary at 5%
# plot(sy)

diffReal = na.omit(diff(sy))
# determining the optimal lag structure
pars = expand.grid(ar=1:6,d=0,ma=1:6)

# declaring vectors to hold the aic and bic values
aic <- rep(0, nrow(pars))
bic <- rep(0, nrow(pars))

# why is aic listed twice in this loop?
# for (i in seq(along=bic)) {
#   aic[i] <- AIC(arima(diffReal, unlist(pars[i,1:3]), method = 'ML', optim.control=list(maxit=5000)))
#   bic[i] <- BIC(arima(diffReal, unlist(pars[i,1:3]), method = 'ML', optim.control=list(maxit=5000)))
#   k=length(diffReal)
# }
# 
# pars[which.min(aic),]
# pars[which.min(bic),]
# 
# # figuring out optimal degree
# # p=seq(1,6,0)
# # q=seq(1,6,0)
# # group=expand.grid(p=p,q=q)
# # cbind(group,aic,bic)
# 
# # forecast with BIC (1,1,2)
# library(forecast)
# final=Arima(diffReal,order=c(1,0,1), include.mean = T)
# 
# # Model evaluation
# library(lmtest)
# 
# fe = lm(residuals(final) ~ 1)
# a=rep(0,12)
# b=rep(0,12)
# ### look for serial correlation from order 1 to 12
# for (p in seq(along = rep(1,12))) {
#   a[p]=bgtest(fe,order=p)$statistic
#   b[p]=bgtest(fe,order=p)$p.value
# }
# results=data.frame(cbind(1:12, a,b))
# #up till the twelfth lag there is no serial correlation
# 
# # final forecast
# yf = forecast(final,h=6)
# plot(yf)
# 
# library(ggplot2)
# 
# # create a data frame with the time series data
# df <- data.frame(year=as.yearqtr(time(real)), real=real, group="Real GDP")
# 
# # create a data frame with the forecast data
# df2 <- data.frame(year=as.yearqtr(time(yf$mean)), forecast=exp(yf$mean), group="Forecast")
# 
# # combine the data frames into one
# df3 <- rbind(df, df2)
# df3$value <- ifelse(!is.na(df3$real), df3$real, df3$forecast)
# 
# # create the plot
# ggplot(df3, aes(x=year, y=value, color=group)) +
#   geom_line(size=1) +
#   labs(title="Real GDP and Forecast",
#        x="Year", y="Real GDP") +
#   scale_color_manual(values=c("Real GDP"="#0072B2", "Forecast"="#D55E00")) +
#   theme_minimal() +
#   theme(plot.title = element_text(size=20, face="bold", hjust=0.5),
#         axis.title = element_text(size=16, face="bold"),
#         axis.text = element_text(size=14),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_blank(),
#         panel.background = element_rect(fill="#F0F0F0"),
#         plot.background = element_rect(fill="#F0F0F0"))



#split into training and testing
train_size <- round(length(diffReal) * 0.8)
train_data <- diffReal[1:train_size]
test_data <- diffReal[(train_size + 1):length(diffReal)]

#model with training set
train_model <- Arima(train_data, order = c(1, 0, 1), include.mean = T)

#forecasting with the testing set of data
test_forecast <- forecast(train_model, h = length(test_data))

#i dont know which one he wants so i am just putting all of them
mae <- mean(abs(test_forecast$mean - test_data))
mse <- mean((test_forecast$mean - test_data)^2)
rmse <- sqrt(mse)

#printing
cat("MAE:", mae, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")

