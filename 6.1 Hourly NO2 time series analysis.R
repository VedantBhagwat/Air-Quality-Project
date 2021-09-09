library(reshape)
NO2_combined <- melt(NO2, id.vars=c('Date'),var='values')

######################################
# Hourly time series
######################################
kpss.test(NO2_ts_hourly)
library(tseries)
adf.test(NO2_ts_hourly)
adf.test(NO2_ts_monthly)

NO2_ts_hourly <- ts(NO2_clone$UCC, start = c(2018,1), frequency = 365*24)
# View(NO2_ts_hourly)
ts_info(NO2_ts_hourly)
ts_plot(NO2_ts_hourly, title="Hourly time series plot of NO2 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")

# Season plot
ggseasonplot(window(NO2_ts_hourly), year.labels=TRUE) +
  ggtitle("Season plot hourly NO2 from UCC station") + ylab("Values (in ppb)")

acf(NO2_ts_hourly)
pacf(NO2_ts_hourly)

# Trend and Seasonality
# NO2.add = decompose(NO2_ts_hourly, "additive")
# autoplot(NO2.add)
# NO2.mul = decompose(NO2_ts_hourly, "multiplicative")
# autoplot(NO2.mul)

#########################################
# Benchmark method to forcast
#########################################
fit_hourly_snaive <- snaive(NO2_ts_hourly)
print(summary(fit_hourly_snaive))
# Residual sd: 6.6211 
# 
# Error measures:
#   ME     RMSE      MAE  MPE MAPE MASE      ACF1
# Training set -0.374954 6.621052 4.322946 -Inf  Inf    1 0.8683134
checkresiduals(fit_hourly_snaive)


#########################################
# Moving Average
#########################################
plot(NO2_ts_hourly)
fit_hourly_ma <- ma(NO2_ts_hourly, order = 8)
lines(fit_hourly_ma, col = "red")
# print(summary(fit_hourly_ma))
checkresiduals(fit_hourly_ma)


#########################################
# Fit ETS model
#########################################
fit_hourly_ets <- ets(NO2_ts_hourly)
# "ANN" is simple exponential smoothing with additive errors
print(summary(fit_hourly_ets))
# sigma:  2.644
# 
# AIC     AICc      BIC 
# 260274.9 260274.9 260298.9 
# 
# Training set error measures:
#                     ME     RMSE      MAE      MPE MAPE      MASE      ACF1
# Training set 1.394905e-05 2.643924 1.444758 -Inf  Inf   0.3342067 0.1985775
checkresiduals(fit_hourly_ets)


#########################################
# Fit ARIMA model
#########################################
fit_hourly_arima1 <- auto.arima(NO2_ts_hourly)
# "ANN" is simple exponential smoothing with additive errors
print(summary(fit_hourly_arima1))
# ARIMA(2,1,1)
# sigma^2 estimated as 5.976:  log likelihood=-50437.02
# AIC=100882.1   AICc=100882.1   BIC=100914
# 
# Training set error measures:
#                     ME     RMSE      MAE      MPE MAPE      MASE         ACF1
# Training set -0.0006261083 2.444348 1.418795 NaN  Inf   0.3236823 -0.001996141
checkresiduals(fit_hourly_arima1)

fit_hourly_arima2 <- auto.arima(NO2_ts_hourly,d=1,D=1,stepwise = F,approximation = F,trace = T) # sigma/Residual sd:  sqrt(14.07)=3.751
# Best model: ARIMA(1,1,3)(0,1,0)[365] 
fit_hourly_arima2 <- arima(NO2_ts_hourly, order=c(1,1,3), seasonal =  c(0,1,0))
print(summary(fit_hourly_arima2))
# sigma^2 estimated as 14.07:  log likelihood=-1490.54
# AIC=2991.08   AICc=2991.2   BIC=3012.57
# 
# Training set error measures:
#                     ME     RMSE      MAE      MPE MAPE      MASE       ACF1
# Training set -0.002039714 2.887955 1.660302 -Inf  Inf   0.5375387 0.01246659
checkresiduals(fit_hourly_arima2)



#########################################
# Regression line to the TS
#########################################
fit_hourly_LR1 <- tslm(NO2_ts_hourly ~ trend)
print(summary(fit_hourly_LR1))
accuracy(fit_hourly_LR1)
#                   ME     RMSE      MAE  MPE MAPE      MASE      ACF1
# Training set 6.070934e-16 5.474378 3.827293 -Inf  Inf 0.8853437 0.8833524

fit_hourly_LR2 <- tslm(NO2_ts_hourly ~ trend + season )
print(summary(fit_hourly_LR2))
accuracy(fit_hourly_LR2)
#                     ME     RMSE      MAE      MPE MAPE      MASE      ACF1
# Training set 2.541606e-16 3.671883 2.421236 NaN  Inf 0.5600892 0.8720057






#########################################
# Forecast ARIMA models
#########################################
fcst_hourly1 <- forecast(fit_hourly_arima1,h=180*24)
autoplot(fcst_hourly1) +
  ggtitle("Forecasts of hourly NO2 data using ARIMA model") +
  xlab("Year") + ylab("NO2 values (in ppb)")

print(summary(fcst_hourly1))

# fcst_hourly2 <- forecast(fit_hourly_arima2,h=180)
# autoplot(fcst_hourly2) +
#   ggtitle("Forecasts of NO2 using ARIMA") +
#   xlab("Year") + ylab("NO2 values (in ppb)")

#########################################
# Forecast LR model
#########################################
fcst_hourly3 <- forecast(fit_hourly_LR2,h=180*24)
# Error: cannot allocate vector of size 585.6 Mb
autoplot(fcst_hourly3) +
  ggtitle("Forecasts of hourly NO2 data using tslm model") +
  xlab("Year") + ylab("NO2 values (in ppb)")

#########################################
# Forecast ETS model
#########################################
fcst_hourly4 <- forecast(fit_hourly_ets,h=180*24)
autoplot(fcst_hourly4) +
  ggtitle("Forecasts of hourly NO2 data using ETS model") +
  xlab("Year") + ylab("NO2 values (in ppb)")










#########################################################
# Old code
#########################################################
# x2018_no2 <- data.frame(X2018_Hourly_NO2_for_UCC$Date, X2018_Hourly_NO2_for_UCC$UCC)
# x2019_no2 <- data.frame(X2019_Hourly_NO2_for_UCC$Date, X2019_Hourly_NO2_for_UCC$UCC)
# x2020_no2 <- data.frame(X2020_Hourly_NO2_for_UCC$Date, X2020_Hourly_NO2_for_UCC$UCC)
# 
# colnames(x2018_no2) <- c("Date", "UCC")
# colnames(x2019_no2) <- c("Date", "UCC")
# colnames(x2020_no2) <- c("Date", "UCC")
# 
# x2018_no2$Date <- as.POSIXct(x2018_no2$Date, format="%d/%m/%Y %H:%M")
# x2019_no2$Date <-  as.POSIXct(x2019_no2$Date, format="%d/%m/%Y %H:%M")
# x2020_no2$Date <- as.POSIXct(x2020_no2$Date, format="%d/%m/%Y %H:%M")
# 
# a_NO2 <- rbind(x2018_no2, x2019_no2, x2020_no2)
# View(a_NO2)
# summary(a_NO2)
# 
# 
# library(reshape2)
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(zoo)
# 
# library(astsa)
# library(forecast)
# library(TSstudio)
# library(timetk)
# library(xts)
# library(stats)
# 
# # Replace negative values with NA
# # a_NO2 <- a_NO2 %>% mutate_all(funs(replace(., .<0, NA)))
# a_NO2$UCC[a_NO2$UCC < 0] <- NA
# 
# 
# summary(a_NO2)
# 
# 
# # Last obs. carried forward
# a_NO2$UCC <- na.locf(a_NO2$UCC, na.rm = F) 
# 
# summary(a_NO2)
# 
# # Replace NA's with median
# # for(i in 2:ncol(a_NO2)){
# #   a_NO2[is.na(a_NO2[,i]), i] <- format(round(median(a_NO2[,i], na.rm = TRUE), 2), nsmall = 2)
# #   a_NO2[,i] <- as.double(a_NO2[,i])
# # }
# 
# NO2_ts_hourly <- ts(a_NO2$UCC, start = c(2018,1), frequency = 365*24)
# 