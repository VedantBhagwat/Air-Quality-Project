######################################
# Daily time series
######################################

# Daily time series
NO2_clone$Date <- as.Date(NO2_clone$Date, "%m/%d/%Y")
ag <- aggregate(UCC ~ Date, NO2_clone, mean)
# View(ag)

NO2_ts_daily <- ts(ag$UCC, start = c(2018,1), frequency = 365)
ts_info(NO2_ts_daily)

ts_plot(NO2_ts_daily, title="Daily time series plot of NO2 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")

acf(NO2_ts_daily)
pacf(NO2_ts_daily)

# Season plot
ggseasonplot(window(NO2_ts_daily), year.labels=TRUE) +
  ggtitle("Season plot daily NO2 from UCC station") + ylab("Values (in ppb)")

# Trend and Seasonality
# NO2.add = decompose(NO2_ts_daily, "additive")
# autoplot(NO2.add)
# NO2.mul = decompose(NO2_ts_daily, "multiplicative")
# autoplot(NO2.mul)

#########################################
# Benchmark method to forcast
#########################################
fit_daily_snaive <- snaive(NO2_ts_daily)
print(summary(fit_daily_snaive))
# Residual sd: 4.1858 
# 
# Error measures:
#   ME     RMSE     MAE       MPE    MAPE MASE      ACF1
# Training set -0.3759306 4.185775 3.09828 -79.19421 117.141    1 0.4598426
checkresiduals(fit_daily_snaive)


#########################################
# Moving Average
#########################################
plot(NO2_ts_daily, main="Daily plot of NO2 data from UCC station", xlab="Year", ylab="NO2 (in ppb)")
fit_daily_ma <- ma(NO2_ts_daily, order = 4)
lines(fit_daily_ma, col = "blue")
# print(summary(fit_daily_ma))
checkresiduals(fit_daily_ma)


#########################################
# Fit ETS model
#########################################
fit_daily_ets <- ets(NO2_ts_daily)
# "ANN" is simple exponential smoothing with additive errors
print(summary(fit_daily_ets))
# sigma:  2.8945
# 
# AIC     AICc      BIC 
# 8128.607 8128.633 8143.044 
# 
# Training set error measures:
#                     ME     RMSE      MAE      MPE MAPE      MASE      ACF1
# Training set -0.009933112 2.891305 2.080104 -Inf  Inf   0.6713737 0.1344075
checkresiduals(fit_daily_ets)


#########################################
# Fit ARIMA model
#########################################
fit_daily_arima1 <- auto.arima(NO2_ts_daily)
# "ANN" is simple exponential smoothing with additive errors
print(summary(fit_daily_arima1))
# ARIMA(5,0,2) with non-zero mean 
#
# sigma^2 estimated as 7.502:  log likelihood=-2202.12
# AIC=4422.25   AICc=4422.45   BIC=4465.56
# 
# Training set error measures:
#                     ME     RMSE      MAE      MPE MAPE      MASE        ACF1
# Training set -0.01786541 2.726837 2.012439 -Inf  Inf    0.6495341 0.004810739
checkresiduals(fit_daily_arima1)

fit_daily_arima2 <- auto.arima(NO2_ts_daily,d=1,D=1,stepwise = F,approximation = F,trace = T) # sigma/Residual sd:  sqrt(14.07)=3.751
# Best model: ARIMA(1,1,3)(0,1,0)[365]
print(summary(fit_daily_arima2))
# sigma^2 estimated as 14.2:  log likelihood=-1493.04
# AIC=2996.08   AICc=2996.19   BIC=3017.56
# 
# Training set error measures:
#                       ME     RMSE      MAE  MPE MAPE    MASE       ACF1
# Training set -0.00112832 2.901253 1.675897 -Inf  Inf  0.5409121 0.01200697
checkresiduals(fit_daily_arima2)



#########################################
# Regression line to the TS
#########################################
fit_daily_LR1 <- tslm(NO2_ts_daily ~ trend)
print(summary(fit_daily_LR1))
accuracy(fit_daily_LR1)
#                   ME       RMSE      MAE    MPE MAPE      MASE     ACF1
# Training set 1.677089e-15 5.4745 3.827591 -Inf  Inf   0.8732225 0.883363

fit_daily_LR2 <- tslm(NO2_ts_daily ~ trend + season )
print(summary(fit_daily_LR2))
accuracy(fit_daily_LR2)
#                     ME     RMSE      MAE      MPE MAPE      MASE      ACF1
# Training set 1.472735e-16 2.352515 1.745353 -Inf  Inf   0.5633295 0.4765114






#########################################
# Forecast ARIMA models
#########################################
fcst_daily1 <- forecast(fit_daily_arima1,h=180)
autoplot(fcst_daily1) +
  ggtitle("Forecast of Daily NO2 using ARIMA") +
  xlab("Year") + ylab("NO2 values (in ppb)")
print(summary(fcst_daily1))

fcst_daily2 <- forecast(fit_daily_arima2,h=180)
autoplot(fcst_daily2) +
  ggtitle("Forecast of Daily NO2 using ARIMA") +
  xlab("Year") + ylab("NO2 values (in ppb)")

#########################################
# Forecast LR model
#########################################
fcst_daily3 <- forecast(fit_daily_LR2,h=180)
autoplot(fcst_daily3) +
  ggtitle("Forecast of Daily NO2 using tslm model") +
  xlab("Year") + ylab("NO2 values (in ppb)")

#########################################
# Forecast ETS model
#########################################
fcst_daily4 <- forecast(fit_daily_ets,h=180)
autoplot(fcst_daily4) +
  ggtitle("Forecast of Daily NO2 using ETS model") +
  xlab("Year") + ylab("NO2 values (in ppb)")


