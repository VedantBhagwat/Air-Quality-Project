######################################
# Daily time series
######################################

mo <- strftime(NO2_clone$Date, "%m")
yr <- strftime(NO2_clone$Date, "%Y")
dd <- data.frame(mo, yr, NO2_clone$UCC)
colnames(dd) <- c("month","year","UCC")

dd.agg <- aggregate(dd$UCC ~ mo + yr, dd, FUN = mean)
colnames(dd.agg) <- c("month","year","UCC")

NO2_ts_monthly <- ts(dd.agg$UCC, start = c(2018,1), frequency = 12)
ts_info(NO2_ts_monthly)

ts_plot(NO2_ts_monthly, title="Monthly time series plot of NO2 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")

acf2(NO2_ts_monthly)

# Season plot
ggseasonplot(window(NO2_ts_monthly), year.labels=TRUE) +
  ggtitle("Monthly NO2 season plot of UCC station") + ylab("Values (in ppb)")

ggsubseriesplot(window(NO2_ts_monthly), year.labels=TRUE) +
  # geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  # theme_minimal() +
  # theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("NO2 season plot of UCC station") + ylab("Values (in ppb)")




#########################################
# Benchmark method to forcast
#########################################
fit_monthly_snaive <- snaive(NO2_ts_monthly)
print(summary(fit_monthly_snaive))
# Residual sd: 1.2478 
# 
# Error measures:
#                     ME     RMSE       MAE       MPE     MAPE     MASE     ACF1
# Training set -0.3838428 1.247775 0.9748348 -16.54531 26.12249    1    -0.02120819
checkresiduals(fit_monthly_snaive)


#########################################
# Moving Average
#########################################
plot(NO2_ts_monthly, main="Monthly plot of NO2 data from UCC station", xlab="Year", ylab="NO2 (in ppb)")
fit_monthly_ma <- ma(NO2_ts_monthly, order = 4)
lines(fit_monthly_ma, col = "red")
# print(summary(fit_monthly_ma))
checkresiduals(fit_monthly_ma)


#########################################
# Fit ETS model
#########################################
fit_monthly_ets <- ets(NO2_ts_monthly)
# "ANN" is simple exponential smoothing with additive errors
print(summary(fit_monthly_ets))
# sigma:  1.5654
# 
# AIC     AICc      BIC 
# 132.8554 133.7785 137.0590 
# 
# Training set error measures:
#                     ME    RMSE      MAE       MPE     MAPE     MASE     ACF1
# Training set -0.1485354 1.51234 1.242442 -8.647331 27.83369 1.274515 0.147657
checkresiduals(fit_monthly_ets)


#########################################
# Fit ARIMA model
#########################################
fit_monthly_arima1 <- auto.arima(NO2_ts_monthly)
# "ANN" is simple exponential smoothing with additive errors
print(summary(fit_monthly_arima1))
# ARIMA(0,0,0)(0,1,0)[12] 
# 
# sigma^2 estimated as 1.557:  log likelihood=-29.53
# AIC=61.05   AICc=61.3   BIC=61.94
# 
# Training set error measures:
#                     ME      RMSE       MAE       MPE     MAPE      MASE       ACF1
# Training set -0.2281135 0.9665293 0.5870931 -9.887186 15.71349 0.6022488 0.02627233
checkresiduals(fit_monthly_arima1)

fit_monthly_arima2 <- auto.arima(NO2_ts_monthly,d=1,D=1,stepwise = F,approximation = F,trace = T) # sigma/Residual sd:  sqrt(14.07)=3.751
# Best model: ARIMA(0,1,1)(0,1,0)[12]
print(summary(fit_monthly_arima2))
# sigma^2 estimated as 1.675:  log likelihood=-28.96
# AIC=61.93   AICc=62.78   BIC=63.59
# 
# Training set error measures:
#                   ME      RMSE      MAE       MPE     MAPE    MASE        ACF1
# Training set 0.1235898 0.9452388 0.584394 -0.162004 13.51379 0.59948 -0.01290534
checkresiduals(fit_monthly_arima2)


#########################################
# Regression line to the TS
#########################################
fit_monthly_LR1 <- tslm(NO2_ts_monthly ~ trend)
print(summary(fit_monthly_LR1))
accuracy(fit_monthly_LR1)
#                     ME     RMSE      MAE       MPE     MAPE     MASE      ACF1
# Training set -1.035774e-16 1.963482 1.642422 -20.47694 42.59494 1.684821 0.6773871

fit_monthly_LR2 <- tslm(NO2_ts_monthly ~ trend + season )
print(summary(fit_monthly_LR2))
accuracy(fit_monthly_LR2)
#                     ME      RMSE       MAE       MPE     MAPE      MASE        ACF1
# Training set 1.480273e-17 0.6461706 0.5116688 -1.984315 10.61939 0.5248775 -0.08035256





#########################################
# Forecast ARIMA models
#########################################
fcst_monthly1 <- forecast(fit_monthly_arima1,h=6)
autoplot(fcst_monthly1) +
  ggtitle("Forecast of Monthly NO2 data using ARIMA") +
  xlab("Year") + ylab("NO2 values (in ppb)")
print(summary(fcst_monthly1))

fcst_monthly2 <- forecast(fit_monthly_arima2,h=6)
autoplot(fcst_monthly2) +
  ggtitle("Forecast of Monthly NO2 data using ARIMA") +
  xlab("Year") + ylab("NO2 values (in ppb)")

#########################################
# Forecast LR model
#########################################
fcst_monthly3 <- forecast(fit_monthly_LR2,h=6)
autoplot(fcst_monthly3) +
  ggtitle("Forecast of Monthly NO2 data using tslm model") +
  xlab("Year") + ylab("NO2 values (in ppb)")

#########################################
# Forecast ETS model
#########################################
fcst_monthly4 <- forecast(fit_monthly_ets,h=6)
autoplot(fcst_monthly4) +
  ggtitle("Forecast of Monthly NO2 data using tslm model") +
  xlab("Year") + ylab("NO2 values (in ppb)")