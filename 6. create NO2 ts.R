library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

library(astsa)
library(forecast)
library(TSstudio)
library(timetk)
library(xts)
library(stats)


#############################################
#        NO2 Time series analysis
#############################################
summary(NO2_clone$UCC)

# Hourly time series
NO2_ts_hourly <- ts(NO2_clone$UCC, start = c(2018,1), frequency = 365*24)
ts_info(NO2_ts_hourly)
ts_plot(NO2_ts_hourly, title="Hourly time series plot of NO2 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")

# Daily time series
NO2_clone$Date <- as.Date(NO2_clone$Date, "%m/%d/%Y")
ag <- aggregate(UCC ~ Date, NO2_clone, mean)
# View(ag)

NO2_ts_daily <- ts(ag$UCC, start = c(2018,1), frequency = 365)
ts_info(NO2_ts_daily)

ts_plot(NO2_ts_daily, title="Daily time series plot of NO2 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")
# plot.ts(NO2_ts_daily)
# autoplot(NO2_ts_daily)
# NO2_ts_daily %>% 
#   plot_time_series(as.Date(Date), UCC, 
#                    .interactive = interactive,
#                    .plotly_slider = TRUE)

# Monthly TS
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