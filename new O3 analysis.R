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
#        O3 Time series analysis
#############################################
summary(O3_clone$UCC)

# Hourly time series
O3_ts_hourly <- ts(O3_clone$UCC, start = c(2018,1), frequency = 365*24)
ts_info(O3_ts_hourly)
ts_plot(O3_ts_hourly, title="Hourly time series plot of O3 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")

# Daily time series
O3_clone$Date <- as.Date(O3_clone$Date, "%m/%d/%Y")
ag <- aggregate(UCC ~ Date, O3_clone, mean)
View(ag)

O3_ts_daily <- ts(ag$UCC, start = c(2018,1), frequency = 365)
ts_info(O3_ts_daily)

ts_plot(O3_ts_daily, title="Daily time series plot of O3 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")
# plot.ts(O3_ts_daily)
# autoplot(O3_ts_daily)
# O3_ts_daily %>% 
#   plot_time_series(as.Date(Date), UCC, 
#                    .interactive = interactive,
#                    .plotly_slider = TRUE)

# Monthly TS
mo <- strftime(O3_clone$Date, "%m")
yr <- strftime(O3_clone$Date, "%Y")
dd <- data.frame(mo, yr, O3_clone$UCC)
colnames(dd) <- c("month","year","UCC")

dd.agg <- aggregate(dd$UCC ~ mo + yr, dd, FUN = mean)
colnames(dd.agg) <- c("month","year","UCC")

O3_ts_monthly <- ts(dd.agg$UCC, start = c(2018,1), frequency = 12)
ts_info(O3_ts_monthly)

ts_plot(O3_ts_monthly, title="Monthly time series plot of O3 data from the UCC station",
        Xtitle="Year", Ytitle = "Values (in ppb)")

