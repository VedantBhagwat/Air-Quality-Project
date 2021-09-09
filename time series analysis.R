library(tidyverse)
library(lubridate)
# install.packages("timetk")
library(timetk)
library(imputeTS)
library(ggplot2)

NO2_copy <- NO2
# Date19 <- seq(as.POSIXct("2019-01-01 00:00:00"), as.POSIXct("2019-12-31 24:00:00"), by="hour")
# Date20 <- seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-06-27 15:00:00"), by="hour")
# Date <- seq(as.POSIXct("2019-01-01 00:00:00"), as.POSIXct("2020-06-27 15:00:00"), by="hour")
NO2_copy$Date2 <- seq(as.POSIXct("2019-01-01 00:00:00"), as.POSIXct("2020-06-27 15:00:00"), by="hour")
which(duplicated(NO2_copy$Date)) # 10874
NO2_copy$Date[duplicated(NO2_copy$Date)] # "2020-03-29 01:00:00"
# match(NO2_copy$Date,"2020-03-29 01:00:00")

which(duplicated(NO$Date))

# data.frame(V1_V2=union(NO2_copy$Date, Date))
# library(dplyr)
# setdiff(NO2_copy$Date,Date)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE

# Time series plots
NO2 %>% 
  plot_time_series(as.Date(Date), UCC, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)

NO2 %>% 
  plot_time_series(as.Date(NO2$Date), NO2$Rathmines, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)

NO2 %>% 
  plot_time_series(as.Date(NO2$Date), NO2$Waterford, 
                   .interactive = interactive,
                   .plotly_slider = TRUE)

# Grouped data
NO2_combined %>%
  group_by(values) %>%
  plot_time_series(as.Date(Date), value, 
                   .facet_ncol = 5, .facet_scales = "free",
                   .interactive = interactive,
                   .title = "Time Series Plot of NO2 in 2018, 2019 and 2020")

NO_combined %>%
  group_by(values) %>%
  plot_time_series(as.Date(Date), value, 
                   .facet_ncol = 5, .facet_scales = "free",
                   .interactive = interactive,
                   .title = "Time Series Plot of NO in 2019 and 2020")

O3_combined %>%
  group_by(values) %>%
  plot_time_series(as.Date(Date), value, 
                   .facet_ncol = 5, .facet_scales = "free",
                   .interactive = interactive,
                   .title = "Time Series Plot of O3 in 2019 and 2020")

PM2_5_combined %>%
  group_by(values) %>%
  plot_time_series(as.Date(Date), value, 
                   .facet_ncol = 5, .facet_scales = "free",
                   .interactive = interactive,
                   .title = "Time Series Plot of PM2.5 in 2019 and 2020")

PM10_combined %>%
  group_by(values) %>%
  plot_time_series(as.Date(Date), value, 
                   .facet_ncol = 5, .facet_scales = "free",
                   .interactive = interactive,
                   .title = "Time Series Plot of PM10 in 2019 and 2020")

# data1 <- na_ma(NO2$Limerick.Peoples.Park, k = 4, weighting = "exponential")

# plot the data with ggplot
# ggplot(data = NO2, aes(x = as.Date(Date), y = Limerick.Peoples.Park)) +
#   geom_line()

sum(is.na.data.frame(X2019_Hourly_NO2_for_UCC)) # 46972

X2019_Hourly_NO2_for_UCC_new <- X2019_Hourly_NO2_for_UCC[,c(T,(nrow(X2019_Hourly_NO2_for_UCC)*0.25 > missing_data_NO2_2019$value))]
# reduced from 24 columns to  18 columns

sum(is.na.data.frame(X2019_Hourly_NO2_for_UCC_new)) # 6162

summary(X2019_Hourly_NO2_for_UCC_new)

boxplot(X2019_Hourly_NO2_for_UCC_new[,-1])
boxplot(X2019_Hourly_NO2_for_UCC_new$Davitt.Road)



a <- X2019_Hourly_NO2_for_UCC_new[X2019_Hourly_NO2_for_UCC_new$Davitt.Road < 0,]
for(i in 2:ncol(a)){
  a[is.na(a[,i]), i] <- 0
  a[,i] <- as.double(a[,i])
}
sum(is.na.data.frame(a))


# replace NA's with median
for(i in 2:ncol(X2019_Hourly_NO2_for_UCC_new)){
  X2019_Hourly_NO2_for_UCC_new[is.na(X2019_Hourly_NO2_for_UCC_new[,i]), i] <- format(round(median(X2019_Hourly_NO2_for_UCC_new[,i], na.rm = TRUE), 2), nsmall = 2)
  X2019_Hourly_NO2_for_UCC_new[,i] <- as.double(X2019_Hourly_NO2_for_UCC_new[,i])
}

sum(is.na.data.frame(X2019_Hourly_NO2_for_UCC_new)) # 6162
which(is.na.data.frame(X2019_Hourly_NO2_for_UCC_new)) # 2138
X2019_Hourly_NO2_for_UCC_new[2138,]
is.na(X2019_Hourly_NO2_for_UCC_new[2138,]) 



