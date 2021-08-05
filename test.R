

?ts
plot(X2020_Hourly_O3_for_UCC$Castlebar)
str(X2020_Hourly_O3_for_UCC)
summary(X2020_Hourly_O3_for_UCC)

# Convert string date to Date object
X2020_Hourly_O3_for_UCC$`Date & Time` <- as.Date(X2020_Hourly_O3_for_UCC$`Date & Time`)

# X2020_Hourly_O3_for_UCC$Castlebar <- as.double(X2020_Hourly_O3_for_UCC$Castlebar)

# Check NA values
# X2020_Hourly_O3_for_UCC$Castlebar <- na.omit(X2020_Hourly_O3_for_UCC$Castlebar)
is.na(X2020_Hourly_O3_for_UCC$Castlebar)
sum(is.na(X2020_Hourly_O3_for_UCC$Castlebar))

typeof(X2020_Hourly_O3_for_UCC)
# Convert data into data frame
X2020_Hourly_O3_for_UCC <- data.frame(X2020_Hourly_O3_for_UCC)

# Convert character numbers into double and replace NA values with mean
for(i in 2:ncol(X2020_Hourly_O3_for_UCC)){
  X2020_Hourly_O3_for_UCC[,i] <- as.double(X2020_Hourly_O3_for_UCC[,i])
  X2020_Hourly_O3_for_UCC[is.na(X2020_Hourly_O3_for_UCC[,i]), i] <- mean(X2020_Hourly_O3_for_UCC[,i], na.rm = TRUE)
}
str(X2020_Hourly_O3_for_UCC)


# Replace NA values with mean
# install.packages("imputeTS")
# library(imputeTS)
# X2020_Hourly_O3_for_UCC$Castlebar <- na_mean(X2020_Hourly_O3_for_UCC$Castlebar)

# install.packages("xts")
# library(xts)
# train_xts <- xts(x = X2020_Hourly_O3_for_UCC, order.by = X2020_Hourly_O3_for_UCC$`Date & Time`)
# # View(train_xts)

plot.ts(X2020_Hourly_O3_for_UCC$Bray)
plot.ts(X2020_Hourly_O3_for_UCC$Castlebar)

timeseries <- ts(X2020_Hourly_O3_for_UCC$Bray, frequency=12, start=c(2020,1))
# View(timeseries)
plot.ts(timeseries)

# Plot hourly data with xts package
# library(xts)
# X2020_Hourly_O3_for_UCC$starttime <- strptime(X2020_Hourly_O3_for_UCC$Date, "%d.%m.%Y %H:%M")
# hnew_dataxts <- xts(X2020_Hourly_O3_for_UCC[,"Bray"], order.by=X2020_Hourly_O3_for_UCC[,"Date"])
# 
# plot(hnew_dataxts)
# 
# (acf(X2020_Hourly_O3_for_UCC[,"Bray"]))

#######################################
# start.date <- as.POSIXct(strptime('2020-01-01 00:00:00', format="%Y-%m-%d %H:%M:%S"))  # the min from your df
# dat <- data.frame(
#   timestamp=as.POSIXct(seq.POSIXt(from=start.date, by="hour", length.out=4287)),
#   Bray=X2020_Hourly_O3_for_UCC$Bray)

# library(plotly)
# plot_ly(dat, x=timestamp, y=dat$Bray, text="Bray", type='scatter')

# library(zoo)
# new_dat <- zoo(X2020_Hourly_O3_for_UCC$Bray, as.POSIXct(seq.POSIXt(from=start.date, by="hour", length.out=length(X2020_Hourly_O3_for_UCC$Bray))))
# # plot.ts(new_dat)
# autoplot(new_dat)+xlab("days")+ylab("Values in ppm")


