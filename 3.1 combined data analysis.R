# Find  no of rows in combined data
nrow(X2018_Hourly_NO2_for_UCC) + nrow(X2019_Hourly_NO2_for_UCC) + nrow(X2020_Hourly_NO2_for_UCC)
# 21809

# Combine 2018, 2019 and 2020 data
# NO2 <- rbind(X2019_Hourly_NO2_for_UCC, X2020_Hourly_NO2_for_UCC)
NO2 <- rbind(X2018_Hourly_NO2_for_UCC, X2019_Hourly_NO2_for_UCC, X2020_Hourly_NO2_for_UCC)

O3 <- rbind(X2018_Hourly_O3_for_UCC,X2019_Hourly_O3_for_UCC, X2020_Hourly_O3_for_UCC)

NO <- rbind(X2018_Hourly_NO_for_UCC, X2019_Hourly_NO_for_UCC, X2020_Hourly_NO_for_UCC)

PM2_5 <- rbind(X2018_Hourly_PM2_5_for_UCC,X2019_Hourly_PM2_5_for_UCC, X2020_Hourly_PM2_5_for_UCC)

PM10 <- rbind(X2018_Hourly_PM10_for_UCC,X2019_Hourly_PM10_for_UCC, X2020_Hourly_PM10_for_UCC)


############################################################
# Go to data type conversion file to plot missing data count
############################################################


# Melt data
library(reshape)
NO2_combined <- melt(NO2, id.vars=c('Date'),var='values')
O3_combined <- melt(O3, id.vars=c('Date'),var='values')
NO_combined <- melt(NO, id.vars=c('Date'),var='values')
PM2_5_combined <- melt(PM2_5, id.vars=c('Date'),var='values')
PM10_combined <- melt(PM10, id.vars=c('Date'),var='values')


# 1. Check missing values in independent features
# 2. Check the scale of independent features
summary(NO_combined) 
summary(O3_combined)
summary(PM2_5_combined)
summary(PM10_combined)
summary(NO2_combined) 


# 3. Check the distribution of independent features
hist(NO2_combined$value)
hist(O3_combined$value)
hist(NO_combined$value)
hist(PM2_5_combined$value)
hist(PM10_combined$value)


# 4. Correlation of independent features with the target variable
# cor(NO2_combined$value, NO_combined$value)
# hist(NO2_combined$value)
# hist(O3_combined$value)
# hist(PM2_5_combined$value)
# hist(PM10_combined$value)


################################################
# Find common stations from all air pollutants
################################################
NO2_cols <- colnames(NO2)
O3_cols <- colnames(O3)
NO_cols <- colnames(NO)
PM2_5_cols <- colnames(PM2_5)
PM10_cols <- colnames(PM10)

NO2_O3_cols <- intersect(NO2_cols,O3_cols)
NO2_NO_cols <- intersect(NO2_cols,NO_cols)
NO2_PM2_5_cols <- intersect(NO2_cols,PM2_5_cols)
NO2_PM10_cols <- intersect(NO2_cols,PM10_cols)

Reduce(intersect, list(NO2_O3_cols,NO2_NO_cols,NO2_PM2_5_cols,NO2_PM10_cols))
# "Date", "Limerick.Peoples.Park", "Rathmines", "Waterford"

# plot.ts(NO2$Waterford)


hist(NO2$Limerick.Peoples.Park)
hist(NO2$Rathmines)
hist(NO2$Waterford)

hist(log(NO2$Limerick.Peoples.Park))
hist(log(NO2$Rathmines))
hist(log(NO2$Waterford))

# cor(NO$Limerick.Peoples.Park, NO2$Limerick.Peoples.Park,use = "complete.obs")
# # 0.6842223
# cor(O3$Limerick.Peoples.Park, NO2$Limerick.Peoples.Park,use = "complete.obs")
# # -0.6622431
# cor(PM2_5$Limerick.Peoples.Park, NO2$Limerick.Peoples.Park,use = "complete.obs")
# # 0.5556335
# cor(PM10$Limerick.Peoples.Park, NO2$Limerick.Peoples.Park,use = "complete.obs")
# # 0.5556464



#######################################
# Combined 2018, 2019 and 2020 data 
# from UCC for TS analysis
#######################################

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
# 
# # Replace negative values with NA
# # a_NO2 <- a_NO2 %>% mutate_all(funs(replace(., .<0, NA)))
# a_NO2$UCC[a_NO2$UCC < 0] <- NA
# 
# 
# summary(a_NO2)
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
