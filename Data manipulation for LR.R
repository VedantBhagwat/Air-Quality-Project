########### All air pollutant analysis using linear regression #########

############# Check the correlation of all air pollutants
df_Limerick.Peoples.Park <- data.frame(NO2_clone$Date,NO2$Limerick.Peoples.Park,NO$Limerick.Peoples.Park,O3$Limerick.Peoples.Park,PM2_5$Limerick.Peoples.Park,PM10$Limerick.Peoples.Park)
df_Rathmines <- data.frame(NO2_clone$Date,NO2$Rathmines,NO$Rathmines,O3$Rathmines,PM2_5$Rathmines,PM10$Rathmines)
df_Waterford <- data.frame(NO2_clone$Date,NO2$Waterford,NO$Waterford,O3$Waterford,PM2_5$Waterford,PM10$Waterford)

cols <- c("Date","NO2","NO","O3","PM2.5","PM10")
colnames(df_Limerick.Peoples.Park) <- cols
colnames(df_Rathmines) <- cols
colnames(df_Waterford) <- cols

cor(df_Limerick.Peoples.Park[-1],use="complete.obs")
cor(df_Rathmines[-1],use="complete.obs")
cor(df_Waterford[-1],use="complete.obs")

# write.csv(df_Limerick.Peoples.Park,"E:\\CIT\\Project\\AQ data\\Export\\df_Limerick.Peoples.Park.csv", row.names = FALSE)
# write.csv(df_Rathmines,"E:\\CIT\\Project\\AQ data\\Export\\df_Rathmines.csv", row.names = FALSE)
# write.csv(df_Waterford,"E:\\CIT\\Project\\AQ data\\Export\\df_Waterford.csv", row.names = FALSE)

summary(df_Limerick.Peoples.Park)
summary(df_Rathmines)
summary(df_Waterford)

# which(is.na(df_Limerick.Peoples.Park$Date))
# df_Limerick.Peoples.Park[2138,]
# df_Limerick.Peoples.Park[10874,]
# which(is.na(df_Rathmines$Date))
# which(is.na(df_Waterford$Date))


# df_Limerick.Peoples.Park[2138,]$Date <- as.POSIXct("2019-03-30 15:00:00")
# df_Limerick.Peoples.Park[10874,]$Date <- as.POSIXct("2020-03-29 01:00:00")
# df_Rathmines[2138,]$Date <- as.POSIXct("2019-03-30 15:00:00")
# df_Rathmines[10874,]$Date <- as.POSIXct("2020-03-29 01:00:00")
# df_Waterford[2138,]$Date <- as.POSIXct("2019-03-30 15:00:00")
# df_Waterford[10874,]$Date <- as.POSIXct("2020-03-29 01:00:00")
# strptime(as.Date("2019-03-30 15:00:00"), format="%d/%m/%Y %H:%M")


# Replace negative values with NA
df_Limerick.Peoples.Park <- df_Limerick.Peoples.Park %>% mutate_all(funs(replace(., .<0, NA)))
df_Rathmines <- df_Rathmines %>% mutate_all(funs(replace(., .<0, NA)))
df_Waterford <- df_Waterford %>% mutate_all(funs(replace(., .<0, NA)))

# Last obs. carried forward
df_Limerick.Peoples.Park_locf <- na.locf(df_Limerick.Peoples.Park, na.rm = F)  
df_Rathmines_locf <- na.locf(df_Rathmines, na.rm = F)
df_Waterford_locf <- na.locf(df_Waterford, na.rm = F)

summary(df_Limerick.Peoples.Park_locf)
summary(df_Rathmines_locf)
summary(df_Waterford_locf)

# Replace NA with median
for(i in 2:ncol(df_Limerick.Peoples.Park_locf)){
  df_Limerick.Peoples.Park_locf[is.na(df_Limerick.Peoples.Park_locf[,i]), i] <- format(round(median(df_Limerick.Peoples.Park_locf[,i], na.rm = TRUE), 2), nsmall = 2)
  df_Limerick.Peoples.Park_locf[,i] <- as.double(df_Limerick.Peoples.Park_locf[,i])
}

for(i in 2:ncol(df_Rathmines_locf)){
  df_Rathmines_locf[is.na(df_Rathmines_locf[,i]), i] <- format(round(median(df_Rathmines_locf[,i], na.rm = TRUE), 2), nsmall = 2)
  df_Rathmines_locf[,i] <- as.double(df_Rathmines_locf[,i])
}

for(i in 2:ncol(df_Waterford_locf)){
  df_Waterford_locf[is.na(df_Waterford_locf[,i]), i] <- format(round(median(df_Waterford_locf[,i], na.rm = TRUE), 2), nsmall = 2)
  df_Waterford_locf[,i] <- as.double(df_Waterford_locf[,i])
}

summary(df_Limerick.Peoples.Park_locf)
summary(df_Rathmines_locf)
summary(df_Waterford_locf)

write.csv(df_Limerick.Peoples.Park_locf,"E:\\CIT\\Project\\AQ data\\Export\\df_Limerick.Peoples.Park_locf.csv", row.names = FALSE)
write.csv(df_Rathmines_locf,"E:\\CIT\\Project\\AQ data\\Export\\df_Rathmines_locf.csv", row.names = FALSE)
write.csv(df_Waterford_locf,"E:\\CIT\\Project\\AQ data\\Export\\df_Waterford_locf.csv", row.names = FALSE)



