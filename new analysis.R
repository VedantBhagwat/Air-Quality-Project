nrow(X2019_Hourly_NO2_for_UCC) + nrow(X2020_Hourly_NO2_for_UCC)

# Combine 2019 and 2020 data
NO2 <- rbind(X2019_Hourly_NO2_for_UCC, X2020_Hourly_NO2_for_UCC)

NO <- rbind(X2019_Hourly_NO_for_UCC, X2020_Hourly_NO_for_UCC)

O3 <- rbind(X2019_Hourly_O3_for_UCC, X2020_Hourly_O3_for_UCC)

PM2_5 <- rbind(X2019_Hourly_PM2_5_for_UCC, X2020_Hourly_PM2_5_for_UCC)

PM10 <- rbind(X2019_Hourly_PM10_for_UCC, X2020_Hourly_PM10_for_UCC)

###### Go to data type conversion file ######

# Change the data types of data frame and plot missing data
# 
# typeof(PM10)
# # Convert data into data frame
# PM10 <- data.frame(PM10)
# 
# 
# # Convert string date to Date object
# colnames(PM10)[1] <- "Date"
# PM10$Date <- strptime(PM10$Date, format="%d/%m/%Y %H:%M")
# 
# # Convert character numbers into double 
# for(i in 2:ncol(PM10)){
#   PM10[,i] <- as.double(PM10[,i])
# }
# 
# # Check NA values
# missing_data_PM10 <- data.frame("Station"=colnames(PM10[,-1]), "value"=colSums(is.na(PM10[,-1])))
# md_plot_PM10<-ggplot(data=missing_data_PM10, aes(x=Station, y=value)) +
#   geom_bar(stat="identity", fill="steelblue")
# md_plot_PM10 + 
#   # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
#   geom_text(aes(label=value), vjust=-0.3, size=3.5)+
#   theme_minimal() +
#   theme(axis.text.x=element_text(angle = 45, hjust = 1))+
#   ggtitle("Missing data of PM10 in 2019 and 2020")
# 
# str(PM10)


# Melt data
library(reshape)
NO2_combined <- melt(NO2, id.vars=c('Date'),var='values')
NO_combined <- melt(NO, id.vars=c('Date'),var='values')
O3_combined <- melt(O3, id.vars=c('Date'),var='values')
PM2_5_combined <- melt(PM2_5, id.vars=c('Date'),var='values')
PM10_combined <- melt(PM10, id.vars=c('Date'),var='values')

# hist(a$value)
# summary(a)
# IQR(a$value, na.rm=T)

# 1. Check missing values in independent features
# 2. Check the scale of independent features
summary(NO_combined) 
# NA's   :57089
# Min.   :-9999.00
# Max.   :  650.40
summary(O3_combined)
# NA's   :28117
# Min.   :-648.30
# Max.   :  97.20 
summary(PM2_5_combined)
# NA's   :98892 
# Min.   :-79.30
# Max.   :577.80
summary(PM10_combined)
# NA's   :103773
# Min.   :-69.00 
# Max.   :985.00 
summary(NO2_combined) 
# 28117
# Min.   :-648.30
# Max.   :  97.20


# 3. Check the distribution of independent features
hist(NO_combined$value)
hist(O3_combined$value)
hist(PM2_5_combined$value)
hist(PM10_combined$value)
hist(NO2_combined$value)


# 4. Correlation of independent features with the target variable
cor(NO2_combined$value, NO_combined$value)
hist(O3_combined$value)
hist(PM2_5_combined$value)
hist(PM10_combined$value)
hist(NO2_combined$value)












# Find common stations from all air pollutants
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

plot.ts(NO2$Waterford)


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


