library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)

NO2_clone <- NO2

str(NO2_clone)
summary(NO2_clone)


####### Plot Histogram

# gg <- melt(NO2_clone[,c(-1,-5)])
# ggplot(gg, aes(x=value, fill=variable)) +
#   geom_histogram(binwidth=10)+
#   facet_grid(variable~.)

NO2_clone[,-1] %>% gather() %>% head()
ggplot(gather(NO2_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+
  ggtitle("Histogram of NO2 before data preprocessing")

# Remove cobh column from data frame because of missing data
NO2_clone <- NO2_clone[,-5]

# Boxplots
boxplot(NO2_clone[,c(-1)])

NO2_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(NO2_clone[,c(-1)]), aes(value)) + 
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  stat_boxplot(geom ='errorbar') +
  ggtitle("Boxplot of NO2 before data preprocessing")

summary(NO2_clone)

summary(NO2_clone$Castlebar)
# Min. 1st Qu.  Median    Mean      3rd Qu.    Max.    NA's 
#  -0.800   1.300   2.400   3.746   4.500     96.400     180 
summary(NO2_clone$Davitt.Road)
# Min. 1st Qu.  Median    Mean      3rd Qu.    Max.    NA's 
#  -20.70    3.80    7.70   10.05   13.70     83.20      40 

# Replace negative values with NA
# NO2_clone$Castlebar <- replace(NO2_clone$Castlebar, which(NO2_clone$Castlebar < 0), NA)
# NO2_clone %>% mutate(Cobh = replace(Cobh, which(Cobh< -21), NA))
NO2_clone <- NO2_clone %>% mutate_all(funs(replace(., .<0, NA)))

summary(NO2_clone)

# Replace NA with median
for(i in 2:ncol(NO2_clone)){
  NO2_clone[is.na(NO2_clone[,i]), i] <- format(round(median(NO2_clone[,i], na.rm = TRUE), 2), nsmall = 2)
  NO2_clone[,i] <- as.double(NO2_clone[,i])
}

summary(NO2_clone)
# NO2_clone <- na.omit(NO2_clone)
which(is.na(NO2_clone$Date))
NO2_clone <- NO2_clone[-c(2138, 10874), ]


# Plot the graphs again
NO2_clone[,-1] %>% gather() %>% head()
ggplot(gather(NO2_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Histogram of NO2 after data preprocessing")

boxplot(NO2_clone[,c(-1)], main="Boxplot of NO2 after data preprocessing")

NO2_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(NO2_clone[,c(-1)]), aes(value)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Boxplot of NO2 after data preprocessing")





############# Check the correlation of all air pollutants
df_Limerick.Peoples.Park <- data.frame(NO2$Limerick.Peoples.Park,NO$Limerick.Peoples.Park,O3$Limerick.Peoples.Park,PM2_5$Limerick.Peoples.Park,PM10$Limerick.Peoples.Park)
df_Rathmines <- data.frame(NO2$Rathmines,NO$Rathmines,O3$Rathmines,PM2_5$Rathmines,PM10$Rathmines)
df_Waterford <- data.frame(NO2$Waterford,NO$Waterford,O3$Waterford,PM2_5$Waterford,PM10$Waterford)

cols <- c("NO2","NO","O3","PM2.5","PM10")
colnames(df_Limerick.Peoples.Park) <- cols
colnames(df_Rathmines) <- cols
colnames(df_Waterford) <- cols

cor(df_Limerick.Peoples.Park,use="complete.obs")
cor(df_Rathmines,use="complete.obs")
cor(df_Waterford,use="complete.obs")

# write.csv(df_Limerick.Peoples.Park,"E:\\CIT\\Project\\AQ data\\Export\\df_Limerick.Peoples.Park.csv", row.names = FALSE)
# write.csv(df_Rathmines,"E:\\CIT\\Project\\AQ data\\Export\\df_Rathmines.csv", row.names = FALSE)
# write.csv(df_Waterford,"E:\\CIT\\Project\\AQ data\\Export\\df_Waterford.csv", row.names = FALSE)

summary(df_Limerick.Peoples.Park)
summary(df_Rathmines)
summary(df_Waterford)
