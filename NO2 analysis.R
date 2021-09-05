library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

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
boxplot(NO2_clone[,c(-1)], las=2, main="Boxplot of NO2 before data preprocessing")

NO2_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(NO2_clone[,c(-1)]), aes(value)) + 
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  stat_boxplot(geom ='errorbar') +
  ggtitle("Boxplot of NO2 before data preprocessing")

summary(NO2_clone)

# summary(NO2_clone$Castlebar)
# Min. 1st Qu.  Median    Mean      3rd Qu.    Max.    NA's 
#  -0.800   1.300   2.400   3.746   4.500     96.400     180 
# summary(NO2_clone$Davitt.Road)
# Min. 1st Qu.  Median    Mean      3rd Qu.    Max.    NA's 
#  -20.70    3.80    7.70   10.05   13.70     83.20      40 

# Replace negative values with NA
NO2_clone <- NO2_clone %>% mutate_all(funs(replace(., .<0, NA)))

summary(NO2_clone)

## Date is showing 2 NA values. So we need to handle this situation
# NO2_clone <- na.omit(NO2_clone)
which(is.na(NO2_clone$Date))
# NO2_clone <- NO2_clone[-c(2138, 10874), ]
NO2_clone[2138,]
NO2_clone[10874,]

NO2_clone[2138,]$Date <- as.POSIXct("2019-03-31 01:00:00")
NO2_clone[10874,]$Date <- as.POSIXct("2020-03-29 01:00:00")

summary(NO2_clone)

# Missing data of NO2 before LOCF(Last Observation Carried Forward)
missing_data_NO2_clone <- data.frame("Station"=colnames(NO2_clone[,-1]), "value"=colSums(is.na(NO2_clone[,-1])))
md_plot_NO2_clone<-ggplot(data=missing_data_NO2_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO2_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO2 before LOCF(Last Observation Carried Forward)")

# Last obs. carried forward
NO2_clone <- na.locf(NO2_clone, na.rm = F) 

# Check NA values
missing_data_NO2_clone <- data.frame("Station"=colnames(NO2_clone[,-1]), "value"=colSums(is.na(NO2_clone[,-1])))
md_plot_NO2_clone<-ggplot(data=missing_data_NO2_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO2_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO2 after LOCF(Last Observation Carried Forward)")
# It is clearly seen that limerick peoples park, Navan, Pearse street, Tallaght, 
# and waterford has a lot of missing data

# Replace NA's with median
for(i in 2:ncol(NO2_clone)){
  NO2_clone[is.na(NO2_clone[,i]), i] <- format(round(median(NO2_clone[,i], na.rm = TRUE), 2), nsmall = 2)
  NO2_clone[,i] <- as.double(NO2_clone[,i])
}

summary(NO2_clone)

# Plot the graphs again
NO2_clone[,-1] %>% gather() %>% head()
ggplot(gather(NO2_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Histogram of NO2 after data preprocessing")

boxplot(NO2_clone[,c(-1)],las=2, main="Boxplot of NO2 after data preprocessing")

NO2_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(NO2_clone[,c(-1)]), aes(value)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Boxplot of NO2 after data preprocessing")

