library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

PM10_clone <- PM10

str(PM10_clone)
summary(PM10_clone)


####### Plot Histogram

# gg <- melt(PM10_clone[,c(-1,-5)])
# ggplot(gg, aes(x=value, fill=variable)) +
#   geom_histogram(binwidth=10)+
#   facet_grid(variable~.)

PM10_clone[,-1] %>% gather() %>% head()
ggplot(gather(PM10_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+
  ggtitle("Histogram of PM10 before data preprocessing")

# Remove cobh column from data frame because of missing data
# PM10_clone <- PM10_clone[,-5]

# Boxplots
# boxplot(PM10_clone[,c(-1)], las=2, main="Boxplot of PM10 before data preprocessing")

PM10_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(PM10_clone[,c(-1)]), aes(value)) + 
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  stat_boxplot(geom ='errorbar') +
  ggtitle("Boxplot of PM10 before data preprocessing")

summary(PM10_clone)

# Replace negative values with NA
PM10_clone <- PM10_clone %>% mutate_all(funs(replace(., .<0, NA)))

summary(PM10_clone)

## Date is showing 2 NA values. So we need to handle this situation
# PM10_clone <- na.omit(PM10_clone)
which(is.na(PM10_clone$Date))
# PM10_clone <- PM10_clone[-c(2138, 10874), ]
PM10_clone[2138,]
PM10_clone[10874,]

PM10_clone[2138,]$Date <- as.POSIXct("2019-03-31 01:00:00")
PM10_clone[10874,]$Date <- as.POSIXct("2020-03-29 01:00:00")

summary(PM10_clone)

# Missing data of PM10 before LOCF(Last Observation Carried Forward)
missing_data_PM10_clone <- data.frame("Station"=colnames(PM10_clone[,-1]), "value"=colSums(is.na(PM10_clone[,-1])))
md_plot_PM10_clone<-ggplot(data=missing_data_PM10_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_PM10_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of PM10 before LOCF(Last Observation Carried Forward)")

# Last obs. carried forward
PM10_clone <- na.locf(PM10_clone, na.rm = F) 

# Check NA values
missing_data_PM10_clone <- data.frame("Station"=colnames(PM10_clone[,-1]), "value"=colSums(is.na(PM10_clone[,-1])))
md_plot_PM10_clone<-ggplot(data=missing_data_PM10_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_PM10_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of PM10 after LOCF(Last Observation Carried Forward)")
# It is clearly seen that limerick peoples park, Navan, Pearse street, Tallaght, 
# and waterford has a lot of missing data

# Replace NA's with median
for(i in 2:ncol(PM10_clone)){
  PM10_clone[is.na(PM10_clone[,i]), i] <- format(round(median(PM10_clone[,i], na.rm = TRUE), 2), nsmall = 2)
  PM10_clone[,i] <- as.double(PM10_clone[,i])
}

summary(PM10_clone)

# Plot the graphs again
PM10_clone[,-1] %>% gather() %>% head()
ggplot(gather(PM10_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Histogram of PM10 after data preprocessing")

# boxplot(PM10_clone[,c(-1)],las=2, main="Boxplot of PM10 after data preprocessing")

PM10_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(PM10_clone[,c(-1)]), aes(value)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Boxplot of PM10 after data preprocessing")

