library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

NO_clone <- NO

which(duplicated(NO_clone$Date))
NO_clone$Date[duplicated(NO_clone$Date)]

# Remove duplicate row from dataframe
NO_clone <- NO_clone[-8762, ]

str(NO_clone)
summary(NO_clone)


####### Plot Histogram

# gg <- melt(NO_clone[,c(-1,-5)])
# ggplot(gg, aes(x=value, fill=variable)) +
#   geom_histogram(binwidth=10)+
#   facet_grid(variable~.)

NO_clone[,-1] %>% gather() %>% head()
ggplot(gather(NO_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+
  ggtitle("Histogram of NO before data preprocessing")

# Remove cobh column from data frame because of missing data
NO_clone <- NO_clone[,-5]

# Boxplots
# boxplot(NO_clone[,c(-1)], las=2, main="Boxplot of NO before data preprocessing")

NO_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(NO_clone[,c(-1)]), aes(value)) + 
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  stat_boxplot(geom ='errorbar') +
  ggtitle("Boxplot of NO before data preprocessing")

summary(NO_clone)

# Replace negative values with NA
NO_clone <- NO_clone %>% mutate_all(funs(replace(., .<0, NA)))

summary(NO_clone)

# ## Date is showing 2 NA values. So we need to handle this situation
# # NO_clone <- na.omit(NO_clone)
# which(is.na(NO_clone$Date))
# # NO_clone <- NO_clone[-c(2138, 10874), ]
# NO_clone[2138,]
# NO_clone[10874,]
# 
# NO_clone[2138,]$Date <- as.POSIXct("2019-03-31 01:00:00")
# NO_clone[10874,]$Date <- as.POSIXct("2020-03-29 01:00:00")
# 
# summary(NO_clone)

# Missing data of NO before LOCF(Last Observation Carried Forward)
missing_data_NO_clone <- data.frame("Station"=colnames(NO_clone[,-1]), "value"=colSums(is.na(NO_clone[,-1])))
md_plot_NO_clone<-ggplot(data=missing_data_NO_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO before LOCF(Last Observation Carried Forward)")

# Last obs. carried forward
NO_clone <- na.locf(NO_clone, na.rm = F) 

summary(NO_clone)

# Check NA values
missing_data_NO_clone <- data.frame("Station"=colnames(NO_clone[,-1]), "value"=colSums(is.na(NO_clone[,-1])))
md_plot_NO_clone<-ggplot(data=missing_data_NO_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO after LOCF(Last Observation Carried Forward)")
# It is clearly seen that limerick peoples park, Navan, Pearse street, Tallaght, 
# and waterford has a lot of missing data

# Replace NA's with median
for(i in 2:ncol(NO_clone)){
  NO_clone[is.na(NO_clone[,i]), i] <- format(round(median(NO_clone[,i], na.rm = TRUE), 2), nsmall = 2)
  NO_clone[,i] <- as.double(NO_clone[,i])
}

summary(NO_clone)

# Plot the graphs again
NO_clone[,-1] %>% gather() %>% head()
ggplot(gather(NO_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Histogram of NO after data preprocessing")

# boxplot(NO_clone[,c(-1)],las=2, main="Boxplot of NO after data preprocessing")

NO_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(NO_clone[,c(-1)]), aes(value)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Boxplot of NO after data preprocessing")

