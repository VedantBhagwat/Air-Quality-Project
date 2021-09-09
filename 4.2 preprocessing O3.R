library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

O3_clone <- O3

which(duplicated(O3_clone$Date))
O3_clone$Date[duplicated(O3_clone$Date)]

# Remove duplicate row from dataframe
O3_clone <- O3_clone[-8762, ]

str(O3_clone)
summary(O3_clone)


####### Plot Histogram

# gg <- melt(O3_clone[,c(-1,-5)])
# ggplot(gg, aes(x=value, fill=variable)) +
#   geom_histogram(binwidth=10)+
#   facet_grid(variable~.)

O3_clone[,-1] %>% gather() %>% head()
ggplot(gather(O3_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')+
  ggtitle("Histogram of O3 before data preprocessing")

# Remove cobh column from data frame because of missing data
# O3_clone <- O3_clone[,-5]

# Boxplots
# boxplot(O3_clone[,c(-1)], las=2, main="Boxplot of O3 before data preprocessing")

O3_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(O3_clone[,c(-1)]), aes(value)) + 
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  stat_boxplot(geom ='errorbar') +
  ggtitle("Boxplot of O3 before data preprocessing")

summary(O3_clone)

# Replace negative values with NA
O3_clone <- O3_clone %>% mutate_all(funs(replace(., .<0, NA)))

summary(O3_clone)

# ## Date is showing 2 NA values. So we need to handle this situation
# # O3_clone <- na.omit(O3_clone)
# which(is.na(O3_clone$Date))
# # O3_clone <- O3_clone[-c(2138, 10874), ]
# O3_clone[2138,]
# O3_clone[10874,]
# 
# O3_clone[2138,]$Date <- as.POSIXct("2019-03-31 01:00:00")
# O3_clone[10874,]$Date <- as.POSIXct("2020-03-29 01:00:00")

summary(O3_clone)

# Missing data of O3 before LOCF(Last Observation Carried Forward)
missing_data_O3_clone <- data.frame("Station"=colnames(O3_clone[,-1]), "value"=colSums(is.na(O3_clone[,-1])))
md_plot_O3_clone<-ggplot(data=missing_data_O3_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_O3_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of O3 before LOCF(Last Observation Carried Forward)")

# Last obs. carried forward
O3_clone[,-1] <- na.locf(O3_clone[,-1], na.rm = F) 

# Check NA values
missing_data_O3_clone <- data.frame("Station"=colnames(O3_clone[,-1]), "value"=colSums(is.na(O3_clone[,-1])))
md_plot_O3_clone<-ggplot(data=missing_data_O3_clone, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_O3_clone + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of O3 after LOCF(Last Observation Carried Forward)")
# It is clearly seen that limerick peoples park, Navan, Pearse street, Tallaght, 
# and waterford has a lot of missing data

# Replace NA's with median
for(i in 2:ncol(O3_clone)){
  O3_clone[is.na(O3_clone[,i]), i] <- format(round(median(O3_clone[,i], na.rm = TRUE), 2), nsmall = 2)
  O3_clone[,i] <- as.double(O3_clone[,i])
}

summary(O3_clone)

# Plot the graphs again
O3_clone[,-1] %>% gather() %>% head()
ggplot(gather(O3_clone[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Histogram of O3 after data preprocessing")

# boxplot(O3_clone[,c(-1)],las=2, main="Boxplot of O3 after data preprocessing")

O3_clone[,c(-1)] %>% gather() %>% head()
ggplot(gather(O3_clone[,c(-1)]), aes(value)) + 
  stat_boxplot(geom ='errorbar')+
  geom_boxplot(bins = 10) + 
  facet_wrap(~key, scales = 'free_x') +
  ggtitle("Boxplot of O3 after data preprocessing")

