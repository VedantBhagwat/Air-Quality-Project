Date19 <- seq(as.POSIXct("2019-01-01 00:00:00"), as.POSIXct("2019-12-31 24:00:00"), by="hour")
Date20 <- seq(as.POSIXct("2020-01-01 00:00:00"), as.POSIXct("2020-06-27 15:00:00"), by="hour")
Date <- seq(as.POSIXct("2018-12-31 24:00:00"), as.POSIXct("2020-06-27 15:00:00"), by="hour")


########### 2018 NO2
library(ggplot2)

str(X2018_Hourly_NO2_for_UCC)

typeof(X2018_Hourly_NO2_for_UCC)
# Convert data into data frame
X2018_Hourly_NO2_for_UCC <- data.frame(X2018_Hourly_NO2_for_UCC)


# Convert string date to Date object
colnames(X2018_Hourly_NO2_for_UCC)[1] <- "Date"
X2018_Hourly_NO2_for_UCC$Date <- strptime(X2018_Hourly_NO2_for_UCC$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(X2018_Hourly_NO2_for_UCC)){
  X2018_Hourly_NO2_for_UCC[,i] <- as.double(X2018_Hourly_NO2_for_UCC[,i])
}

# Check NA values
missing_data_NO2_2018 <- data.frame("Station"=colnames(X2018_Hourly_NO2_for_UCC[,-1]), "value"=colSums(is.na(X2018_Hourly_NO2_for_UCC[,-1])))
md_plot_NO2_2018<-ggplot(data=missing_data_NO2_2018, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO2_2018 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO2 in 2018")

str(X2018_Hourly_NO2_for_UCC)



########### 2019 NO2

str(X2019_Hourly_NO2_for_UCC)

typeof(X2019_Hourly_NO2_for_UCC)
# Convert data into data frame
X2019_Hourly_NO2_for_UCC <- data.frame(X2019_Hourly_NO2_for_UCC)


# Convert string date to Date object
colnames(X2019_Hourly_NO2_for_UCC)[1] <- "Date"
X2019_Hourly_NO2_for_UCC$Date <- Date19
# strptime(X2019_Hourly_NO2_for_UCC$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(X2019_Hourly_NO2_for_UCC)){
  X2019_Hourly_NO2_for_UCC[,i] <- as.double(X2019_Hourly_NO2_for_UCC[,i])
}

# Check NA values
missing_data_NO2_2019 <- data.frame("Station"=colnames(X2019_Hourly_NO2_for_UCC[,-1]), "value"=colSums(is.na(X2019_Hourly_NO2_for_UCC[,-1])))
md_plot_NO2_2019<-ggplot(data=missing_data_NO2_2019, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO2_2019 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO2 in 2019")

str(X2019_Hourly_NO2_for_UCC)


########### 2020 NO2

str(X2020_Hourly_NO2_for_UCC)

typeof(X2020_Hourly_NO2_for_UCC)
# Convert data into data frame
X2020_Hourly_NO2_for_UCC <- data.frame(X2020_Hourly_NO2_for_UCC)


# Convert string date to Date object
colnames(X2020_Hourly_NO2_for_UCC)[1] <- "Date"
X2020_Hourly_NO2_for_UCC$Date <- Date20
# strptime(X2020_Hourly_NO2_for_UCC$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(X2020_Hourly_NO2_for_UCC)){
  X2020_Hourly_NO2_for_UCC[,i] <- as.double(X2020_Hourly_NO2_for_UCC[,i])
}

# Check NA values
missing_data_NO2_2020 <- data.frame("Station"=colnames(X2020_Hourly_NO2_for_UCC[,-1]), "value"=colSums(is.na(X2020_Hourly_NO2_for_UCC[,-1])))
md_plot_NO2_2020<-ggplot(data=missing_data_NO2_2020, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO2_2020 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO2 in 2020")

str(X2020_Hourly_NO2_for_UCC)

#########################################################

# ## Select columns with the missing data less than 25% ##
# X2018_Hourly_NO2_for_UCC <- X2018_Hourly_NO2_for_UCC[,c(T,(nrow(X2018_Hourly_NO2_for_UCC)*0.25 > missing_data_NO2_2018$value))]
# # reduced from 24 columns to  7 columns
# 
# X2019_Hourly_NO2_for_UCC <- X2019_Hourly_NO2_for_UCC[,c(T,(nrow(X2019_Hourly_NO2_for_UCC)*0.25 > missing_data_NO2_2019$value))]
# # reduced from 24 columns to  18 columns
# 
# X2020_Hourly_NO2_for_UCC <- X2020_Hourly_NO2_for_UCC[,c(T,(nrow(X2020_Hourly_NO2_for_UCC)*0.25 > missing_data_NO2_2020$value))]
# # reduced from 24 columns to  21 columns


########### Histogram of all stations ################

# library(Hmisc)
# dev.new()
# hist.data.frame(X2019_Hourly_NO_for_UCC[,-1])

library(tidyr)
X2018_Hourly_NO2_for_UCC[,-1] %>% gather() %>% head()
ggplot(gather(X2018_Hourly_NO2_for_UCC[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

X2019_Hourly_NO2_for_UCC[,-1] %>% gather() %>% head()
ggplot(gather(X2019_Hourly_NO2_for_UCC[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

X2020_Hourly_NO2_for_UCC[,-1] %>% gather() %>% head()
ggplot(gather(X2020_Hourly_NO2_for_UCC[,-1]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')


########### Boxplots ###########
boxplot(X2018_Hourly_NO2_for_UCC[,-1])
boxplot(X2019_Hourly_NO2_for_UCC[,-1])
# Some negative values present in 2018 and 2019
boxplot(X2020_Hourly_NO2_for_UCC[,-1])

test_2018_Hourly_NO2_for_UCC <- X2018_Hourly_NO2_for_UCC

X2018_Hourly_NO2_for_UCC[X2018_Hourly_NO2_for_UCC$Dundalk>150,]




test_2018_Hourly_NO2_for_UCC[5748,5] <- NA
test_2018_Hourly_NO2_for_UCC[2411,2] <- NA

summary(test_2018_Hourly_NO2_for_UCC)

boxplot(test_2018_Hourly_NO2_for_UCC[,-1])

test_2018_Hourly_NO2_for_UCC[5748,]
# Check number of NA's
sum(is.na.data.frame(X2018_Hourly_NO2_for_UCC)) # 2808
sum(is.na.data.frame(X2019_Hourly_NO2_for_UCC)) # 6162
sum(is.na.data.frame(X2020_Hourly_NO2_for_UCC)) # 2881

# fill NA's with mean values
for(i in 2:ncol(X2018_Hourly_NO2_for_UCC)){
  X2018_Hourly_NO2_for_UCC[is.na(X2018_Hourly_NO2_for_UCC[,i]), i] <- format(round(mean(X2018_Hourly_NO2_for_UCC[,i], na.rm = TRUE), 2), nsmall = 2)
  X2018_Hourly_NO2_for_UCC[,i] <- as.double(X2018_Hourly_NO2_for_UCC[,i])
}
str(X2018_Hourly_NO2_for_UCC)

for(i in 2:ncol(X2019_Hourly_NO2_for_UCC)){
  X2019_Hourly_NO2_for_UCC[is.na(X2019_Hourly_NO2_for_UCC[,i]), i] <- format(round(mean(X2019_Hourly_NO2_for_UCC[,i], na.rm = TRUE), 2), nsmall = 2)
  X2019_Hourly_NO2_for_UCC[,i] <- as.double(X2019_Hourly_NO2_for_UCC[,i])
}
str(X2019_Hourly_NO2_for_UCC)

for(i in 2:ncol(X2020_Hourly_NO2_for_UCC)){
  X2020_Hourly_NO2_for_UCC[is.na(X2020_Hourly_NO2_for_UCC[,i]), i] <- format(round(mean(X2020_Hourly_NO2_for_UCC[,i], na.rm = TRUE), 2), nsmall = 2)
  X2020_Hourly_NO2_for_UCC[,i] <- as.double(X2020_Hourly_NO2_for_UCC[,i])
}
str(X2020_Hourly_NO2_for_UCC)






### Find where is the NA value
# which(is.na(X2020_Hourly_NO2_for_UCC), arr.ind=TRUE)
# OR
# library(tidyverse)
# X2020_Hourly_NO2_for_UCC %>%
#   rowid_to_column() %>%
#   filter(is.na(X2020_Hourly_NO2_for_UCC))

