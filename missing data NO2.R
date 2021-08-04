########### 2018 NO2

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
md_NO2_2018<-ggplot(data=missing_data_NO2_2018, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_NO2_2018 + 
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
X2019_Hourly_NO2_for_UCC$Date <- strptime(X2019_Hourly_NO2_for_UCC$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(X2019_Hourly_NO2_for_UCC)){
  X2019_Hourly_NO2_for_UCC[,i] <- as.double(X2019_Hourly_NO2_for_UCC[,i])
}

# Check NA values
missing_data_NO2_2019 <- data.frame("Station"=colnames(X2019_Hourly_NO2_for_UCC[,-1]), "value"=colSums(is.na(X2019_Hourly_NO2_for_UCC[,-1])))
md_NO2_2019<-ggplot(data=missing_data_NO2_2019, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_NO2_2019 + 
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
X2020_Hourly_NO2_for_UCC$Date <- strptime(X2020_Hourly_NO2_for_UCC$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(X2020_Hourly_NO2_for_UCC)){
  X2020_Hourly_NO2_for_UCC[,i] <- as.double(X2020_Hourly_NO2_for_UCC[,i])
}

# Check NA values
missing_data_NO2_2020 <- data.frame("Station"=colnames(X2020_Hourly_NO2_for_UCC[,-1]), "value"=colSums(is.na(X2020_Hourly_NO2_for_UCC[,-1])))
md_NO2_2020<-ggplot(data=missing_data_NO2_2020, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_NO2_2020 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO2 in 2020")

str(X2020_Hourly_NO2_for_UCC)


## Select columns with the missing data less than 25% ##

X2018_Hourly_NO2_for_UCC <- X2018_Hourly_NO2_for_UCC[,c(T,(nrow(X2018_Hourly_NO2_for_UCC)*0.25 > missing_data_NO2_2018$value))]
# reduced from 23 columns to  7 columns

# fill NA's with mean values
for(i in 2:ncol(X2018_Hourly_NO2_for_UCC)){
  X2018_Hourly_NO2_for_UCC[is.na(X2018_Hourly_NO2_for_UCC[,i]), i] <- format(round(mean(X2018_Hourly_NO2_for_UCC[,i], na.rm = TRUE), 2), nsmall = 2)
  X2018_Hourly_NO2_for_UCC[,i] <- as.double(X2018_Hourly_NO2_for_UCC[,i])
}

str(X2018_Hourly_NO2_for_UCC)


X2019_Hourly_NO2_for_UCC <- X2019_Hourly_NO2_for_UCC[,c(T,(nrow(X2019_Hourly_NO2_for_UCC)*0.25 > missing_data_NO2_2019$value))]
# reduced from 23 columns to  18 columns

# fill NA's with mean values
for(i in 2:ncol(X2019_Hourly_NO2_for_UCC)){
  X2019_Hourly_NO2_for_UCC[is.na(X2019_Hourly_NO2_for_UCC[,i]), i] <- format(round(mean(X2019_Hourly_NO2_for_UCC[,i], na.rm = TRUE), 2), nsmall = 2)
  X2019_Hourly_NO2_for_UCC[,i] <- as.double(X2019_Hourly_NO2_for_UCC[,i])
}

str(X2019_Hourly_NO2_for_UCC)

## Select columns with the missing data less than 25% ##
X2020_Hourly_NO2_for_UCC <- X2020_Hourly_NO2_for_UCC[,c(T,(nrow(X2020_Hourly_NO2_for_UCC)*0.25 > missing_data_NO2_2019$value))]
# reduced from 23 columns to  16 columns

# fill NA's with mean values
for(i in 2:ncol(X2020_Hourly_NO2_for_UCC)){
  X2020_Hourly_NO2_for_UCC[is.na(X2020_Hourly_NO2_for_UCC[,i]), i] <- format(round(mean(X2020_Hourly_NO2_for_UCC[,i], na.rm = TRUE), 2), nsmall = 2)
  X2020_Hourly_NO2_for_UCC[,i] <- as.double(X2020_Hourly_NO2_for_UCC[,i])
}
str(X2020_Hourly_NO2_for_UCC)

sum(is.na.data.frame(X2018_Hourly_NO2_for_UCC)) # 0
sum(is.na.data.frame(X2019_Hourly_NO2_for_UCC)) # 0
sum(is.na.data.frame(X2020_Hourly_NO2_for_UCC)) # 0

### Find where is the NA value
# which(is.na(X2020_Hourly_NO2_for_UCC), arr.ind=TRUE)
# OR
# library(tidyverse)
# X2020_Hourly_NO2_for_UCC %>%
#   rowid_to_column() %>%
#   filter(is.na(X2020_Hourly_NO2_for_UCC))
