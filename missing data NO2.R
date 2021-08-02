########### 2018 NO2

str(X2018_Hourly_NO2_for_UCC)

typeof(X2018_Hourly_NO2_for_UCC)
# Convert data into data frame
X2018_Hourly_NO2_for_UCC <- data.frame(X2018_Hourly_NO2_for_UCC)


# Convert string date to Date object
colnames(X2018_Hourly_NO2_for_UCC)[1] <- "Date"
X2018_Hourly_NO2_for_UCC$Date <- as.Date(X2018_Hourly_NO2_for_UCC$Date)

# Convert character numbers into double and replace NA values with mean
for(i in 2:ncol(X2018_Hourly_NO2_for_UCC)){
  X2018_Hourly_NO2_for_UCC[,i] <- as.double(X2018_Hourly_NO2_for_UCC[,i])
  # X2018_Hourly_NO2_for_UCC[is.na(X2018_Hourly_NO2_for_UCC[,i]), i] <- mean(X2018_Hourly_NO2_for_UCC[,i], na.rm = TRUE)
}

# X2018_Hourly_NO2_for_UCC$Castlebar <- as.double(X2018_Hourly_NO2_for_UCC$Castlebar)

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
X2019_Hourly_NO2_for_UCC$Date <- as.Date(X2019_Hourly_NO2_for_UCC$Date)

# Convert character numbers into double and replace NA values with mean
for(i in 2:ncol(X2019_Hourly_NO2_for_UCC)){
  X2019_Hourly_NO2_for_UCC[,i] <- as.double(X2019_Hourly_NO2_for_UCC[,i])
  # X2019_Hourly_NO2_for_UCC[is.na(X2019_Hourly_NO2_for_UCC[,i]), i] <- mean(X2019_Hourly_NO2_for_UCC[,i], na.rm = TRUE)
}

# X2019_Hourly_NO2_for_UCC$Castlebar <- as.double(X2019_Hourly_NO2_for_UCC$Castlebar)

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
X2020_Hourly_NO2_for_UCC$Date <- as.Date(X2020_Hourly_NO2_for_UCC$Date)

# Convert character numbers into double and replace NA values with mean
for(i in 2:ncol(X2020_Hourly_NO2_for_UCC)){
  X2020_Hourly_NO2_for_UCC[,i] <- as.double(X2020_Hourly_NO2_for_UCC[,i])
  # X2020_Hourly_NO2_for_UCC[is.na(X2020_Hourly_NO2_for_UCC[,i]), i] <- mean(X2020_Hourly_NO2_for_UCC[,i], na.rm = TRUE)
}

# X2020_Hourly_NO2_for_UCC$Castlebar <- as.double(X2020_Hourly_NO2_for_UCC$Castlebar)

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

