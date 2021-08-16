# NO2
typeof(NO2)
# Convert data into data frame
NO2 <- data.frame(NO2)


# Convert string date to Date object
colnames(NO2)[1] <- "Date"
NO2$Date <- strptime(NO2$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(NO2)){
  NO2[,i] <- as.double(NO2[,i])
}

# Check NA values
missing_data_NO2 <- data.frame("Station"=colnames(NO2[,-1]), "value"=colSums(is.na(NO2[,-1])))
md_plot_NO2<-ggplot(data=missing_data_NO2, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO2 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO2 in 2019 and 2020")

str(NO2)

# NO
typeof(NO)
# Convert data into data frame
NO <- data.frame(NO)


# Convert string date to Date object
colnames(NO)[1] <- "Date"
NO$Date <- strptime(NO$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(NO)){
  NO[,i] <- as.double(NO[,i])
}

# Check NA values
missing_data_NO <- data.frame("Station"=colnames(NO[,-1]), "value"=colSums(is.na(NO[,-1])))
md_plot_NO<-ggplot(data=missing_data_NO, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_NO + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of NO in 2019 and 2020")

str(NO)

# O3
typeof(O3)
# Convert data into data frame
O3 <- data.frame(O3)


# Convert string date to Date object
colnames(O3)[1] <- "Date"
O3$Date <- strptime(O3$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(O3)){
  O3[,i] <- as.double(O3[,i])
}

# Check NA values
missing_data_O3 <- data.frame("Station"=colnames(O3[,-1]), "value"=colSums(is.na(O3[,-1])))
md_plot_O3<-ggplot(data=missing_data_O3, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_O3 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of O3 in 2019 and 2020")

str(O3)


# PM2_5
typeof(PM2_5)
# Convert data into data frame
PM2_5 <- data.frame(PM2_5)


# Convert string date to Date object
colnames(PM2_5)[1] <- "Date"
PM2_5$Date <- strptime(PM2_5$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(PM2_5)){
  PM2_5[,i] <- as.double(PM2_5[,i])
}

# Check NA values
missing_data_PM2_5 <- data.frame("Station"=colnames(PM2_5[,-1]), "value"=colSums(is.na(PM2_5[,-1])))
md_plot_PM2_5<-ggplot(data=missing_data_PM2_5, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_PM2_5 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of PM2_5 in 2019 and 2020")

str(PM2_5)


# PM10
typeof(PM10)
# Convert data into data frame
PM10 <- data.frame(PM10)


# Convert string date to Date object
colnames(PM10)[1] <- "Date"
PM10$Date <- strptime(PM10$Date, format="%d/%m/%Y %H:%M")

# Convert character numbers into double 
for(i in 2:ncol(PM10)){
  PM10[,i] <- as.double(PM10[,i])
}

# Check NA values
missing_data_PM10 <- data.frame("Station"=colnames(PM10[,-1]), "value"=colSums(is.na(PM10[,-1])))
md_plot_PM10<-ggplot(data=missing_data_PM10, aes(x=Station, y=value)) +
  geom_bar(stat="identity", fill="steelblue")
md_plot_PM10 + 
  # geom_text(aes(label=value), vjust=1.6, color="white", size=3.5)+
  geom_text(aes(label=value), vjust=-0.3, size=3.5)+
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  ggtitle("Missing data of PM10 in 2019 and 2020")

str(PM10)

