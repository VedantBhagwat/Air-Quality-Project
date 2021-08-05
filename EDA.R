###############################################
#              Latest Hourly Data
###############################################


# NO 
library(readxl)
X2018_Hourly_NO_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2018 Hourly NO for UCC.xls", 
                                      skip = 2)
# View(X2018_Hourly_NO_for_UCC)
X2018_Hourly_NO_for_UCC_stats <- X2018_Hourly_NO_for_UCC[c(8764:8771),]
X2018_Hourly_NO_for_UCC <- X2018_Hourly_NO_for_UCC[-c(1,2,(8764:8771)),]
str(X2018_Hourly_NO_for_UCC)
summary(X2018_Hourly_NO_for_UCC)

X2019_Hourly_NO_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2019 Hourly NO for UCC.xls", 
                                      skip = 2)
# View(X2019_Hourly_NO_for_UCC)
X2019_Hourly_NO_for_UCC_stats <- X2019_Hourly_NO_for_UCC[c(8764:8771),]
X2019_Hourly_NO_for_UCC <- X2019_Hourly_NO_for_UCC[-c(1,2,(8764:8771)),]

X2020_Hourly_NO_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2020 updated NO for UCC.xls", 
                                       skip = 2)
# View(X2020_Hourly_NO_for_UCC)
X2020_Hourly_NO_for_UCC_stats <- X2020_Hourly_NO_for_UCC[c(4290:4297),]
X2020_Hourly_NO_for_UCC <- X2020_Hourly_NO_for_UCC[-c(1,2,(4290:4297)),]

# NO2 
X2018_Hourly_NO2_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2018 Hourly NO2 for UCC.xls", 
                                      skip = 2)
# View(X2018_Hourly_NO2_for_UCC)
X2018_Hourly_NO2_for_UCC_stats <- X2018_Hourly_NO2_for_UCC[c(8764:8771),]
X2018_Hourly_NO2_for_UCC <- X2018_Hourly_NO2_for_UCC[-c(1,2,(8764:8771)),]

X2019_Hourly_NO2_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2019 Hourly NO2 for UCC.xls", 
                                      skip = 2)
# View(X2019_Hourly_NO2_for_UCC)
X2019_Hourly_NO2_for_UCC_stats <- X2019_Hourly_NO2_for_UCC[c(8764:8771),]
X2019_Hourly_NO2_for_UCC <- X2019_Hourly_NO2_for_UCC[-c(1,2,(8764:8771)),]

X2020_Hourly_NO2_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2020 updated  NO2 for UCC.xls", 
                                       skip = 2)
# View(X2020_Hourly_NO2_for_UCC)
X2020_Hourly_NO2_for_UCC_stats <- X2020_Hourly_NO2_for_UCC[c(4290:4297),]
X2020_Hourly_NO2_for_UCC <- X2020_Hourly_NO2_for_UCC[-c(1,2,(4290:4297)),]

# PM 2.5
X2018_Hourly_PM2_5_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2018 Hourly PM2.5 for UCC.xls", 
                                         skip = 2)
# View(X2018_Hourly_PM2_5_for_UCC)
X2018_Hourly_PM2_5_for_UCC_stats <- X2018_Hourly_PM2_5_for_UCC[c(8764:8771),]
X2018_Hourly_PM2_5_for_UCC <- X2018_Hourly_PM2_5_for_UCC[-c(1,2,(8764:8771)),]

X2019_Hourly_PM2_5_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2019 Hourly PM2.5 for UCC.xls", 
                                         skip = 2)
# View(X2019_Hourly_PM2_5_for_UCC)
X2019_Hourly_PM2_5_for_UCC_stats <- X2019_Hourly_PM2_5_for_UCC[c(8764:8771),]
X2019_Hourly_PM2_5_for_UCC <- X2019_Hourly_PM2_5_for_UCC[-c(1,2,(8764:8771)),]

X2020_Hourly_PM2_5_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2020 updated PM25 for UCC.xls", 
                                         skip = 2)
# View(X2020_Hourly_PM2_5_for_UCC)
X2020_Hourly_PM2_5_for_UCC_stats <- X2020_Hourly_PM2_5_for_UCC[c(4290:4297),]
X2020_Hourly_PM2_5_for_UCC <- X2020_Hourly_PM2_5_for_UCC[-c(1,2,(4290:4297)),]


# PM 10
X2018_Hourly_PM10_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2018 Hourly PM10 for UCC.xls", 
                                        skip = 2)
# View(X2018_Hourly_PM10_for_UCC)
X2018_Hourly_PM10_for_UCC_stats <- X2018_Hourly_PM10_for_UCC[c(8764:8771),]
X2018_Hourly_PM10_for_UCC <- X2018_Hourly_PM10_for_UCC[-c(1,2,(8764:8771)),]

X2019_Hourly_PM10_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2019 Hourly PM10 for UCC.xls", 
                                        skip = 2)
# View(X2019_Hourly_PM10_for_UCC)
X2019_Hourly_PM10_for_UCC_stats <- X2019_Hourly_PM10_for_UCC[c(8764:8771),]
X2019_Hourly_PM10_for_UCC <- X2019_Hourly_PM10_for_UCC[-c(1,2,(8764:8771)),]


X2020_Hourly_PM10_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2020 updated PM10 for UCC.xls", 
                                        skip = 2)
# View(X2020_Hourly_PM10_for_UCC)
X2020_Hourly_PM10_for_UCC_stats <- X2020_Hourly_PM10_for_UCC[c(4290:4297),]
X2020_Hourly_PM10_for_UCC <- X2020_Hourly_PM10_for_UCC[-c(1,2,(4290:4297)),]

# Ozone
X2018_Hourly_O3_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2018 Hourly O3 for UCC.xls", 
                                      skip = 2)
# View(X2018_Hourly_O3_for_UCC)
X2018_Hourly_O3_for_UCC_stats <- X2018_Hourly_O3_for_UCC[c(8764:8771),]
X2018_Hourly_O3_for_UCC <- X2018_Hourly_O3_for_UCC[-c(1,2,(8764:8771)),]

X2019_Hourly_O3_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2019 Hourly O3 for UCC.xls", 
                                      skip = 2)
# View(X2019_Hourly_O3_for_UCC)
X2019_Hourly_O3_for_UCC_stats <- X2019_Hourly_O3_for_UCC[c(8764:8771),]
X2019_Hourly_O3_for_UCC <- X2019_Hourly_O3_for_UCC[-c(1,2,(8764:8771)),]

X2020_Hourly_O3_for_UCC <- read_excel("E:/CIT/Project/AQ data/AAMP/2020 updated Ozone for UCC.xls", 
                                      skip = 2)
# View(X2020_Hourly_O3_for_UCC)
X2020_Hourly_O3_for_UCC_stats <- X2020_Hourly_O3_for_UCC[c(4290:4297),]
X2020_Hourly_O3_for_UCC <- X2020_Hourly_O3_for_UCC[-c(1,2,(4290:4297)),]

