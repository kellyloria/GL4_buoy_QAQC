## ---------------------------
## QA'QC for for 3 PME Dissolved Oxygen sensors
##    ongoing space for aggregation of new deployment data
##
## Author: Kelly A. Loria
## Date Created: 2019-03-02 & updated 2020-03-18
## Email: kelly.loria@colorado.edu
##
## ---------------------------
## Load packages:
library(ggplot2)
library(dplyr)

## ---------------------------
# I. Read in past year's data - here 2018 summer
old.datDO <- read.csv("Summer2018_PME_DO.csv", header=T)

#   Fix timestamp - so it is no longer a character:
tmpDateFormat<-"%Y-%m-%d %H:%M:%OS"
tmp1stmp <- as.POSIXct(old.datDO$timestamp,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1stmp[!is.na(tmp1stmp)])){old.datDO$timestamp <- tmp1stmp } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1stmp) 
summary(old.datDO) #check to make sure timestamp looks okay

#   If it doesn't work use:
old.datDO$timestamp <- as.POSIXct(old.datDO$timestamp, format="%Y-%m-%d %H:%M:%OS")
range(old.datDO$timestamp)

## ---------------------------
## I. Winter 2018 Deployment: 3m DO sensor 

#     1. Read in new raw data at depth (for 2018-2019): DO_214423_180823_190723_3m.TXT
do.3m <- read.delim("DO_214423_180823_190723_3m.TXT", header=T, sep = ',')
names(do.3m)
summary(do.3m)

#   2. fix timestamp
do.3m$timestamp <- as.POSIXct(do.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.3m$timestamp)

#   3. restrict for date range
do.3m <- subset(do.3m,timestamp >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-23 00:00:00'))
range(do.3m$timestamp)

#   4. check new summary
summary(do.3m)
#   temp range: -0.003 to 10.353
#   Dissolved.Oxygen range: 5.688 to 12.090
#   Dissolved.Oxygen.Saturation range: 60.65 to 130.99
#   Battery: 2.920 to 3.500
#   Q > 0.7

#   5. add in column for depth, deployment and sensor 
do.3m$depth <- 3
do.3m$deployment <- "Winter2018"
do.3m$sensor <- 214423
do.3m$flag_Temp <- "n"
do.3m$flag_DO <- "n"

#   Data check from plots
qplot(timestamp, Temperature, data = do.3m, geom="point", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.3m, geom="point", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
## II. Winter 2018 Deployment: 7m DO sensor 

#   1. Read in new raw data at depth (for 2018-2019): DO_245673_180823_190723_7m.TXT
do.7m <- read.delim("DO_245673_180823_190723_7m.TXT", header=T, sep = ',')

#   2. Fix timestamp
do.7m$timestamp <- as.POSIXct(do.7m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.7m$timestamp)

#   3. Restrict for date range
do.7m <- subset(do.7m,timestamp >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-23 00:00:00'))
range(do.7m$timestamp)

#   4. check new summary
summary(do.7m) 

#   5. add in column for depth, deployment and sensor 
do.7m$depth <- 7
do.7m$deployment <- "Winter2018"
do.7m$sensor <- 245673
do.7m$flag_Temp <- "n"
do.7m$flag_DO <- "n"

# Data check from plots
qplot(timestamp, Temperature, data = do.7m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.7m, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
## III. Winter 2018 Deployment: 11m DO sensor 

#   1. Read in new raw data at depth (for 2018-2019): DO_245673_180823_190723_7m.TXT
do.11m <- read.delim("DO_248353_180823_190723_11m.TXT", header=T, sep = ',')
names(do.11m)
summary(do.11m)

#   2. Fix timestamp
do.11m$timestamp <- as.POSIXct(do.11m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.11m$timestamp)

#   3. Restrict for date range
do.11m <- subset(do.11m,timestamp >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-23 00:00:00'))
range(do.11m$timestamp)

#   4. Check new summary
summary(do.11m) 
# temp range: 0.355 to 9.744
# Dissolved.Oxygen range: 0.0290 to 9.2924
# Dissolved.Oxygen.Saturation range: 0.3413 to 106.6919

#   5. add in column for depth, deployment and sensor  
do.11m$depth <- 11
do.11m$deployment <- "Winter2018"
do.11m$sensor <- 248353
do.11m$flag_Temp <- "n"
do.11m$flag_DO <- "n"

# Data check from plots
qplot(timestamp, Temperature, data = do.11m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.11m, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
## IV. Summer18 + Winter 2018 Deployment

#   1. combine all QAQC winter 2018
PME_DO_winter18 <- rbind(do.3m, do.7m, do.11m)
summary(PME_DO_winter18)

#   2. Add in year 
PME_DO_winter18 <- transform(PME_DO_winter18,
               year = as.numeric(format(timestamp, '%Y')))
names(PME_DO_winter18)

#   3.select for relevant parameters
PME_DO_winter18 <- subset(PME_DO_winter18, select=c(sensor, deployment, year, timestamp, depth,
                                              Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, 
                                              Battery, Q, flag_Temp, flag_DO))
#   4. change names
names((old.datDO))
names(PME_DO_winter18)

colnames(PME_DO_winter18)[6] = "temperature"
colnames(PME_DO_winter18)[7] = "DO"
colnames(PME_DO_winter18)[8] = "DO_saturation"
colnames(PME_DO_winter18)[9] = "battery"

#   5. Add winter 2018 to summer 2018
PME_DO_agg18 <- rbind(old.datDO, PME_DO_winter18)
summary(PME_DO_agg18)

#   6. Plot and color by deployment:
p <- ggplot(PME_DO_agg18, aes(x=timestamp, y=(DO), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("DO") 


## ---------------------------
## V. Summer19 Deployment:
#     Some intial deployment notes:
#       * We removed the buoy from GL4 on 2019-07-30 from ~9:30-11:00am 
#           to add the chlorophyll-a sensor (C7).
#       * Also one of the sensor's had a damaged membrane and so was not deployed 

# 3m DO sensor
#   1. Read in new raw data at depth (for 2018-2019): DO_214423_190725_190820_3m.TXT
do.3m <- read.delim("DO_214423_190725_190820_3m.TXT", header=T, sep = ',')
names(do.3m)
summary(do.3m)

#   2. Fix timestamp
do.3m$timestamp <- as.POSIXct(do.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.3m$timestamp)

#   3. Restrict for date range
do.3ma <- subset(do.3m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-30 00:00:00'))
range(do.3ma$timestamp)
do.3mb <- subset(do.3m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                   timestamp <= as.POSIXct('2019-08-20 00:00:00'))
range(do.3mb$timestamp)
do.3m_1 <- rbind(do.3ma, do.3mb)

#   4. Check new summary
summary(do.3m_1) 
# temp range: 6.848 to 11.220
# Dissolved.Oxygen range: 7.101 to 8.782
# Dissolved.Oxygen.Saturation range: 96.27 to 113.83

#   5. Add in column for depth, deployment and sensor 
do.3m_1$depth <- 3
do.3m_1$deployment <- "Summer2019"
do.3m_1$sensor <- 214423
do.3m_1$flag_Temp <- "n"
do.3m_1$flag_DO <- "n"

# Data check from plots
qplot(timestamp, Temperature, data = do.3m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.3m_1, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
## VI. Summer19 Deployment: 9m DO Sensor

#   1. Read in new raw data at depth (for 2018-2019): 
do.9m <- read.delim("DO_245673_190725_190820_9m.TXT", header=T, sep = ',')
names(do.9m)
summary(do.9m)

#   2. fix timestamp
do.9m$timestamp <- as.POSIXct(do.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.9m$timestamp)

#   3. Restrict for date range
do.9ma <- subset(do.9m, timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                   timestamp <= as.POSIXct('2019-07-30 00:00:00'))
range(do.9ma$timestamp)
do.9mb <- subset(do.9m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                   timestamp <= as.POSIXct('2019-08-20 00:00:00'))
range(do.9mb$timestamp)
do.9m_1 <- rbind(do.9ma, do.9mb)

#  4. Check new summary
summary(do.9m_1) 
# temp range: 5.923 to 9.609
# Dissolved.Oxygen range: 7.229 to 9.043
# Dissolved.Oxygen.Saturation range: 97.63 to 113.80

#   5. add in column for depth, deployment and sensor 
do.9m_1$depth <- 9
do.9m_1$deployment <- "Summer2019"
do.9m_1$sensor <- 245673
do.9m_1$flag_Temp <- "n"
do.9m_1$flag_DO <- "n"

# Data check from plots
qplot(timestamp, Temperature, data = do.9m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.9m_1, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

## ---------------------------
## VII. All previous DO data + Summer 2018 Deployment

#   1. combine all QAQC winter 2018
PME_DO_summer19 <- rbind(do.3m_1, do.9m_1)
summary(PME_DO_summer19)

#   2. Add in year 
PME_DO_summer19 <- transform(PME_DO_summer19,
                             year = as.numeric(format(timestamp, '%Y')))
names(PME_DO_summer19)

#   3.select for relevant parameters
PME_DO_summer19 <- subset(PME_DO_summer19, select=c(sensor, deployment, year, timestamp, depth,
                                                    Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, 
                                                    Battery, Q, flag_Temp, flag_DO))
#   4. Change names
names((PME_DO_agg18))
names(PME_DO_summer19)

colnames(PME_DO_summer19)[6] = "temperature"
colnames(PME_DO_summer19)[7] = "DO"
colnames(PME_DO_summer19)[8] = "DO_saturation"
colnames(PME_DO_summer19)[9] = "battery"

#   5. Add winter 2018 to summer 2019
PME_DO_agg19 <- rbind(PME_DO_agg18, PME_DO_summer19)
summary(PME_DO_agg19)

# Plot and color by deployment:
p <- ggplot(PME_DO_agg19, aes(x=timestamp, y=(DO), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("DO") 

p <- ggplot(PME_DO_agg19, aes(x=timestamp, y=(temperature), colour =as.factor(deployment))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("DO") 

#write.csv(PME_DO_agg19, "Summer2019_PME_DO.csv") # complied data file of all DO sensors along buoy line


## ---------------------------
# VI. End notes:
#   NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
##  Back ground information for users:
#     For PME DO a Q value of < 0.7 is poor quality
#     Sensor manual: https://www.pme.com/product-installs/q-measurement-found-in-minidot

