# QA'QC for 3 PME Dissolved Oxygen sensors
# summer 2018 - summer 2019

# load packages for visualization
library(ggplot2)
library(scales)
library(dplyr)

# Back ground information for users
#    For PME DO a Q value of < 0.7 is poor quality
#    https://www.pme.com/product-installs/q-measurement-found-in-minidot

# Read in past year's data - here 2018 summer
old.datDO <- read.csv("GL4.buoy.PMEDO.data.csv", header=T)

# Fix timestamp - so it is no longer a character:
# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%OS"
tmp1stmp <- as.POSIXct(old.datDO$timestamp,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date) == length(tmp1stmp[!is.na(tmp1stmp)])){old.datDO$timestamp <- tmp1stmp } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1stmp) 

#check to make sure timestamp looks okay
summary(old.datDO)

##############################
### Winter 2018 Deployment ###

### 3m DO sensor ###
# 1. Read in new raw data at depth (for 2018-2019): DO_214423_180823_190723_3m.TXT
do.3m <- read.delim("DO_214423_180823_190723_3m.TXT", header=T, sep = ',')
names(do.3m)
summary(do.3m)

###
# 2. fix timestamp
do.3m$timestamp <- as.POSIXct(do.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.3m$timestamp)

# restrict for date range
do.3m <- subset(do.3m,timestamp >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-23 00:00:00'))
range(do.3m$timestamp)

#check new summary
summary(do.3m) 
# temp range: -0.003 to 10.353
# Dissolved.Oxygen range: 5.688 to 12.090
# Dissolved.Oxygen.Saturation range: 60.65 to 130.99

###
# 3. add in column for depth, deployment and sensor 
do.3m$depth <- 3
do.3m$deployment <- "Winter2018"
do.3m$sensor <- 214423

###
# 4. data check from plots
qplot(timestamp, Temperature, data = do.3m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.3m, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 7m DO Sensor ###
# 1. Read in new raw data at depth (for 2018-2019): DO_245673_180823_190723_7m.TXT
do.7m <- read.delim("DO_245673_180823_190723_7m.TXT", header=T, sep = ',')
names(do.7m)
summary(do.7m)

###
# 2. fix timestamp
do.7m$timestamp <- as.POSIXct(do.7m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.7m$timestamp)

# restrict for date range
do.7m <- subset(do.7m,timestamp >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-23 00:00:00'))
range(do.7m$timestamp)

#check new summary
summary(do.7m) 
# temp range: 0.333 to 9.957
# Dissolved.Oxygen range: 3.987 to 9.462
# Dissolved.Oxygen.Saturation range: 46.52 to 110.42

###
# 3. add in column for depth, deployment and sensor 
do.7m$depth <- 7
do.7m$deployment <- "Winter2018"
do.7m$sensor <- 245673

###
# 4. data check from plots
qplot(timestamp, Temperature, data = do.7m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.7m, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 11m DO Sensor ###
# 1. Read in new raw data at depth (for 2018-2019): DO_245673_180823_190723_7m.TXT
do.11m <- read.delim("DO_248353_180823_190723_11m.TXT", header=T, sep = ',')
names(do.11m)
summary(do.11m)

###
# 2. fix timestamp
do.11m$timestamp <- as.POSIXct(do.11m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.11m$timestamp)

# restrict for date range
do.11m <- subset(do.11m,timestamp >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-23 00:00:00'))
range(do.11m$timestamp)

#check new summary
summary(do.11m) 
# temp range: 0.355 to 9.744
# Dissolved.Oxygen range: 0.0290 to 9.2924
# Dissolved.Oxygen.Saturation range: 0.3413 to 106.6919

###
# 3. add in column for depth, deployment and sensor 
do.11m$depth <- 11
do.11m$deployment <- "Winter2018"
do.11m$sensor <- 248353

###
# 4. data check from plots
qplot(timestamp, Temperature, data = do.11m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.11m, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


######################################################
########## ALL DO + Winter2018 DATA #################

###
# 1. combine all QAQC winter 2018
PME_DO_winter18 <- rbind(do.3m, do.7m, do.11m)
summary(PME_DO_winter18)

###
# 2. Add in year 
PME_DO_winter18 <- transform(PME_DO_winter18,
               year = as.numeric(format(timestamp, '%Y')))

names(PME_DO_winter18)

###
# 3.select for relevant parameters
PME_DO_winter18 <- subset(PME_DO_winter18, select=c(sensor, deployment, year, timestamp, depth,
                                              Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, 
                                              Battery, Q))
###
# 4. change names
names((old.datDO))
names(PME_DO_winter18)

colnames(PME_DO_winter18)[6] = "temperature"
colnames(PME_DO_winter18)[7] = "DO"
colnames(PME_DO_winter18)[8] = "DO_saturation"
colnames(PME_DO_winter18)[9] = "battery"

###
# 5. Add winter 2018 to summer 2018
PME_DO_agg18 <- rbind(old.datDO, PME_DO_winter18)
summary(PME_DO_agg18)


# Plot and facet by deployment:
p <- ggplot(PME_DO_agg18, aes(x=timestamp, y=(DO), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("DO") 


##############################
##############################
### Summer 2019 Deployment ###

# As a deployment note we removed the buoy from GL4 on 2019-07-30 
# from 9:30-11:00 am to add the chlorophyll-a sensor (C7).

# Also one of the sensor's had a damaged membrane and so was not deployed 

### 3m DO sensor ###
# 1. Read in new raw data at depth (for 2018-2019): DO_214423_190725_190820_3m.TXT
do.3m <- read.delim("DO_214423_190725_190820_3m.TXT", header=T, sep = ',')
names(do.3m)
summary(do.3m)

###
# 2. fix timestamp
do.3m$timestamp <- as.POSIXct(do.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.3m$timestamp)

# restrict for date range
do.3ma <- subset(do.3m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                  timestamp <= as.POSIXct('2019-07-30 00:00:00'))
range(do.3ma$timestamp)
do.3mb <- subset(do.3m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                   timestamp <= as.POSIXct('2019-08-20 00:00:00'))
range(do.3mb$timestamp)
do.3m_1 <- rbind(do.3ma, do.3mb)

#check new summary
summary(do.3m_1) 
# temp range: 6.848 to 11.220
# Dissolved.Oxygen range: 7.101 to 8.782
# Dissolved.Oxygen.Saturation range: 96.27 to 113.83

###
# 3. add in column for depth, deployment and sensor 
do.3m_1$depth <- 3
do.3m_1$deployment <- "Summer2019"
do.3m_1$sensor <- 214423

###
# 4. data check from plots
qplot(timestamp, Temperature, data = do.3m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.3m_1, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 9m DO Sensor ###
# 1. Read in new raw data at depth (for 2018-2019): 
do.9m <- read.delim("DO_245673_190725_190820_9m.TXT", header=T, sep = ',')
names(do.9m)
summary(do.9m)

###
# 2. fix timestamp
do.9m$timestamp <- as.POSIXct(do.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(do.9m$timestamp)

# restrict for date range
do.9ma <- subset(do.9m, timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                   timestamp <= as.POSIXct('2019-07-30 00:00:00'))
range(do.9ma$timestamp)

do.9mb <- subset(do.9m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                   timestamp <= as.POSIXct('2019-08-20 00:00:00'))
range(do.9mb$timestamp)

do.9m_1 <- rbind(do.9ma, do.9mb)

#check new summary
summary(do.9m_1) 
# temp range: 5.923 to 9.609
# Dissolved.Oxygen range: 7.229 to 9.043
# Dissolved.Oxygen.Saturation range: 97.63 to 113.80

###
# 3. add in column for depth, deployment and sensor 
do.9m_1$depth <- 9
do.9m_1$deployment <- "Summer2019"
do.9m_1$sensor <- 245673

###
# 4. data check from plots
qplot(timestamp, Temperature, data = do.9m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, Dissolved.Oxygen, data = do.9m_1, geom="point", ylab = "DO [mgL]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


######################################################
########## ALL DO + Winter2018 DATA #################

###
# 1. combine all QAQC winter 2018
PME_DO_summer19 <- rbind(do.3m_1, do.9m_1)
summary(PME_DO_summer19)

###
# 2. Add in year 
PME_DO_summer19 <- transform(PME_DO_summer19,
                             year = as.numeric(format(timestamp, '%Y')))

names(PME_DO_summer19)

###
# 3.select for relevant parameters
PME_DO_summer19 <- subset(PME_DO_summer19, select=c(sensor, deployment, year, timestamp, depth,
                                                    Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, 
                                                    Battery, Q))
###
# 4. change names
names((old.datDO))
names(PME_DO_summer19)

colnames(PME_DO_summer19)[6] = "temperature"
colnames(PME_DO_summer19)[7] = "DO"
colnames(PME_DO_summer19)[8] = "DO_saturation"
colnames(PME_DO_summer19)[9] = "battery"

###
# 5. Add winter 2018 to summer 2019
PME_DO_agg19 <- rbind(PME_DO_agg18, PME_DO_summer19)
summary(PME_DO_agg19)


# Plot and facet by deployment:
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
