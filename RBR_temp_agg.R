# QA'QC for 8 RBR TSolo sensors
# Onging
library(ggplot2)
library(scales)
library(dplyr)

# Back ground information for users:
#    link to product mannual
#    https://rbr-global.com/products/compact-loggers/rbrsolo-t
#    RBR sensors have a measurement range of -5°C to +35°C

# Read in past year's data
old.datTemp <- read.csv("gl4.buoy.RBRTemp.data.csv", header=T)

# Fix timestamp - so it is no longer a character:
old.datTemp$timestamp1 <- as.POSIXct(old.datTemp$timestamp, format= "%Y-%m-%d %H:%M:%OS")
range(old.datTemp$timestamp1)

##############################
### Winter 2018 Deployment ###

### 3.5m temp sensor ###

# 1. Read in new raw data at depth (for 2018-2019): RBR102495_180823_190723_3.5m.csv
rbr.3.5m <- read.csv("RBR102495_180823_190723_3.5m.csv", header=T, sep = ',')
names(rbr.3.5m)
summary(rbr.3.5m)

###
# 2. fix timestamp
rbr.3.5m$timestamp1 <- as.POSIXct(rbr.3.5m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.3.5m$timestamp1)

# restrict for date range
rbr.3.5m <- subset(rbr.3.5m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(rbr.3.5m$timestamp1)

#check new summary
summary(rbr.3.5m) 
# temp range: 0.0794 to 10.0993

###
# 3. add in column for depth, deployment and sensor 
rbr.3.5m$depth <- 3.5
rbr.3.5m$deployment <- "Winter2018"
rbr.3.5m$sensor <- 102495

qplot(timestamp1, Temperature, data = rbr.3.5m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 4.5m temp sensor ###

# 1. Read in new raw data at depth (for 2018-2019): RBR102496_180823_190723_4.5m.csv
rbr.4.5m <- read.csv("RBR102496_180823_190723_4.5m.csv", header=T, sep = ',')
names(rbr.4.5m)
summary(rbr.4.5m)

###
# 2. fix timestamp
rbr.4.5m$timestamp1 <- as.POSIXct(rbr.4.5m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.4.5m$timestamp1)

# restrict for date range
rbr.4.5m <- subset(rbr.4.5m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                     timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(rbr.4.5m$timestamp1)

#check new summary
summary(rbr.4.5m) 
# temp range: 0.1861 to 10.0647

###
# 3. add in column for depth, deployment and sensor 
rbr.4.5m$depth <- 4.5
rbr.4.5m$deployment <- "Winter2018"
rbr.4.5m$sensor <- 102496

qplot(timestamp1, Temperature, data = rbr.4.5m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

### 6m temp sensor ###
# download error RBR102497_180823_190723_6m.xlsx

### 8m temp sensor ###
# download error RBR102497_180823_190723_6m.xlsx

### 9m temp sensor ###

# 1. Read in new raw data at depth (for 2018-2019): RBR102499_180823_190723_9m.csv
rbr.9m <- read.csv("RBR102499_180823_190723_9m.csv", header=T, sep = ',')
names(rbr.9m)
summary(rbr.9m)

###
# 2. fix timestamp
rbr.9m$timestamp1 <- as.POSIXct(rbr.9m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.9m$timestamp1)

# restrict for date range
rbr.9m <- subset(rbr.9m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                     timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(rbr.9m$timestamp1)

#check new summary
summary(rbr.9m) 
# temp range: 0.3547 to 9.7952

###
# 3. add in column for depth, deployment and sensor 
rbr.9m$depth <- 9
rbr.9m$deployment <- "Winter2018"
rbr.9m$sensor <- 102499

qplot(timestamp1, Temperature, data = rbr.9m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 10m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102500_180823_190723_10m.csv
rbr.10m <- read.csv("RBR102500_180823_190723_10m.csv", header=T, sep = ',')
names(rbr.10m)
summary(rbr.10m)

###
# 2. fix timestamp
rbr.10m$timestamp1 <- as.POSIXct(rbr.10m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.10m$timestamp1)

# restrict for date range
rbr.10m <- subset(rbr.10m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                   timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(rbr.10m$timestamp1)

#check new summary
summary(rbr.10m) 
# temp range: 0.419 to 9.682

###
# 3. add in column for depth, deployment and sensor 
rbr.10m$depth <- 10
rbr.10m$deployment <- "Winter2018"
rbr.10m$sensor <- 102500

qplot(timestamp1, Temperature, data = rbr.10m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 12m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102501_180823_190723_12m.csv
rbr.12m <- read.csv("RBR102501_180823_190723_12m.csv", header=T, sep = ',')
names(rbr.12m)
summary(rbr.12m)

###
# 2. fix timestamp
rbr.12m$timestamp1 <- as.POSIXct(rbr.12m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.12m$timestamp1)

# restrict for date range
rbr.12m <- subset(rbr.12m, timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                    timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(rbr.12m$timestamp1)

#check new summary
summary(rbr.12m) 
# temp range: 0.3993 to 9.7049

###
# 3. add in column for depth, deployment and sensor 
rbr.12m$depth <- 12
rbr.12m$deployment <- "Winter2018"
rbr.12m$sensor <- 102501

qplot(timestamp1, Temperature, data = rbr.12m, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

######################################################
########## ALL DO + Winter2018 DATA #################

###
# 1. combine all QAQC winter 2018
RBR_temp_winter18 <- rbind(rbr.3.5m, rbr.4.5m, rbr.9m, rbr.10m, rbr.12m)
summary(RBR_temp_winter18)

###
# 2. Add in year 
RBR_temp_winter18 <- transform(RBR_temp_winter18,
                             year = as.numeric(format(timestamp1, '%Y')))

names(RBR_temp_winter18)

###
# 3.select for relevant parameters
RBR_temp_winter18 <- subset(RBR_temp_winter18, select=c(sensor, deployment, year, timestamp1, depth,
                                                    Temperature))
colnames(RBR_temp_winter18)[6] = "temperature"

# 3.select for relevant parameters
old.datTemp <- subset(old.datTemp, select=c(sensor, deployment, year, timestamp1, depth,
                                                        temperature))

###
# 4. Add winter 2018 to summer 2018
RBR_temp_agg18 <- rbind(old.datTemp, RBR_temp_winter18)
summary(RBR_temp_agg18)

###
# 5. change names
colnames(RBR_temp_agg18)[4] = "timestamp"

# Plot and facet by deployment:
p <- ggplot(RBR_temp_agg18, aes(x=timestamp, y=(temperature), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("DO") 



##############################
##############################
### Summer 2019 Deployment ###

# As a deployment note we removed the buoy from GL4 on 2019-07-30 
# from 9:30-11:00 am to add the chlorophyll-a sensor (C7).

### 0.5m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102495_190725_190820_0.5m.csv
rbr.0.5m <- read.csv("RBR102495_190725_190820_0.5m.csv", header=T, sep = ',')
names(rbr.0.5m)
summary(rbr.0.5m)

###
# 2. fix timestamp
rbr.0.5m$timestamp <- as.POSIXct(rbr.0.5m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.0.5m$timestamp)

# restrict for date range
rbr.0.5ma <- subset(rbr.0.5m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                   timestamp <= as.POSIXct('2019-07-30 00:00:00'))
range(rbr.0.5ma$timestamp)
rbr.0.5mb <- subset(rbr.0.5m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                   timestamp <= as.POSIXct('2019-08-20 00:00:00'))
range(rbr.0.5mb$timestamp)
rbr.0.5m_1 <- rbind(rbr.0.5mb, rbr.0.5ma)

#check new summary
summary(rbr.0.5m_1) 
# temp range: 6.832 to 11.440

###
# 3. add in column for depth, deployment and sensor 
rbr.0.5m_1$depth <- 0.5
rbr.0.5m_1$deployment <- "Summer2019"
rbr.0.5m_1$sensor <- 102495

qplot(timestamp, Temperature, data = rbr.0.5m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 2m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102496_190725_190820_2m.csv
rbr.2m <- read.csv("RBR102496_190725_190820_2m.csv", header=T, sep = ',')
names(rbr.2m)
summary(rbr.2m)

###
# 2. fix timestamp
rbr.2m$timestamp <- as.POSIXct(rbr.2m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.2m$timestamp)

# restrict for date range
rbr.2ma <- subset(rbr.2m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                      timestamp <= as.POSIXct('2019-07-30 00:00:00'))

rbr.2mb <- subset(rbr.2m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                      timestamp <= as.POSIXct('2019-08-20 00:00:00'))
rbr.2m_1 <- rbind(rbr.2mb, rbr.2ma)

#check new summary
summary(rbr.2m_1) 
# temp range: 6.832 to 11.440

###
# 3. add in column for depth, deployment and sensor 
rbr.2m_1$depth <- 2
rbr.2m_1$deployment <- "Summer2019"
rbr.2m_1$sensor <- 102496

qplot(timestamp, Temperature, data = rbr.2m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 4m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102497_190725_190820_4m.csv
rbr.4m <- read.csv("RBR102497_190725_190820_4m.csv", header=T, sep = ',')
names(rbr.4m)
summary(rbr.4m)

###
# 2. fix timestamp
rbr.4m$timestamp <- as.POSIXct(rbr.4m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.4m$timestamp)

# restrict for date range
rbr.4ma <- subset(rbr.4m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                    timestamp <= as.POSIXct('2019-07-30 00:00:00'))

rbr.4mb <- subset(rbr.4m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                    timestamp <= as.POSIXct('2019-08-20 00:00:00'))
rbr.4m_1 <- rbind(rbr.4mb, rbr.4ma)

#check new summary
summary(rbr.4m_1) 
# temp range: 6.832 to 11.440

###
# 3. add in column for depth, deployment and sensor 
rbr.4m_1$depth <- 4
rbr.4m_1$deployment <- "Summer2019"
rbr.4m_1$sensor <- 102497

qplot(timestamp, Temperature, data = rbr.4m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

### 6m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102498_190725_190820_6m.csv
rbr.6m <- read.csv("RBR102498_190725_190820_6m.csv", header=T, sep = ',')
names(rbr.6m)
summary(rbr.6m)

###
# 2. fix timestamp
rbr.6m$timestamp <- as.POSIXct(rbr.6m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.6m$timestamp)

# restrict for date range
rbr.6ma <- subset(rbr.6m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                    timestamp <= as.POSIXct('2019-07-30 00:00:00'))

rbr.6mb <- subset(rbr.6m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                    timestamp <= as.POSIXct('2019-08-20 00:00:00'))
rbr.6m_1 <- rbind(rbr.6mb, rbr.6ma)

#check new summary
summary(rbr.6m_1) 
# temp range: 6.449 to 10.309

###
# 3. add in column for depth, deployment and sensor 
rbr.6m_1$depth <- 6
rbr.6m_1$deployment <- "Summer2019"
rbr.6m_1$sensor <- 102498

qplot(timestamp, Temperature, data = rbr.6m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 8m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102499_190725_190820_8m.csv
rbr.8m <- read.csv("RBR102499_190725_190820_8m.csv", header=T, sep = ',')
names(rbr.8m)
summary(rbr.8m)

###
# 2. fix timestamp
rbr.8m$timestamp <- as.POSIXct(rbr.8m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.8m$timestamp)

# restrict for date range
rbr.8ma <- subset(rbr.8m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                    timestamp <= as.POSIXct('2019-07-30 00:00:00'))

rbr.8mb <- subset(rbr.8m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                    timestamp <= as.POSIXct('2019-08-20 00:00:00'))


rbr.8m_1 <- rbind(rbr.8mb, rbr.8ma)

#check new summary
summary(rbr.8m_1) 
# temp range: 6.219 to 9.755

###
# 3. add in column for depth, deployment and sensor 
rbr.8m_1$depth <- 8
rbr.8m_1$deployment <- "Summer2019"
rbr.8m_1$sensor <- 102499

qplot(timestamp, Temperature, data = rbr.8m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 10m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102500_190725_190820_10m.csv
rbr.10m <- read.csv("RBR102500_190725_190820_10m.csv", header=T, sep = ',')
names(rbr.10m)
summary(rbr.10m)

###
# 2. fix timestamp
rbr.10m$timestamp <- as.POSIXct(rbr.10m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.10m$timestamp)

# restrict for date range
rbr.10ma <- subset(rbr.10m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                    timestamp <= as.POSIXct('2019-07-30 00:00:00'))

rbr.10mb <- subset(rbr.10m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                    timestamp <= as.POSIXct('2019-08-20 00:00:00'))
rbr.10m_1 <- rbind(rbr.10mb, rbr.10ma)

#check new summary
summary(rbr.10m_1) 
# temp range: 5.373 to 9.436

###
# 3. add in column for depth, deployment and sensor 
rbr.10m_1$depth <- 10
rbr.10m_1$deployment <- "Summer2019"
rbr.10m_1$sensor <- 102500

qplot(timestamp, Temperature, data = rbr.10m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 12m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102501_190725_190820_12m.csv
rbr.12m <- read.csv("RBR102501_190725_190820_12m.csv", header=T, sep = ',')
names(rbr.12m)
summary(rbr.12m)

###
# 2. fix timestamp
rbr.12m$timestamp <- as.POSIXct(rbr.12m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.12m$timestamp)

# restrict for date range
rbr.12ma <- subset(rbr.12m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                     timestamp <= as.POSIXct('2019-07-30 00:00:00'))

rbr.12mb <- subset(rbr.12m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                     timestamp <= as.POSIXct('2019-08-20 00:00:00'))
rbr.12m_1 <- rbind(rbr.12mb, rbr.12ma)

#check new summary
summary(rbr.12m_1) 
# temp range: 5.373 to 9.436

###
# 3. add in column for depth, deployment and sensor 
rbr.12m_1$depth <- 12
rbr.12m_1$deployment <- "Summer2019"
rbr.12m_1$sensor <- 102501

qplot(timestamp, Temperature, data = rbr.12m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


### 13m temp sensor ###
# 1. Read in new raw data at depth (for 2018-2019): RBR102502_190725_190820_13m.csv
rbr.13m <- read.csv("RBR102502_190725_190820_13m.csv", header=T, sep = ',')
names(rbr.13m)
summary(rbr.13m)

###
# 2. fix timestamp
rbr.13m$timestamp <- as.POSIXct(rbr.13m$Time, format="%Y-%m-%d %H:%M:%OS")
range(rbr.13m$timestamp)

# restrict for date range
rbr.13ma <- subset(rbr.13m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                     timestamp <= as.POSIXct('2019-07-30 00:00:00'))

rbr.13mb <- subset(rbr.12m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                     timestamp <= as.POSIXct('2019-08-20 00:00:00'))
rbr.13m_1 <- rbind(rbr.13ma, rbr.13mb)

#check new summary
summary(rbr.13m_1) 
# temp range: 4.819 to 9.167

###
# 3. add in column for depth, deployment and sensor 
rbr.13m_1$depth <- 13
rbr.13m_1$deployment <- "Summer2019"
rbr.13m_1$sensor <- 102502

qplot(timestamp, Temperature, data = rbr.13m_1, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


#############################################################
########## RBR_temp_agg18 + Summer2019 DATA #################

###
# 1. combine all QAQC winter 2018
RBR_temp_summer19 <- rbind(rbr.0.5m_1,rbr.2m_1, rbr.4m_1, rbr.6m_1, rbr.8m_1, rbr.10m_1, rbr.12m_1, rbr.13m_1)
summary(RBR_temp_summer19)

###
# 2. Add in year 
RBR_temp_summer19 <- transform(RBR_temp_summer19,
                               year = as.numeric(format(timestamp, '%Y')))
names(RBR_temp_summer19)

###
# 3.select for relevant parameters
RBR_temp_summer19 <- subset(RBR_temp_summer19, select=c(sensor, deployment, year, timestamp, depth,
                                                        Temperature))
colnames(RBR_temp_summer19)[6] = "temperature"

###
# 4. Add winter 2018 to summer 2018
RBR_temp_agg19 <- rbind(RBR_temp_agg18, RBR_temp_summer19)
summary(RBR_temp_agg19)

# Plot and facet by deployment:
p <- ggplot(RBR_temp_agg19, aes(x=timestamp, y=(temperature), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("DO") 

#write.csv(RBR_temp_agg19, "Summer2019_RBR_Temp.csv") # complied data file of all RBR sensors along buoy line

