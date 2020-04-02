## ---------------------------
## QA'QC for 3 Dissloved Oxygen Sensors (DO)
##    edit of first deployment: Summer18
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
# File path setup:
if (dir.exists('/Volumes/data/data2/rawarchive/gl4/buoy/')){
  inputDir<- '/Volumes/data/data2/rawarchive/gl4/buoy/'
  outputDir<- '/Users/kellyloria/Desktop/' 
}

# Don't forget to 
#     1. Set output path to personal desktop 
#     2. Physically move final files (pending datamanager approval) into final folder in server

## ---------------------------
# I. Read in Summer 2018 deployment data: 1.5m DO data
d1.5m <- read.csv(paste0(inputDir,"/2018_2019/DO/1807_1808_deployment/DO_1.5m.csv"), header=T)
names(d1.5m)

#   1. Fix timestamp - so it is no longer a character:
d1.5m$timestamp <- as.POSIXct(d1.5m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d1.5m$timestamp)

#   2. Restrict for date range:
d1.5m <- subset(d1.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d1.5m$timestamp)

# 3. Check temp and DO data:
qplot(timestamp, Dissolved.Oxygen, data = d1.5m, geom="point", color = factor(Depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) # !Outliers
summary(d1.5m)

###
# Flag outliers Temp and DO 
#   4. Flag temperatures
temp_mean <- (mean(d1.5m$Temperature)) 
temp_sd <- (sd(d1.5m$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d1.5m$flag_Temp[ d1.5m$Temperature > 14.63071 | d1.5m$Temperature < 7.940486 ] <- "o"
d1.5m$flag_Temp[ d1.5m$Temperature <= 14.63071 & d1.5m$Temperature >= 7.940486 ] <- "n"
#   *adjusted lower limit as it looked like 8.240486 cut off real data

qplot(timestamp, Temperature, data = d1.5m, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   5. Flag DO
DO_mean <- (mean(d1.5m$Dissolved.Oxygen)) 
DO_sd <- (sd(d1.5m$Dissolved.Oxygen)) 
# look for values 3 SD away from mean 
DO_cutoff <- (DO_sd*3)
#find outlier values 
DO_upL <- (DO_mean + DO_cutoff)
DO_lowL <- (DO_mean - DO_cutoff)
# Apply flag: flag_DO:
d1.5m$flag_DO[ d1.5m$Dissolved.Oxygen > 8.555786 | d1.5m$Dissolved.Oxygen < 6.356205 ] <- "o"
d1.5m$flag_DO[ d1.5m$Dissolved.Oxygen <= 8.555786 & d1.5m$Dissolved.Oxygen >= 6.356205 ] <- "n"
#   *adjusted upper limit as it looked like 8.155786 cut off real data

qplot(timestamp, Dissolved.Oxygen, data = d1.5m, geom="point", color=flag_DO) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 


## ---------------------------
# II. Read in Summer 2018 deployment data: 9m DO data
d9m <- read.csv(paste0(inputDir,"/2018_2019/DO/1807_1808_deployment/DO_9m.csv"), header=T)
names(d9m)

#   1. Fix timestamp - so it is no longer a character:
d9m$timestamp <- as.POSIXct(d9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d9m$timestamp)

#   2. Restrict for date range:
d9m_2 <- subset(d9m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d9m_2$timestamp)

#   3. Check temp and DO data:
hist(d9m_2$Temperature)
qplot(timestamp, Temperature, data = d9m_2, geom="point", color = factor(Depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###
# Flag outliers Temp and DO 
#   4. Flag temperatures
temp_mean <- (mean(d9m_2$Temperature)) 
temp_sd <- (sd(d9m_2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)
# find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d9m_2$flag_Temp[ d9m_2$Temperature > 13.19897 | d9m_2$Temperature < 4.216576 ] <- "o"
d9m_2$flag_Temp[ d9m_2$Temperature <= 13.19897 & d9m_2$Temperature >= 4.216576 ] <- "n"

qplot(timestamp, Temperature, data = d9m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# Flag outliers Temp and DO 
#   5. Flag DO
DO_mean <- (mean(d9m_2$Dissolved.Oxygen)) 
DO_sd <- (sd(d9m_2$Dissolved.Oxygen)) 
# look for values 4 SD away from mean 
DO_cutoff <- (DO_sd*4)
#find outlier values 
DO_upL <- (DO_mean + DO_cutoff)
DO_lowL <- (DO_mean - DO_cutoff)
# Apply flag: flag_DO:
d9m_2$flag_DO[ d9m_2$Dissolved.Oxygen > 8.455716 | d9m_2$Dissolved.Oxygen < 6.056275 ] <- "o"
d9m_2$flag_DO[ d9m_2$Dissolved.Oxygen <= 8.455716 & d9m_2$Dissolved.Oxygen >= 6.056275 ] <- "n"

qplot(timestamp, Dissolved.Oxygen, data = d9m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 
#  Temp flag catches odd DO point 


## ---------------------------
# III. Read in Summer 2018 deployment data: 11m DO data
d11.5m <- read.csv(paste0(inputDir,"/2018_2019/DO/1807_1808_deployment/DO_11.5m.csv"), header=T)
names(d11.5m)

#   1. Fix timestamp - so it is no longer a character:
d11.5m$timestamp <- as.POSIXct(d11.5m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d11.5m$timestamp)

#   2. Restrict for date range:
d11.5m_2 <- subset(d11.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d11.5m_2$timestamp)

#   3. Check temp and DO data:
hist(d11.5m_2$Temperature)
qplot(timestamp, Temperature, data = d11.5m_2, geom="point", color = factor(Depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###
# Flag outliers Temp and DO 
#   4. Flag temperatures
temp_mean <- (mean(d11.5m_2$Temperature)) #7.231291
temp_sd <- (sd(d11.5m_2$Temperature)) #1.077465
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d11.5m_2$flag_Temp[ d11.5m_2$Temperature > 11 | d11.5m_2$Temperature < 3.998897 ] <- "o"
d11.5m_2$flag_Temp[ d11.5m_2$Temperature <= 11 & d11.5m_2$Temperature >= 3.998897 ] <- "n"

qplot(timestamp, Temperature, data = d11.5m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# Flag outliers Temp and DO 
#   5. Flag DO
DO_mean <- (mean(d11.5m_2$Dissolved.Oxygen)) 
DO_sd <- (sd(d11.5m_2$Dissolved.Oxygen)) 
# look for values 4 SD away from mean 
DO_cutoff <- (DO_sd*3)
#find outlier values 
DO_upL <- (DO_mean + DO_cutoff)
DO_lowL <- (DO_mean - DO_cutoff)
# Apply flag: flag_DO:
d11.5m_2$flag_DO[ d11.5m_2$Dissolved.Oxygen > 9.494087 | d11.5m_2$Dissolved.Oxygen < 3.04832 ] <- "o"
d11.5m_2$flag_DO[ d11.5m_2$Dissolved.Oxygen <= 9.494087 & d11.5m_2$Dissolved.Oxygen >= 3.04832 ] <- "n"

qplot(timestamp, Dissolved.Oxygen, data = d9m_2, geom="point", color=flag_DO) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 
#  Temp flag catches odd DO point again 

## ---------------------------
# IV. Read combine all depths of Summer 2018 DO data
PME_DO_dat <- rbind(d1.5m, d9m_2, d11.5m_2)
summary(PME_DO_dat)

#   1. Add in year of deployment
PME_DO_dat$year <- 2018
PME_DO_dat$deployment <- "Summer2018"

#   2. Select for relevant parameters
PME_DO_dat_exp <- subset(PME_DO_dat, select=c(sensor, deployment, year, timestamp, Depth,
                                              Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, 
                                              Battery, Q, flag_Temp, flag_DO))
summary(PME_DO_dat_exp)

#   3. Change names:
colnames(PME_DO_dat_exp)[6] = "temperature"
colnames(PME_DO_dat_exp)[7] = "DO"
colnames(PME_DO_dat_exp)[8] = "DO_saturation"
colnames(PME_DO_dat_exp)[9] = "battery"

#write.csv(PME_DO_dat_exp, paste0(outputDir,"Summer2018_PME_DO.csv")) # complied data file of all DO sensors along buoy line

#   4. Check to make sure all sensors + data are there
Summer2018_DO <- qplot(timestamp, Dissolved.Oxygen.Saturation, data = PME_DO_dat_exp, geom="point", ylab = "DO saturation", color = factor(Depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + theme_classic()

## ---------------------------
# VI. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
##  * Back ground information for users:
#       For PME DO a Q value of < 0.7 is poor quality
#       Sensor manual: https://www.pme.com/product-installs/q-measurement-found-in-minidot

