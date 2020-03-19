## ---------------------------
## QA'QC for 8 RBR Tsolo Water Temperature Sensors
##    edit of first deployment: Summer18
##
## Author: Kelly A. Loria
## Date Created: 2019-03-02 & updated 2020-03-19
## Email: kelly.loria@colorado.edu
##
## ---------------------------
## Load packages:
library(ggplot2)
library(dplyr)

## ---------------------------
# I. Read in Summer 2018 deployment: 0.4m 

m0.4 <- read.csv("102495_07_03_08_21_2018_0.4m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
m0.4$timestamp <- as.POSIXct(m0.4$Time, format="%m/%d/%y %H:%M")
range(m0.4$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
m0.4.2 <- subset(m0.4,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(m0.4.2$timestamp)

#   3. Check data summary:
summary(m0.4.2)

#   Temperature has some odd points:
qplot(timestamp, Temperature, data = m0.4.2, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

###
#   4. Flag outliers temperatures
temp_mean <- (mean(m0.4.2$Temperature)) 
temp_sd <- (sd(m0.4.2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3.5)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
m0.4.2$flag_Temp[ m0.4.2$Temperature > 15.39588 | m0.4.2$Temperature < 7.115377 ] <- "o"
m0.4.2$flag_Temp[ m0.4.2$Temperature <= 15.39588 & m0.4.2$Temperature >= 7.115377 ] <- "n"

qplot(timestamp, Temperature, data = m0.4.2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 


## ---------------------------
# II. Read in Summer 2018 deployment: 1.5m 
d1.5m <- read.csv("102496_07_03_08_21_2018_1.5m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
d1.5m$timestamp <- as.POSIXct(d1.5m$Time, format="%m/%d/%y %H:%M")
range(d1.5m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d1.5m.2 <- subset(d1.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d1.5m.2$timestamp)

#   3. Check data summary:
summary(d1.5m.2) 

###
#   4. Flag outliers temperatures
temp_mean <- (mean(d1.5m.2$Temperature)) 
temp_sd <- (sd(d1.5m.2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*4)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d1.5m.2$flag_Temp[ d1.5m.2$Temperature > 15.68842 | d1.5m.2$Temperature < 7.192818 ] <- "o"
d1.5m.2$flag_Temp[ d1.5m.2$Temperature <= 15.68842 & d1.5m.2$Temperature >= 7.192818 ] <- "n"

qplot(timestamp, Temperature, data = d1.5m.2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 


## ---------------------------
# III. Read in Summer 2018 deployment: 3m 
d3m <- read.csv("102497_07_03_08_21_2018_3m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
d3m$timestamp <- as.POSIXct(d3m$Time, format="%m/%d/%y %H:%M")
range(d3m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d3m_2 <- subset(d3m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d3m_2$timestamp)

#   3. Check data summary:
summary(d3m_2)

qplot(timestamp, Temperature, data = d3m_2, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

###
#   4. Flag outliers temperatures
temp_mean <- (mean(d3m_2$Temperature)) 
temp_sd <- (sd(d3m_2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d3m_2$flag_Temp[ d3m_2$Temperature > 14.60748 | d3m_2$Temperature < 7.724803 ] <- "o"
d3m_2$flag_Temp[ d3m_2$Temperature <= 14.60748 & d3m_2$Temperature >= 7.724803 ] <- "n"

qplot(timestamp, Temperature, data = d3m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

## ---------------------------
# IV. Read in Summer 2018 deployment: 5.1m 
d5.1m <- read.csv("102498_07_03_08_21_2018_5.1m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
d5.1m$timestamp <- as.POSIXct(d5.1m$Time, format="%m/%d/%y %H:%M")
range(d5.1m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d5.1m_2 <- subset(d5.1m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d5.1m_2$timestamp)

#   3. Check out the summary data spread
qplot(timestamp, Temperature, data = d5.1m_2, geom="point") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###
#   4. Flag outliers temperatures
temp_mean <- (mean(d5.1m_2$Temperature)) 
temp_sd <- (sd(d5.1m_2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d5.1m_2$flag_Temp[ d5.1m_2$Temperature > 14.19813 | d5.1m_2$Temperature < 7.134998 ] <- "o"
d5.1m_2$flag_Temp[ d5.1m_2$Temperature <= 14.19813 & d5.1m_2$Temperature >= 7.134998 ] <- "n"

qplot(timestamp, Temperature, data = d5.1m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 


## ---------------------------
# V. Read in Summer 2018 deployment: 7.5m 
d7.5m <- read.csv("102499_07_03_08_21_2018_7.5m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
d7.5m$timestamp <- as.POSIXct(d7.5m$Time, format="%m/%d/%y %H:%M")
range(d$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d7.5m_2 <- subset(d7.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d7.5m_2$timestamp)

qplot(timestamp, Temperature, data = d7.5m_2, geom="point") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###
#   3. Flag outliers temperatures (here none)
d7.5m_2$flag_Temp <- "n"

qplot(timestamp, Temperature, data = d7.5m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

## ---------------------------
# VI. Read in Summer 2018 deployment: 10m 
d10m <- read.csv("102500_07_03_08_21_2018_10m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
d10m$timestamp <- as.POSIXct(d10m$Time, format="%m/%d/%y %H:%M")
range(d$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d10m_2 <- subset(d10m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d10m_2$timestamp)

qplot(timestamp, Temperature, data = d10m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###
#   3. Flag outliers temperatures
temp_mean <- (mean(d10m_2$Temperature)) 
temp_sd <- (sd(d10m_2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d10m_2$flag_Temp[ d10m_2$Temperature > 12.03469 | d10m_2$Temperature < 3.831558 ] <- "o"
d10m_2$flag_Temp[ d10m_2$Temperature <= 12.03469 & d10m_2$Temperature >= 3.831558 ] <- "n"

qplot(timestamp, Temperature, data = d10m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 


## ---------------------------
# VII. Read in Summer 2018 deployment: 11.5m 
dll.5m <- read.csv("102501_07_03_08_21_2018_11.5m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
dll.5m$timestamp <- as.POSIXct(dll.5m$Time, format="%m/%d/%y %H:%M")
range(dll.5m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
dll.5m_2 <- subset(dll.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(dll.5m$timestamp)

qplot(timestamp, Temperature, data = dll.5m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###
#   3. Flag outliers temperatures
temp_mean <- (mean(dll.5m_2$Temperature)) 
temp_sd <- (sd(dll.5m_2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*4)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
dll.5m_2$flag_Temp[ dll.5m_2$Temperature > 11.52138 | dll.5m_2$Temperature < 2.937595 ] <- "o"
dll.5m_2$flag_Temp[ dll.5m_2$Temperature <= 11.52138 & dll.5m_2$Temperature >= 2.937595 ] <- "n"

qplot(timestamp, Temperature, data = dll.5m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

## ---------------------------
# VIII. Read in Summer 2018 deployment: 6.5m 
d6.5m <- read.csv("102502_07_03_08_21_2018_6.4m.csv", header=T)

#   1. Fix timestamp - so it is no longer a character:
d6.5m$timestamp <- as.POSIXct(d6.5m$Time, format="%m/%d/%y %H:%M")
range(d6.5m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d6.5m_2 <- subset(d6.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d6.5m_2$timestamp)

qplot(timestamp, Temperature, data = d6.5m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

###
#   3. Flag outliers temperatures
temp_mean <- (mean(d6.5m_2$Temperature)) 
temp_sd <- (sd(d6.5m_2$Temperature)) 
# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*4)
#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)
# Apply flag: flag_Temp
d6.5m_2$flag_Temp[ d6.5m_2$Temperature > 15.38964 | d6.5m_2$Temperature < 5.229635 ] <- "o"
d6.5m_2$flag_Temp[ d6.5m_2$Temperature <= 15.38964 & d6.5m_2$Temperature >= 5.229635 ] <- "n"

qplot(timestamp, Temperature, data = d6.5m_2, geom="point", color=flag_Temp) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

## ---------------------------
# IX. Combine all sensors data into 1 file:
names(m0.4.2)
names(d1.5m.2)
names(d3m_2)
names(d5.1m_2)
names(d6.5m_2)
names(d7.5m_2)
names(d10m_2)
names(dll.5m_2)

rbr_dat <- rbind(m0.4.2, d1.5m.2, d3m_2, d5.1m_2, d6.5m_2, d7.5m_2, d10m_2, dll.5m_2)
summary(rbr_dat)

#   2. Fix sensor serial number 12500 to 102500
rbr_dat$Sensor[rbr_dat$Sensor == 12500] <- 102500
summary(rbr_dat)

#   3. Add in year of deployment
rbr_dat$year <- 2018
rbr_dat$deployment <- "Summer2018"

#   4. fix column order
rbr_dat_exp <- subset(rbr_dat, select=c(Sensor, deployment, year, timestamp, Depth, Temperature, flag_Temp))
summary(rbr_dat_exp)
# write.csv(rbr_dat_exp, "Summer2018_RBR.csv") # complied data file of all RBR temp sensors along buoy line 

# Check to make sure all sensors + data are there
names(rbr_dat)
Summer2018_RBR <- qplot(timestamp, Temperature, data = rbr_dat, geom="point", ylab = "Temperature [C]",
                        color = factor(Depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + theme_classic()
#ggsave("Summer2018_RBR.pdf", Summer2018_RBR, scale = 2, width = 15, height = 5, units = c("cm"), dpi = 500)

## ---------------------------
# VI. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   * No clear battery column in raw download overtime
#   * Salinity and original calibration settings seem a little off


