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
library(lubridate)
library(tidyverse)
library(zoo)
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
# I. Read in Summer 2018 deployment: 0.4m 
m0.4 <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102495_07_03_08_21_2018_0.4m.csv"), 
                 header=T)

#   1. Fix timestamp - so it is no longer a character:
m0.4$timestamp <- as.POSIXct(m0.4$Time, format="%m/%d/%y %H:%M")
range(m0.4$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
m0.4.2 <- subset(m0.4,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                   timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(m0.4.2$timestamp)

#   3. Check data summary:
summary(m0.4.2)

## ---------------------------
# II. Read in Summer 2018 deployment: 1.5m 
d1.5m <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102496_07_03_08_21_2018_1.5m.csv"),
                  header=T)

#   1. Fix timestamp - so it is no longer a character:
d1.5m$timestamp <- as.POSIXct(d1.5m$Time, format="%m/%d/%y %H:%M")
range(d1.5m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d1.5m.2 <- subset(d1.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                    timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d1.5m.2$timestamp)

#   3. Check data summary:
summary(d1.5m.2) 

## ---------------------------
# III. Read in Summer 2018 deployment: 3m 
d3m <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102497_07_03_08_21_2018_3m.csv"), 
                header=T)

#   1. Fix timestamp - so it is no longer a character:
d3m$timestamp <- as.POSIXct(d3m$Time, format="%m/%d/%y %H:%M")
range(d3m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d3m_2 <- subset(d3m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                  timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d3m_2$timestamp)

#   3. Check data summary:
summary(d3m_2)

## ---------------------------
# IV. Read in Summer 2018 deployment: 5.1m 
d5.1m <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102498_07_03_08_21_2018_5.1m.csv"), 
                  header=T)

#   1. Fix timestamp - so it is no longer a character:
d5.1m$timestamp <- as.POSIXct(d5.1m$Time, format="%m/%d/%y %H:%M")
range(d5.1m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d5.1m_2 <- subset(d5.1m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                    timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d5.1m_2$timestamp)

## ---------------------------
# V. Read in Summer 2018 deployment: 7.5m 
d7.5m <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102499_07_03_08_21_2018_7.5m.csv"), 
                  header=T)

#   1. Fix timestamp - so it is no longer a character:
d7.5m$timestamp <- as.POSIXct(d7.5m$Time, format="%m/%d/%y %H:%M")
range(d$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d7.5m_2 <- subset(d7.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                    timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d7.5m_2$timestamp)

## ---------------------------
# VI. Read in Summer 2018 deployment: 10m 
d10m <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102500_07_03_08_21_2018_10m.csv"), 
                 header=T)

#   1. Fix timestamp - so it is no longer a character:
d10m$timestamp <- as.POSIXct(d10m$Time, format="%m/%d/%y %H:%M")
range(d$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d10m_2 <- subset(d10m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                   timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d10m_2$timestamp)

## ---------------------------
# VII. Read in Summer 2018 deployment: 11.5m 
dll.5m <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102501_07_03_08_21_2018_11.5m.csv"), 
                   header=T)

#   1. Fix timestamp - so it is no longer a character:
dll.5m$timestamp <- as.POSIXct(dll.5m$Time, format="%m/%d/%y %H:%M")
range(dll.5m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
dll.5m_2 <- subset(dll.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                     timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(dll.5m$timestamp)

## ---------------------------
# VIII. Read in Summer 2018 deployment: 6.5m 
d6.5m <- read.csv(paste0(inputDir,"2018_2019/RRB/1807_1808_deployment/102502_07_03_08_21_2018_6.4m.csv"), 
                  header=T)

#   1. Fix timestamp - so it is no longer a character:
d6.5m$timestamp <- as.POSIXct(d6.5m$Time, format="%m/%d/%y %H:%M")
range(d6.5m$timestamp)

#   2. Restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
d6.5m_2 <- subset(d6.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                    timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d6.5m_2$timestamp)

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
rbr_dat_exp <- subset(rbr_dat, select=c(Sensor, deployment, year, timestamp, Depth, Temperature))
summary(rbr_dat_exp)
# write.csv(rbr_dat_exp, paste0(outputDir,"Summer2018_RBR.csv")) # complied data file of all RBR temp sensors along buoy line 

## ---------------------------
# X. Final QA'QC for temperature

# 1. Flag temperature values:
rbr_dat_exp.Q=rbr_dat_exp%>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(deployment, Depth, timestamp)%>%
  group_by(deployment, Depth, hour)%>%
  mutate(mnT=rollapply(Temperature, width = 20, FUN = mean, fill=NA),
         sdT=rollapply(Temperature, width = 20, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  full_join(., rbr_dat_exp)%>%
  mutate(flag_temperature=ifelse((Temperature<loT&!is.na(loT))|(Temperature>hiT&!is.na(hiT)), 'o', 'n'))

# 2. Check to make sure all sensors + data are there
Summer2018_RBR <- qplot(timestamp, Temperature, data = rbr_dat_exp.Q, geom="point",
                        color = factor(Depth), shape= flag_temperature) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
  theme_classic() + facet_wrap(~flag_temperature)
#ggsave("Summer2018_RBR.pdf", Summer2018_RBR, scale = 2, width = 15, height = 5, units = c("cm"), dpi = 500)

# 3. Remove unwanted variables:
Summer2018_RBR <- subset(rbr_dat_exp.Q, select=c(Sensor, deployment, year, timestamp, Depth,
                                                 Temperature, flag_temperature))

# 4. change names
colnames(Summer2018_RBR)[1] = "sensor"
colnames(Summer2018_RBR)[5] = "depth"
colnames(Summer2018_RBR)[6] = "temperature"

# 5. Export and save data:
# write.csv(Summer2018_RBR, paste0(outputDir, "Summer2018_RBR.csv")) # complied data file of all DO sensors along buoy line

## ---------------------------
# VI. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   * No clear battery column in raw download overtime
#   * Salinity and original calibration settings seem a little off