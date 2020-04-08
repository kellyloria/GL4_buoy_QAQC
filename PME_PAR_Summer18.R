## ---------------------------
## QA'QC for 1 photosynthetically active radiation (PAR)
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
# I. Read in Summer 2018 deployment
d <- read.csv(paste0(inputDir,"/2018_2019/PAR/1807_1808_deployment/PAR_07_03_08_21_2018_9m.csv"), header=T)
names(d)
#   1. Fix timestamp - so it is no longer a character:
d$timestamp <- as.POSIXct(d$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d$timestamp)

#   2. Restrict for date range:
d2 <- subset(d,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d2$timestamp)

#   3. Calculate an estimate of tilt:
d2$tilt.y <- (180/pi)*atan(d2$Acceleration.X/sqrt((d2$Acceleration.Y)^2 + (d2$Acceleration.Z)^2))
hist(d2$tilt.y)

#   4. Restrict for negative values of PAR
d3 <- subset(d2, PAR >= 0)
summary(d3)

#   5. Add in column for depth, deployment and sensor 
d3$deployment <- "Summer2018"
d3$year <- 2018

###
# Flag outlier accelartions
#   6. QA'QC accelerations start with temperature
d3.Q=d3%>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnT=rollapply(Temperature, width = 15, FUN = mean, fill=NA),
         sdT=rollapply(Temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  full_join(., d3)%>%
  mutate(flagT=ifelse((Temperature<loT&!is.na(loT))|(Temperature>hiT&!is.na(hiT)), 'o', 'n'))

#   7. QA'QC Acceleration.Y
d3.Q1=d3.Q%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnY=rollapply(Acceleration.Y, width = 15, FUN = mean, fill=NA),
         sdY=rollapply(Acceleration.Y, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loY=mnY- (3*sdY), hiY=mnY+ (3*sdY))%>%
  full_join(., d3.Q)%>%
  mutate(flagY=ifelse((Acceleration.Y<loY&!is.na(loY))|(Acceleration.Y>hiY&!is.na(hiY)), 'o', 'n'))

#   8. QA'QC Acceleration.Z
d3.Q2=d3.Q1%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnZ=rollapply(Acceleration.Z, width = 15, FUN = mean, fill=NA),
         sdZ=rollapply(Acceleration.Z, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loZ=mnZ- (3*sdZ), hiZ=mnZ+ (3*sdZ))%>%
  full_join(., d3.Q1)%>%
  mutate(flagZ=ifelse((Acceleration.Z<loZ&!is.na(loZ))|(Acceleration.Z>hiZ&!is.na(hiZ)), 'o', 'n'))

#   9. QA'QC accelerations:X
d3.Q3=d3.Q2%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnX=rollapply(Acceleration.X, width = 15, FUN = mean, fill=NA),
         sdX=rollapply(Acceleration.X, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loX=mnX- (3*sdX), hiX=mnX+ (3*sdX))%>%
  full_join(., d3.Q2)%>%
  mutate(flagX=ifelse((Acceleration.X<loX&!is.na(loX))|(Acceleration.X>hiX&!is.na(hiX)), 'o', 'n'))

#   10. QA'QC accelerations:tilt
d3.Q4=d3.Q3%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnTy=rollapply(tilt.y, width = 15, FUN = mean, fill=NA),
         sdTy=rollapply(tilt.y, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loTy=mnTy- (3*sdTy), hiTy=mnTy+ (3*sdTy))%>%
  full_join(., d3.Q3)%>%
  mutate(flag_tilt=ifelse((tilt.y<loTy&!is.na(loTy))|(tilt.y>hiTy&!is.na(hiTy)), 'o', 'n'))

qplot(timestamp, Acceleration.Z, data = d3.Q4, geom="point", color=flagZ) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   11. Select for relevant parameters
PME_PAR_summer18 <- subset(d3.Q4, select=c(Sensor, deployment, year, timestamp, depth, Temperature,
                                       PAR, Acceleration.X, Acceleration.Y, Acceleration.Z, tilt.y, Battery, flagT, flagY, 
                                       flagZ, flagX, flag_tilt))
summary(PME_PAR_summer18)
#write.csv(PME_PAR_summer18, paste0(outputDir,"Summer2018_PME_PAR.csv")) # complied data file of all RBR temp sensors along buoy line

## ---------------------------
# II. End notes:
#   * NWT flgging codes:
#         n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   *sensor manual: https://www.pme.com/wp-content/uploads/2017/06/miniPAR-logger-pme.pdf

