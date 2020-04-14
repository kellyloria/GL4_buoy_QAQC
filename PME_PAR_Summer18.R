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
d3.Q=d3 %>% 
  mutate(Temperature=ifelse(Temperature>35, NA, Temperature)) %>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>% #this will get the nearest 15, but could be fewer if some are missing OR >35C, I think (?) the 35 are bogus so that is ok but you could
  mutate(mnT=rollapply(Temperature, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdT=rollapply(Temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  mutate(mnP=rollapply(PAR, width = 15, FUN = mean, fill=NA),
         sdP=rollapply(PAR, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loP=mnP- (3*sdP), hiP=mnP+ (3*sdP))%>%
  mutate(mnAx=rollapply(Acceleration.X, width = 15, FUN = mean, fill=NA),
         sdAx=rollapply(Acceleration.X, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loAx=mnAx- (3*sdAx), hiAx=mnAx+ (3*sdAx))%>%
  mutate(mnAy=rollapply(Acceleration.Y, width = 15, FUN = mean, fill=NA),
         sdAy=rollapply(Acceleration.Y, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loAy=mnAy- (3*sdAy), hiAy=mnAy+ (3*sdAy))%>%
  mutate(mnAz=rollapply(Acceleration.Z, width = 15, FUN = mean, fill=NA),
         sdAz=rollapply(Acceleration.Z, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loAz=mnAz- (3*sdAz), hiAz=mnAz+ (3*sdAz))%>%
  mutate(mnAt=rollapply(tilt.y, width = 15, FUN = mean, fill=NA),
         sdAt=rollapply(tilt.y, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loAt=mnAt- (3*sdAt), hiAt=mnAt+ (3*sdAt))%>%
  full_join(., d3)%>% #then use case_when to sort the final flags
  mutate(
    flag_temperature=
      case_when( #may as well add the m in here since your metadata days that flag was used
        is.na(Temperature) ~ 'm',
        Temperature>35 ~ 'q',
        Temperature<loT&!is.na(loT) ~ 'o',
        Temperature>hiT&!is.na(hiT) ~ 'o',
        Temperature<0 ~ 'q', TRUE ~ 'n')) %>%
  mutate(
    flag_PAR=
      case_when( #may as well add the m in here since your metadata days that flag was used
        PAR<loP&!is.na(loP) ~ 'o',
        PAR>hiP&!is.na(hiP) ~ 'o', TRUE ~ 'n')) %>%
  mutate(
    flag_Ax=
      case_when( 
        Acceleration.X<loAx&!is.na(loAx) ~ 'o',
        Acceleration.X>hiAx&!is.na(hiAx) ~ 'o', TRUE ~ 'n')) %>%   
  mutate(
    flag_Ay=
      case_when( 
        Acceleration.Y<loAy&!is.na(loAy) ~ 'o',
        Acceleration.Y>hiAy&!is.na(hiAy) ~ 'o', TRUE ~ 'n')) %>%
  mutate(
    flag_Az=
      case_when( 
        Acceleration.Z<loAz&!is.na(loAz) ~ 'o',
        Acceleration.Z>hiAz&!is.na(hiAz) ~ 'o', TRUE ~ 'n')) %>%
  mutate(
    flag_At=
      case_when( 
        tilt.y<loAt&!is.na(loAt) ~ 'o',
        tilt.y>hiAt&!is.na(hiAt) ~ 'o', TRUE ~ 'n')) %>%
  mutate(
    flag_battery=
      case_when( 
        Battery<1.5 ~ 'q', TRUE ~ 'n'))

p <- ggplot(d3.Q, aes(x=timestamp, y=(PAR), 
                                  colour =as.factor(flag_At), shape= deployment)) +
  geom_point(alpha = 0.7)  +
  theme_classic() + facet_wrap(~flag_At)

#   7. Select for relevant parameters
PME_PAR_summer18 <- subset(d3.Q, select=c(Sensor, deployment, year, timestamp, depth, Temperature,
                                       PAR, Acceleration.X, Acceleration.Y, Acceleration.Z, tilt.y, Battery, 
                                       flag_temperature, flag_Ax, flag_Ay, flag_Az, flag_At, flag_battery))
summary(PME_PAR_summer18)
#write.csv(PME_PAR_summer18, paste0(outputDir,"Summer2018_PME_PAR.csv")) # complied data file of all RBR temp sensors along buoy line

## ---------------------------
# II. End notes:
#   * NWT flgging codes:
#         n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   *sensor manual: https://www.pme.com/wp-content/uploads/2017/06/miniPAR-logger-pme.pdf

