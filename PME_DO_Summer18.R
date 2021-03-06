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

## ---------------------------
# IV. Read combine all depths of Summer 2018 DO data
PME_DO_dat <- rbind(d1.5m, d9m_2, d11.5m_2)
summary(PME_DO_dat)

PME_DO_dat <- transform(PME_DO_dat,
                               year = as.numeric(format(timestamp, '%Y')))

PME_DO_dat$deployment <- "Summer2018"

## ---------------------------
# V. QA'QC for temperature and DO
#   4. Select for relevant parameters

PME_DO_dat.Q=PME_DO_dat %>% 
  mutate(Temperature=ifelse(Temperature>35, NA, Temperature)) %>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(deployment, Depth, timestamp)%>%
  group_by(deployment, Depth, hour)%>% #this will get the nearest 15, but could be fewer if some are missing OR >35C, I think (?) the 35 are bogus so that is ok but you could
  mutate(mnT=rollapply(Temperature, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdT=rollapply(Temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  mutate(mnDO=rollapply(Dissolved.Oxygen, width = 15, FUN = mean, fill=NA),
         sdDO=rollapply(Dissolved.Oxygen, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loDO=mnDO- (3*sdDO), hiDO=mnDO+ (3*sdDO))%>%
  full_join(., PME_DO_dat)%>% #then use case_when to sort the final flags
  mutate(
    flag_temperature=
      case_when( #may as well add the m in here since your metadata days that flag was used
        is.na(Temperature) ~ 'm',
        Temperature>35 ~ 'q',
        Temperature<loT&!is.na(loT) ~ 'o',
        Temperature>hiT&!is.na(hiT) ~ 'o',
        Temperature<0 ~ 'q', TRUE ~ 'n')) %>%
  mutate(
    flag_DO=
      case_when( #may as well add the m in here since your metadata days that flag was used
        Dissolved.Oxygen<loDO&!is.na(loDO) ~ 'o',
        Dissolved.Oxygen>hiDO&!is.na(hiDO) ~ 'o', TRUE ~ 'n')) %>%
  mutate(
    flag_Q=
      case_when( 
        Q<0.7 ~ 'q', TRUE ~ 'n')) %>%
  mutate(
    flag_battery=
      case_when( 
        Battery<1.5 ~ 'q', TRUE ~ 'n'))

#   3. Check out flagged values
p <- ggplot(PME_DO_dat.Q, aes(x=timestamp, y=(Dissolved.Oxygen), 
                              colour =as.factor(flag_temperature), shape= deployment)) +
  geom_point(alpha = 0.5)  +
  theme_classic() + facet_wrap(~flag_temperature)

#   4. Select for relevant parameters
PME_DO_dat_exp <- subset(PME_DO_dat.Q, select=c(sensor, deployment, year, timestamp, Depth,
                                              Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, 
                                              Battery, Q, flag_temperature, flag_DO, flag_Q, flag_battery))
summary(PME_DO_dat_exp)

#   5. Change names:
colnames(PME_DO_dat_exp)[6] = "temperature"
colnames(PME_DO_dat_exp)[7] = "DO"
colnames(PME_DO_dat_exp)[8] = "DO_saturation"
colnames(PME_DO_dat_exp)[9] = "battery"

#write.csv(PME_DO_dat_exp, paste0(outputDir,"Summer2018_PME_DO.csv")) # complied data file of all DO sensors along buoy line

## ---------------------------
# VI. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
##  * Back ground information for users:
#       For PME DO a Q value of < 0.7 is poor quality
#       Sensor manual: https://www.pme.com/product-installs/q-measurement-found-in-minidot

