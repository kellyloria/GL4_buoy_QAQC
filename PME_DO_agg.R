## ---------------------------
## QA'QC for 3 PME Dissolved Oxygen sensors
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
## I. Winter 2018 Deployment: 3m DO sensor 
#     1. Read in new raw data at depth (for 2018-2019): DO_214423_180823_190723_3m.TXT
do.3m <- read.delim(paste0(inputDir,"/2018_2019/DO/1808_1907_deployment/DO_214423_180823_190723_3m.TXT"), header=T, sep = ',')
names(do.3m)

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

## ---------------------------
## II. Winter 2018 Deployment: 7m DO sensor 
#   1. Read in new raw data at depth (for 2018-2019): DO_245673_180823_190723_7m.TXT
do.7m <- read.delim(paste0(inputDir,"/2018_2019/DO/1808_1907_deployment/DO_245673_180823_190723_7m.TXT"), header=T, sep = ',')

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

## ---------------------------
## III. Winter 2018 Deployment: 11m DO sensor 
#   1. Read in new raw data at depth (for 2018-2019): DO_245673_180823_190723_7m.TXT
do.11m <- read.delim(paste0(inputDir,"/2018_2019/DO/1808_1907_deployment/DO_248353_180823_190723_11m.TXT"), header=T, sep = ',')
names(do.11m)

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

## ---------------------------
## IV. Winter 2018 Deployment 
#   1. combine all QAQC winter 2018
PME_DO_winter18 <- rbind(do.3m, do.7m, do.11m)
summary(PME_DO_winter18)

## ---------------------------
## V. Summer19 Deployment:
#     Some intial deployment notes:
#       * We removed the buoy from GL4 on 2019-07-30 from ~9:30-11:00am 
#           to add the chlorophyll-a sensor (C7).
#       * Also one of the sensor's had a damaged membrane and so was not deployed 

# 3m DO sensor
#   1. Read in new raw data at depth (for 2018-2019): DO_214423_190725_190820_3m.TXT
do.3m <- read.delim(paste0(inputDir,"/2018_2019/DO/1907_1908_deployment/DO_214423_190725_190820_3m.TXT"), header=T, sep = ',')
names(do.3m)

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

## ---------------------------
## VI. Summer19 Deployment: 9m DO Sensor
#   1. Read in new raw data at depth (for 2018-2019): 
do.9m <- read.delim(paste0(inputDir,"/2018_2019/DO/1907_1908_deployment/DO_245673_190725_190820_9m.TXT"), header=T, sep = ',')
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

## ---------------------------
## VII. Winter 2018 + Summer 2019 Deployment

#   1. combine all summer 2019 data
PME_DO_summer19 <- rbind(do.3m_1, do.9m_1)
summary(PME_DO_summer19)

#   2. combine all summer 2019 and winter 2018 data
PME_DO_summer_agg <- rbind(PME_DO_winter18, PME_DO_summer19)
summary(PME_DO_summer_agg)

## ---------------------------
# VIII. QA'QC for temperature and DO

PME_DO_summer_agg.Q=PME_DO_summer_agg %>% 
  mutate(Temperature=ifelse(Temperature>35, NA, Temperature)) %>%
  mutate(hour=lubridate::hour(timestamp))%>%
  arrange(deployment, depth, timestamp)%>%
  group_by(deployment, depth, hour)%>% #this will get the nearest 15, but could be fewer if some are missing OR >35C, I think (?) the 35 are bogus so that is ok but you could
  mutate(mnT=rollapply(Temperature, width = 15, FUN = mean, fill=NA),           # also filter out the NAs and >35s if you wanted to always have 15 values in your rolling window after removing bad values
         sdT=rollapply(Temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  mutate(mnDO=rollapply(Dissolved.Oxygen, width = 15, FUN = mean, fill=NA),
         sdDO=rollapply(Dissolved.Oxygen, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loDO=mnDO- (3*sdDO), hiDO=mnDO+ (3*sdDO))%>%
  full_join(., PME_DO_summer_agg)%>% #then use case_when to sort the final flags
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
        Dissolved.Oxygen>hiDO&!is.na(hiDO) ~ 'o',
        Dissolved.Oxygen.Saturation>150 ~ 'q',
        TRUE ~ 'n')) %>%
  mutate(
    flag_Q=
      case_when( 
        Q<0.7 ~ 'q', TRUE ~ 'n')) %>%
  mutate(
    flag_battery=
      case_when( 
        Battery<1.5 ~ 'q', TRUE ~ 'n'))

p <- ggplot(PME_DO_summer_agg.Q, aes(x=timestamp, y=Dissolved.Oxygen, 
                                  colour =as.factor(flag_DO), shape= deployment)) +
  geom_point(alpha = 0.7)  +
  theme_classic() + facet_wrap(~flag_Q)

#   2. Add in year 
PME_DO_summer_agg.Q1 <- transform(PME_DO_summer_agg.Q,
                                year = as.numeric(format(timestamp, '%Y')))
names(PME_DO_summer_agg.Q1)

## ---------------------------
# VIII. Combine 2019 Summer Agg and 2018 Summer data 

#   1. Select for relevant parameters
PME_DO_summer19.Q <- subset(PME_DO_summer_agg.Q1, select=c(sensor, deployment, year, timestamp, depth,
                                                          Temperature, Dissolved.Oxygen, Dissolved.Oxygen.Saturation, 
                                                          Battery, Q, flag_temperature, flag_DO, flag_Q, flag_battery))
#   2. Change names
names((PME_DO_summer19.Q))
colnames(PME_DO_summer19.Q)[6] = "temperature"
colnames(PME_DO_summer19.Q)[7] = "DO"
colnames(PME_DO_summer19.Q)[8] = "DO_saturation"
colnames(PME_DO_summer19.Q)[9] = "battery"

#   3. Read in past year's data - here 2018 summer
old.datDO <- read.csv(paste0(inputDir,"/2018_2019/DO/1808_1907_deployment/Summer2018_PME_DO.csv"), header=T)

#   4. Fix timestamp - so it is no longer a character:
old.datDO$timestamp1 <- as.POSIXct(old.datDO$timestamp, format="%Y-%m-%d %H:%M:%OS")
range(old.datDO$timestamp1)

#   5.select for relevant parameters
old.datDO.Q <- subset(old.datDO, select=c(sensor, deployment, year, timestamp1, Depth,
                                          temperature, DO, DO_saturation, 
                                          battery, Q, flag_temperature, flag_DO, flag_Q, flag_battery))
#   6. Change names
names((old.datDO.Q))
colnames(old.datDO.Q)[4] = "timestamp"
colnames(old.datDO.Q)[5] = "depth"

PME_DO_summer_agg.QA1 <- rbind(old.datDO.Q, PME_DO_summer19.Q)
summary(PME_DO_summer_agg.QA1)

#   7. Plot and color by deployment:
p <- ggplot(PME_DO_summer_agg.QA1, aes(x=timestamp, y=(DO), colour =(depth), shape = flag_DO)) +
  geom_point(alpha = 0.5) + theme_classic()

#   8. Double chec for duplicated values:
PME_DO_summer_agg.QA1%>%select(deployment, timestamp, depth)%>%duplicated()%>%sum() # 6 dups

View(PME_DO_summer_agg.QA1%>%
       inner_join(
         PME_DO_summer_agg.QA1 %>%
           group_by(deployment, timestamp, depth) %>%
           summarize(ct=dplyr::n())%>% filter(ct>1)))

# Remove values:
PME_DO_summer_agg.QA2 = PME_DO_summer_agg.QA1 %>%
  distinct(deployment, timestamp, depth, .keep_all = TRUE)

PME_DO_summer_agg.QA2%>%select(deployment, timestamp, depth)%>%duplicated()%>%sum() 

#write.csv(PME_DO_summer_agg.QA2, paste0(outputDir,"Summer2019_PME_DO.csv")) # complied data file of all DO sensors along buoy line

## ---------------------------
# IX. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
##  * Back ground information for users:
#       For PME DO a Q value of < 0.7 is poor quality
#       Sensor manual: https://www.pme.com/product-installs/q-measurement-found-in-minidot