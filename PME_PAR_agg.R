## ---------------------------
## QA'QC for 1 photosynthetically active radiation (PAR) Sensor
##    ongoing space for aggregation of new deployment data
##
## Author: Kelly A. Loria
## Date Created: 2020-03-18
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
# I. Winter 2018 Deployment 

# 3m PAR sensor 
#   1. Read in new raw data at depth (for 2018-2019): PAR_695220_180823_190723_3m.TXT
PAR.3m <- read.delim(paste0(inputDir,"/2018_2019/PAR/1808_1908_deployment/PAR_695220_180823_190723_3m.TXT"), header=T, sep = ',')
names(PAR.3m)
summary(PAR.3m)

#   2. fix timestamp
PAR.3m$timestamp1 <- as.POSIXct(PAR.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(PAR.3m$timestamp1)

#   3. Restrict for date range
PAR.3m <- subset(PAR.3m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                   timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(PAR.3m$timestamp1)
# Check new summary to make sure dates are expected
summary(PAR.3m) 

#   4. Calculate an estimate of tilt:
PAR.3m$tilt.y <- (180/pi)*atan(PAR.3m$Acceleration.X/sqrt((PAR.3m$Acceleration.Y)^2 + (PAR.3m$Acceleration.Z)^2))
hist(PAR.3m$tilt.y)
summary(PAR.3m$tilt.y) 

qplot(timestamp1, PAR, data = PAR.3m, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   *Recap so far: positive tilt values might correspond to out of range values for 
#       Z anf Y accelarations, but not bad PAR values 

#   5. Remove out of negative PARR values:
PAR.3m <- subset(PAR.3m, PAR >= 0)
summary(PAR.3m)

PAR.3m$depth <- 3
PAR.3m$deployment <- "Winter2018"
PAR.3m$sensor <- 695220

## ---------------------------
# II. Summer 2019 Deployment
#   As a deployment note we removed the buoy from GL4 on 2019-07-30 
#   from 9:30-11:00 am to add the chlorophyll-a sensor (C7).

### 
# 9m PAR sensor
#   1. Read in new raw data at depth (for 2018-2019): PAR_995520_190725_190820_9m.TXT
PAR.9m <- read.delim(paste0(inputDir,"/2018_2019/PAR/1808_1908_deployment/PAR_995520_190725_190820_9m.TXT"), header=T, sep = ',')
names(PAR.9m)
summary(PAR.9m)

#   2. fix timestamp
PAR.9m$timestamp1 <- as.POSIXct(PAR.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(PAR.9m$timestamp1)

#   3. restrict for date range
PAR.9ma <- subset(PAR.9m,timestamp1 >= as.POSIXct('2019-07-25 13:00:00') & 
                    timestamp1 <= as.POSIXct('2019-07-30 00:00:00'))
range(PAR.9ma$timestamp1)
PAR.9mb <- subset(PAR.9m,timestamp1 >= as.POSIXct('2019-07-30 12:00:00') & 
                    timestamp1 <= as.POSIXct('2019-08-20 00:00:00'))
range(PAR.9m$timestamp1)
PAR.9mm_1 <- rbind(PAR.9ma, PAR.9mb)

#   4. Calculate an estimate of tilt:
PAR.9mm_1$tilt.y <- (180/pi)*atan(PAR.9mm_1$Acceleration.X/sqrt((PAR.9mm_1$Acceleration.Y)^2 + (PAR.9mm_1$Acceleration.Z)^2))
hist(PAR.9mm_1$tilt.y)
summary(PAR.9mm_1$tilt.y) 

# 5. restrict for negative values of PAR
PAR.9mm_1 <- subset(PAR.9mm_1, PAR >= 0)
summary(PAR.9mm_1)

#   10. add in column for depth, deployment and sensor 
PAR.9mm_1$depth <- 9
PAR.9mm_1$deployment <- "Summer2019"
PAR.9mm_1$sensor <- 695220


## ---------------------------
# III. Winter 2018 Deployment + Summer 2019 Deployment

#   1. Add winter 2018 to summer 2018
PME_PAR_agg19 <- rbind(PAR.9mm_1, PAR.3m)
summary(PME_PAR_agg19)

#   2. Add in year to Summer2019
PME_PAR_agg19Q <- transform(PME_PAR_agg19,
                            year = as.numeric(format(timestamp1, '%Y')))

## ---------------------------
# IV. QA'QC of PME_PAR_agg19Q

#   1. QA'QC accelerations start with temperature
PME_PAR_agg19.Q=PME_PAR_agg19Q%>%
  mutate(hour=lubridate::hour(timestamp1))%>%
  arrange(deployment, depth, timestamp1)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnT=rollapply(Temperature, width = 15, FUN = mean, fill=NA),
         sdT=rollapply(Temperature, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loT=mnT- (3*sdT), hiT=mnT+ (3*sdT))%>%
  full_join(., PME_PAR_agg19Q)%>%
  mutate(flagT=ifelse((Temperature<loT&!is.na(loT))|(Temperature>hiT&!is.na(hiT)), 'o', 'n'))

#   2. QA'QC Acceleration.Y
PME_PAR_agg19.Q1=PME_PAR_agg19.Q%>%
  arrange(deployment, depth, timestamp1)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnY=rollapply(Acceleration.Y, width = 15, FUN = mean, fill=NA),
         sdY=rollapply(Acceleration.Y, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loY=mnY- (3*sdY), hiY=mnY+ (3*sdY))%>%
  full_join(., PME_PAR_agg19.Q)%>%
  mutate(flagY=ifelse((Acceleration.Y<loY&!is.na(loY))|(Acceleration.Y>hiY&!is.na(hiY)), 'o', 'n'))

#   3. QA'QC Acceleration.Z
PME_PAR_agg19.Q2=PME_PAR_agg19.Q1%>%
  arrange(deployment, depth, timestamp1)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnZ=rollapply(Acceleration.Z, width = 15, FUN = mean, fill=NA),
         sdZ=rollapply(Acceleration.Z, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loZ=mnZ- (3*sdZ), hiZ=mnZ+ (3*sdZ))%>%
  full_join(., PME_PAR_agg19.Q1)%>%
  mutate(flagZ=ifelse((Acceleration.Z<loZ&!is.na(loZ))|(Acceleration.Z>hiZ&!is.na(hiZ)), 'o', 'n'))

#   4. QA'QC accelerations:X
PME_PAR_agg19.Q3=PME_PAR_agg19.Q2%>%
  arrange(deployment, depth, timestamp1)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnX=rollapply(Acceleration.X, width = 15, FUN = mean, fill=NA),
         sdX=rollapply(Acceleration.X, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loX=mnX- (3*sdX), hiX=mnX+ (3*sdX))%>%
  full_join(., PME_PAR_agg19.Q2)%>%
  mutate(flagX=ifelse((Acceleration.X<loX&!is.na(loX))|(Acceleration.X>hiX&!is.na(hiX)), 'o', 'n'))

#   10. QA'QC accelerations:tilt
PME_PAR_agg19.Q4=PME_PAR_agg19.Q3%>%
  arrange(deployment, depth, timestamp1)%>%
  group_by(deployment, depth, hour)%>%
  mutate(mnTy=rollapply(tilt.y, width = 15, FUN = mean, fill=NA),
         sdTy=rollapply(tilt.y, width = 15, FUN = sd, fill=NA)) %>%
  mutate(loTy=mnTy- (3*sdTy), hiTy=mnTy+ (3*sdTy))%>%
  full_join(., PME_PAR_agg19.Q3)%>%
  mutate(flag_tilt=ifelse((tilt.y<loTy&!is.na(loTy))|(tilt.y>hiTy&!is.na(hiTy)), 'o', 'n'))

qplot(timestamp1, tilt.y, data = PME_PAR_agg19.Q4, geom="point", color=flag_tilt) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

## ---------------------------
# V. Read in past year's data
old.datPAR <- read.csv(paste0(inputDir,"/2018_2019/PAR/1808_1908_deployment/Summer2018_PME_PAR.csv"), header=T)

#   Fix timestamp - so it is no longer a character:
old.datPAR$timestamp1 <- as.POSIXct(old.datPAR$timestamp, format= "%Y-%m-%d %H:%M")
range(old.datPAR$timestamp1)

## ---------------------------
# VI. PME_PAR_agg19 + old data (Summer 2018)
names(old.datPAR)
names(PME_PAR_agg19.Q4)

#   1. Select for relevant parameters
PME_PAR_agg19.Q5 <- subset(PME_PAR_agg19.Q4, select=c(sensor, deployment, year, timestamp1, depth,
                                                      Temperature, PAR, Acceleration.X, Acceleration.Y,
                                                      Acceleration.Z, tilt.y, Battery, flagT, flagX, 
                                                      flagY, flagZ, flag_tilt))


#   2. Select for relevant parameters
old.datPAR.Q <- subset(old.datPAR, select=c(Sensor, deployment, year, timestamp1, depth,
                                            Temperature, PAR, Acceleration.X, Acceleration.Y,
                                            Acceleration.Z, tilt.y, Battery, flagT, flagX, 
                                            flagY, flagZ, flag_tilt))
names(old.datPAR.Q)
colnames(old.datPAR.Q)[1] = "sensor"

# Join data:
PME_PAR_agg19.T <- rbind(old.datPAR.Q, PME_PAR_agg19.Q5)
summary(PME_PAR_agg19.T)

#   3. change names
colnames(PME_PAR_agg19.T)[4] = "timestamp"
colnames(PME_PAR_agg19.T)[6] = "temperature"
colnames(PME_PAR_agg19.T)[8] = "acceleration.X"
colnames(PME_PAR_agg19.T)[9] = "acceleration.Y"
colnames(PME_PAR_agg19.T)[10] = "acceleration.Z"
colnames(PME_PAR_agg19.T)[12] = "battery"



#   5.Plot and color by deployment, depth or flag:
p <- ggplot(PME_PAR_agg19.T, aes(x=timestamp, y=(PAR), colour =as.factor(flag_tilt))) +
  geom_point(alpha = 0.5) +
  theme_classic() + xlab("Time stamp") + ylab("PAR") 

#write.csv(PME_PAR_agg19.T, paste0(outputDir,"Summer2019_PME_PAR.csv")) 


## ---------------------------
# VI. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   * sensor manual: https://www.pme.com/wp-content/uploads/2017/06/miniPAR-logger-pme.pdf
