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
# I. Read in past year's data
old.datPAR <- read.csv(paste0(inputDir,"/2018_2019/PAR/1808_1908_deployment/Summer2018_PME_PAR.csv"), header=T)

#   Fix timestamp - so it is no longer a character:
old.datPAR$timestamp1 <- as.POSIXct(old.datPAR$timestamp, format= "%Y-%m-%d %H:%M")
range(old.datPAR$timestamp1)

## ---------------------------
# II. Winter 2018 Deployment 

###
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
PAR.3m$tilt <- atan(PAR.3m$Acceleration.X/PAR.3m$Acceleration.Y)
hist(PAR.3m$tilt)
summary(PAR.3m$tilt) # positive tilt values are likely out of range

#   5. Check out the data pre-flagging:
qplot(timestamp1, tilt, data = PAR.3m, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, Acceleration.Y, data = PAR.3m, geom="point")  +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, Acceleration.X, data = PAR.3m, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, Acceleration.Z, data = PAR.3m, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, PAR, data = PAR.3m, geom="point") +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   *Recap so far: positive tilt values might correspond to out of range values for 
#       Z anf Y accelarations, but not bad PAR values 

#   6. Remove out of negative PARR values:
PAR.3m <- subset(PAR.3m, PAR >= 0)
summary(PAR.3m)

###
# Flag outlier accelartions
#   7. QA'QC accelerations:Y 
A.Y_mean <- (mean(PAR.3m$Acceleration.Y)) 
A.Y_sd <- (sd(PAR.3m$Acceleration.Y))
# look for values 3 SD away from mean 
A.Y_cutoff <- (A.Y_sd*3)
#find outlier values 
A.Y_upL <- (A.Y_mean + A.Y_cutoff)
A.Y_lowL <- (A.Y_mean - A.Y_cutoff)
# Apply flag: flag_Y
PAR.3m$flag_Y[ PAR.3m$Acceleration.Y > 0.004438161 | PAR.3m$Acceleration.Y < -0.1098816 ] <- "o"
PAR.3m$flag_Y[ PAR.3m$Acceleration.Y <= 0.004438161 & PAR.3m$Acceleration.Y >= -0.1098816 ] <- "n"

qplot(timestamp1, Acceleration.Y, data = PAR.3m, geom="point", color=flag_Y) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   8. QA'QC accelerations:Z
A.Z_mean <- (mean(PAR.3m$Acceleration.Z)) # 0.1727163
A.Z_sd <- (sd(PAR.3m$Acceleration.Z)) #0.03084187
# look for values 3 SD away from mean 
A.Z_cutoff <- (A.Z_sd*3)
#find outlier values 
A.Z_upL <- (A.Z_mean + A.Z_cutoff)
A.Z_lowL <- (A.Z_mean - A.Z_cutoff)
# Apply flag: flag_Z
PAR.3m$flag_Z[ PAR.3m$Acceleration.Z > 0.2652419 | PAR.3m$Acceleration.Z < 0.0801907 ] <- "o"
PAR.3m$flag_Z[ PAR.3m$Acceleration.Z <= 0.2652419 & PAR.3m$Acceleration.Z >= -0.0801907 ] <- "n"

qplot(timestamp1, Acceleration.Z, data = PAR.3m, geom="point", color=flag_Z) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   9. QA'QC accelerations:X
A.X_mean <- (mean(PAR.3m$Acceleration.X))
A.X_sd <- (sd(PAR.3m$Acceleration.X)) 
# look for values 3 SD away from mean 
A.X_cutoff <- (A.X_sd*3)
#find outlier values 
A.X_upL <- (A.X_mean + A.X_cutoff)
A.X_lowL <- (A.X_mean - A.X_cutoff)
# Apply flag: flag_X
PAR.3m$flag_X[ PAR.3m$Acceleration.X > 1.148255 | PAR.3m$Acceleration.X < 1.122743 ] <- "o"
PAR.3m$flag_X[ PAR.3m$Acceleration.X <= 1.148255 & PAR.3m$Acceleration.X >=1.122743 ] <- "n"

qplot(timestamp1, Acceleration.X, data = PAR.3m, geom="point", color=flag_X) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   10. QA'QC accelerations:tilt
A.t_mean <- (mean(PAR.3m$tilt)) #-1.505361
A.t_sd <- (sd(PAR.3m$tilt)) #0.2321761
# look for values 3 SD away from mean 
A.t_cutoff <- (A.t_sd*3)
#find outlier values 
A.t_upL <- (A.t_mean + A.t_cutoff)
A.t_lowL <- (A.t_mean - A.t_cutoff)
# Apply flag: flag_T
PAR.3m$flag_T[ PAR.3m$tilt > -0.8088324 | PAR.3m$tilt < -2.201889 ] <- "o"
PAR.3m$flag_T[ PAR.3m$tilt <= -0.8088324 & PAR.3m$tilt >=-2.201889 ] <- "n"

qplot(timestamp1, tilt, data = PAR.3m, geom="point", color=flag_T) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   11. add in column for depth, deployment and sensor 
PAR.3m$depth <- 3
PAR.3m$deployment <- "Winter2018"
PAR.3m$sensor <- 695220

## ---------------------------
# III. previous PAR data + Winter2018 data

#    1. Add in year for Winter2018 data 
PME_PAR_winter18 <- transform(PAR.3m,
                             year = as.numeric(format(timestamp1, '%Y')))

#   2.select for relevant parameters from "old data"
old.datPAR <- subset(old.datPAR, select=c(Sensor, deployment, year, timestamp1, depth,
                                          Temperature, PAR, Acceleration.X, Acceleration.Y,
                                          Acceleration.Z, tilt, Battery, flag_X, 
                                          flag_Y, flag_Z, flag_T))
#   3.fix names in old data:
colnames(old.datPAR)[1] = "sensor"

#   4.select for relevant parameters from Winter2018 data
PME_PAR_winter18 <- subset(PME_PAR_winter18, select=c(sensor, deployment, year, timestamp1, depth,
                                                    Temperature, PAR, Acceleration.X, Acceleration.Y,
                                                    Acceleration.Z, tilt, Battery, flag_X, 
                                                    flag_Y, flag_Z, flag_T))

#   5. Add winter 2018 to summer 2018
PME_PAR_agg18 <- rbind(old.datPAR, PME_PAR_winter18)
summary(PME_PAR_agg18)

#   6. change names of combined data
colnames(PME_PAR_agg18)[4] = "timestamp"
colnames(PME_PAR_agg18)[6] = "temperature"
colnames(PME_PAR_agg18)[8] = "acceleration.X"
colnames(PME_PAR_agg18)[9] = "acceleration.Y"
colnames(PME_PAR_agg18)[10] = "acceleration.Z"
colnames(PME_PAR_agg18)[12] = "battery"


#   7.Double check data with a plot and color by deployment or flags:
p <- ggplot(PME_PAR_agg18, aes(x=timestamp, y=(PAR), colour =as.factor(flag_T))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  theme_classic() + xlab("Time stamp") + ylab("PAR") 

## ---------------------------
# IV. Summer 2019 Deployment
#   As a deployment note we removed the buoy from GL4 on 2019-07-30 
#   from 9:30-11:00 am to add the chlorophyll-a sensor (C7).

### 
# 9m PAR sensor
#   1. Read in new raw data at depth (for 2018-2019): PAR_995520_190725_190820_9m.TXT
PAR.9m <- read.delim(paste0(inputDir,"/2018_2019/PAR/1808_1908_deployment/PAR_995520_190725_190820_9m.TXT"), header=T, sep = ',')
names(PAR.9m)
summary(PAR.9m)

#   2. fix timestamp
PAR.9m$timestamp <- as.POSIXct(PAR.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(PAR.9m$timestamp)

#   3. restrict for date range
PAR.9ma <- subset(PAR.9m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                   timestamp <= as.POSIXct('2019-07-30 00:00:00'))
range(PAR.9ma$timestamp)
PAR.9mb <- subset(PAR.9m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                   timestamp <= as.POSIXct('2019-08-20 00:00:00'))
range(PAR.9m$timestamp)
PAR.9mm_1 <- rbind(PAR.9ma, PAR.9mb)

#   4. Calculate an estimate of tilt:
PAR.9mm_1$tilt <- atan(PAR.9mm_1$Acceleration.X/PAR.9mm_1$Acceleration.Y)
hist(PAR.9mm_1$tilt)
qplot(timestamp, Acceleration.Y, data = PAR.9mm_1, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# 5. restrict for negative values of PAR
PAR.9mm_1 <- subset(PAR.9mm_1, PAR >= 0)
summary(PAR.9mm_1)

###
# Flag outlier accelartions
#   6. QA'QC accelerations:X 
A.X_mean <- (mean(PAR.9mm_1$Acceleration.X))
A.X_sd <- (sd(PAR.9mm_1$Acceleration.X)) 
# look for values 3 SD away from mean 
A.X_cutoff <- (A.X_sd*3)
# find outlier values 
A.X_upL <- (A.X_mean + A.X_cutoff)
A.X_lowL <- (A.X_mean - A.X_cutoff)
# Apply flag: flag_X
PAR.9mm_1$flag_X[ PAR.9mm_1$Acceleration.X > 1.144522 | PAR.9mm_1$Acceleration.X < 1.111385 ] <- "o"
PAR.9mm_1$flag_X[ PAR.9mm_1$Acceleration.X <= 1.144522 & PAR.9mm_1$Acceleration.X >=1.111385 ] <- "n"

qplot(timestamp, Acceleration.X, data = PAR.9mm_1, geom="point", color=flag_X) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   7.QA'QC accelerations:Y 
A.Y_mean <- (mean(PAR.9mm_1$Acceleration.Y)) # -0.05272171
A.Y_sd <- (sd(PAR.9mm_1$Acceleration.Y)) #0.01905329
# look for values 3 SD away from mean 
A.Y_cutoff <- (A.Y_sd*3)
# find outlier values 
A.Y_upL <- (A.Y_mean + A.Y_cutoff)
A.Y_lowL <- (A.Y_mean - A.Y_cutoff)
# Apply flag: flag_Y
PAR.9mm_1$flag_Y[ PAR.9mm_1$Acceleration.Y > 0.0981075 | PAR.9mm_1$Acceleration.Y < -0.009688046 ] <- "o"
PAR.9mm_1$flag_Y[ PAR.9mm_1$Acceleration.Y <= 0.0981075 & PAR.9mm_1$Acceleration.Y >= -0.009688046 ] <- "n"

qplot(timestamp, Acceleration.Y, data = PAR.9mm_1, geom="point", color=flag_Y) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   8.QA'QC accelerations: Z
A.Z_mean <- (mean(PAR.9mm_1$Acceleration.Z)) 
A.Z_sd <- (sd(PAR.9mm_1$Acceleration.Z)) 
# look for values 3 SD away from mean 
A.Z_cutoff <- (A.Z_sd*3)
# find outlier values 
A.Z_upL <- (A.Z_mean + A.Z_cutoff)
A.Z_lowL <- (A.Z_mean - A.Z_cutoff)
# Apply flag: flag_Z
PAR.9mm_1$flag_Z[ PAR.9mm_1$Acceleration.Z > 0.3238031 | PAR.9mm_1$Acceleration.Z < 0.1336802 ] <- "o"
PAR.9mm_1$flag_Z[ PAR.9mm_1$Acceleration.Z <= 0.3238031 & PAR.9mm_1$Acceleration.Z >= 0.1336802 ] <- "n"

qplot(timestamp, Acceleration.Z, data = PAR.9mm_1, geom="point", color=flag_Z) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   9.QA'QC accelerations:tilt
A.t_mean <- (mean(PAR.9mm_1$tilt)) #-1.505361
A.t_sd <- (sd(PAR.9mm_1$tilt)) #0.2321761
# look for values 3 SD away from mean 
A.t_cutoff <- (A.t_sd*3)
#find outlier values 
A.t_upL <- (A.t_mean + A.t_cutoff)
A.t_lowL <- (A.t_mean - A.t_cutoff)
# Apply flag: flag_Y
PAR.9mm_1$flag_T[ PAR.9mm_1$tilt >  -1.430497 | PAR.9mm_1$tilt < -1.55657 ] <- "o"
PAR.9mm_1$flag_T[ PAR.9mm_1$tilt <=  -1.430497 & PAR.9mm_1$tilt >= -1.55657 ] <- "n"

qplot(timestamp, tilt, data = PAR.9mm_1, geom="point", color=flag_Y) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   10. add in column for depth, deployment and sensor 
PAR.9mm_1$depth <- 9
PAR.9mm_1$deployment <- "Summer2019"
PAR.9mm_1$sensor <- 695220


## ---------------------------
# V. All previous data + Summer 2019 Deployment

#   1. Add in year to Summer2019
PME_PAR_summer2019 <- transform(PAR.9mm_1,
                              year = as.numeric(format(timestamp, '%Y')))

#   2.select for relevant parameters
PME_PAR_summer2019 <- subset(PME_PAR_summer2019, select=c(sensor, deployment, year, timestamp, depth,
                                                      Temperature, PAR, Acceleration.X, Acceleration.Y,
                                                      Acceleration.Z, tilt, Battery, flag_X, 
                                                      flag_Y, flag_Z, flag_T))
#   3. change names
colnames(PME_PAR_summer2019)[6] = "temperature"
colnames(PME_PAR_summer2019)[8] = "acceleration.X"
colnames(PME_PAR_summer2019)[9] = "acceleration.Y"
colnames(PME_PAR_summer2019)[10] = "acceleration.Z"
colnames(PME_PAR_summer2019)[11] = "battery"

#   4. Add winter 2018 to summer 2018
PME_PAR_agg19 <- rbind(PME_PAR_agg18, PME_PAR_summer2019)
summary(PME_PAR_agg19)

#   5.Plot and color by deployment, depth or flag:
p <- ggplot(PME_PAR_agg19, aes(x=timestamp, y=(PAR), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  theme_classic() + xlab("Time stamp") + ylab("PAR") 

#write.csv(PME_PAR_agg19, paste0(outputDir,"Summer2019_PME_PAR.csv")) 

## ---------------------------
# VI. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   * sensor manual: https://www.pme.com/wp-content/uploads/2017/06/miniPAR-logger-pme.pdf

