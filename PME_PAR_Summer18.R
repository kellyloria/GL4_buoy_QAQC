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

## ---------------------------
# I. Read in Summer 2018 deployment
d <- read.csv("PAR_07_03_08_21_2018_corrected.csv", header=T)
names(d)
#   1. Fix timestamp - so it is no longer a character:
d$timestamp <- as.POSIXct(d$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d$timestamp)

#   2. Restrict for date range:
d2 <- subset(d,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d2$timestamp)

#   3. Calculate an estimate of tilt:
d2$tilt <- atan(d2$Acceleration.X/d2$Acceleration.Y)
hist(d2$tilt) 
summary(d2)

#   4. Restrict for negative values of PAR
d3 <- subset(d2, PAR >= 0)
summary(d3)

###
# Flag outlier accelartions
#   5. QA'QC accelerations start with Y
A.Y_mean <- (mean(d3$Acceleration.Y)) 
A.Y_sd <- (sd(d3$Acceleration.Y))
# look for values 3 SD away from mean 
A.Y_cutoff <- (A.Y_sd*3)
#find outlier values 
A.Y_upL <- (A.Y_mean + A.Y_cutoff)
A.Y_lowL <- (A.Y_mean - A.Y_cutoff)
# Apply flag: flag_Y
d3$flag_Y[ d3$Acceleration.Y > -0.01585714 | d3$Acceleration.Y < -0.1599153 ] <- "o"
d3$flag_Y[ d3$Acceleration.Y <= -0.01585714 & d3$Acceleration.Y >= -0.1599153 ] <- "n"

qplot(timestamp, Acceleration.Y, data = d3, geom="point", color=flag_Y) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   6. QA'QC accelerations:Z
A.Z_mean <- (mean(d3$Acceleration.Z)) # 0.1727163
A.Z_sd <- (sd(d3$Acceleration.Z)) #0.03084187
# look for values 3 SD away from mean 
A.Z_cutoff <- (A.Z_sd*3)
#find outlier values 
A.Z_upL <- (A.Z_mean + A.Z_cutoff)
A.Z_lowL <- (A.Z_mean - A.Z_cutoff)
# Apply flag: flag_Z
d3$flag_Z[ d3$Acceleration.Z > 0.3004781 | d3$Acceleration.Z < 0.09147239 ] <- "o"
d3$flag_Z[ d3$Acceleration.Z <= 0.3004781 & d3$Acceleration.Z >= 0.09147239 ] <- "n"

qplot(timestamp, Acceleration.Z, data = d3, geom="point", color=flag_Z) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   7. QA'QC accelerations:X
A.X_mean <- (mean(d3$Acceleration.X))
A.X_sd <- (sd(d3$Acceleration.X)) 
# look for values 3 SD away from mean 
A.X_cutoff <- (A.X_sd*3)
#find outlier values 
A.X_upL <- (A.X_mean + A.X_cutoff)
A.X_lowL <- (A.X_mean - A.X_cutoff)
# Apply flag: flag_X
d3$flag_X[ d3$Acceleration.X > 1.157674 | d3$Acceleration.X < 1.111905 ] <- "o"
d3$flag_X[ d3$Acceleration.X <= 1.157674 & d3$Acceleration.X >=1.111905 ] <- "n"

qplot(timestamp, Acceleration.X, data = d3, geom="point", color=flag_X) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   8. QA'QC accelerations:tilt
A.t_mean <- (mean(d3$tilt))
A.t_sd <- (sd(d3$tilt))
# look for values 3 SD away from mean 
A.t_cutoff <- (A.t_sd*3)
#find outlier values 
A.t_upL <- (A.t_mean + A.t_cutoff)
A.t_lowL <- (A.t_mean - A.t_cutoff)
# Apply flag: flag_T
d3$flag_T[ d3$tilt > -1.430497 | d3$tilt < -1.55657 ] <- "o"
d3$flag_T[ d3$tilt <= -1.430497 & d3$tilt >=-1.55657 ] <- "n"

qplot(timestamp, tilt, data = d3, geom="point", color=flag_T) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#   9. Add in column for depth, deployment and sensor 
d3$deployment <- "Summer2018"
d3$year <- 2018

#   10. Select for relevant parameters
PME_PAR_summer18 <- subset(d3, select=c(Sensor, deployment, year, timestamp, depth, Temperature,
                                       PAR, Acceleration.X, Acceleration.Y, Acceleration.Z, tilt, Battery, flag_Y, 
                                       flag_Z, flag_X, flag_T))
summary(PME_PAR_summer18)
#write.csv(PME_PAR_summer18, "Summer2018_PME_PAR.csv") # complied data file of all RBR temp sensors along buoy line

## ---------------------------
# II. End notes:
#   * NWT flgging codes:
#         n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   *sensor manual: https://www.pme.com/wp-content/uploads/2017/06/miniPAR-logger-pme.pdf

