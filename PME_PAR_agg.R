# QA'QC for 1 photosynthetically active radiation 
# Ongoing

library(ggplot2)
library(scales)
library(dplyr)

# Read in past year's data
old.datPAR <- read.csv("gl4.buoy.PMEPAR.data.csv", header=T)

# Fix timestamp - so it is no longer a character:
old.datPAR$timestamp1 <- as.POSIXct(old.datPAR$timestamp, format= "%Y-%m-%d %H:%M")
range(old.datPAR$timestamp1)

##############################
### Winter 2018 Deployment ###
##############################

### 3m PAR sensor ###
# 1. Read in new raw data at depth (for 2018-2019): PAR_695220_180823_190723_3m.TXT
PAR.3m <- read.delim("PAR_695220_180823_190723_3m.TXT", header=T, sep = ',')
names(PAR.3m)
summary(PAR.3m)

###
# 2. fix timestamp
PAR.3m$timestamp1 <- as.POSIXct(PAR.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(PAR.3m$timestamp1)

# restrict for date range
PAR.3m <- subset(PAR.3m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                     timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(PAR.3m$timestamp1)

#check new summary
summary(PAR.3m) 
# temp range: -0.332 to 10.176
# PAR range: -0.9001 to 1158.1989
# Acceleration.X range: 1.103 to 1.164
# Acceleration.Y range: -0.17600 to 0.03000
# Acceleration.Z range: -0.1040  to 0.2870

PAR.3m$tilt <- atan(PAR.3m$Acceleration.X/PAR.3m$Acceleration.Y)
hist(PAR.3m$tilt)
summary(PAR.3m$tilt) # positive tilt values are likely out of range

qplot(timestamp1, tilt, data = PAR.3m, geom="point", ylab = "tilt est.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, Acceleration.Y, data = PAR.3m, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, Acceleration.X, data = PAR.3m, geom="point", ylab = "X accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, Acceleration.Z, data = PAR.3m, geom="point", ylab = "Z accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# restrict Z and Y axis

# QA'QC accelerations:Y 
A.Y_mean <- (mean(PAR.3m$Acceleration.Y)) # -0.05272171
A.Y_sd <- (sd(PAR.3m$Acceleration.Y)) #0.01905329

# look for values 3 SD away from mean 
A.Y_cutoff <- (A.Y_sd*3)

#find outlier values 
A.Y_upL <- (A.Y_mean + A.Y_cutoff)
A.Y_lowL <- (A.Y_mean - A.Y_cutoff)

PAR.3m_y <- subset(PAR.3m, Acceleration.Y <= 0.004438161 & Acceleration.Y >= -0.1098816)
range(PAR.3m_y$Acceleration.Y)

qplot(timestamp1, Acceleration.Z, data = PAR.3m_y, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# QA'QC accelerations:Z
A.Z_mean <- (mean(PAR.3m_y$Acceleration.Z)) # 0.1727163
A.Z_sd <- (sd(PAR.3m_y$Acceleration.Z)) #0.03084187

# look for values 3 SD away from mean 
A.Z_cutoff <- (A.Z_sd*3)

#find outlier values 
A.Z_upL <- (A.Z_mean + A.Z_cutoff)
A.Z_lowL <- (A.Z_mean - A.Z_cutoff)

PAR.3m_yz <- subset(PAR.3m_y, Acceleration.Z <= 0.2652419 & Acceleration.Z >= 0.0801907)
range(PAR.3m_yz$Acceleration.Z)

qplot(timestamp1, Acceleration.Z, data = PAR.3m_yz, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 


# QA'QC accelerations:X
A.X_mean <- (mean(PAR.3m_yz$Acceleration.X)) # 1.135499
A.X_sd <- (sd(PAR.3m_yz$Acceleration.X)) #0.004251948

# look for values 3 SD away from mean 
A.X_cutoff <- (A.X_sd*3)

#find outlier values 
A.X_upL <- (A.X_mean + A.X_cutoff)
A.X_lowL <- (A.X_mean - A.X_cutoff)

PAR.3m_yzx <- subset(PAR.3m_yz, Acceleration.X <= 1.148255 & Acceleration.X >= 1.122743)
range(PAR.3m_yzx$Acceleration.Z)

qplot(timestamp1, Acceleration.Y, data = PAR.3m_yzx, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

summary(PAR.3m_yzx) 
# not great still, I'm going to remove oulier tilts

# QA'QC accelerations:tilt
A.t_mean <- (mean(PAR.3m_yzx$tilt)) #-1.505361
A.t_sd <- (sd(PAR.3m_yzx$tilt)) #0.2321761

# look for values 3 SD away from mean 
A.t_cutoff <- (A.t_sd*3)

#find outlier values 
A.t_upL <- (A.t_mean + A.t_cutoff)
A.t_lowL <- (A.t_mean - A.t_cutoff)

PAR.3m_yzxt <- subset(PAR.3m_yz, tilt <= -0.8088324 & Acceleration.X >= -2.201889)
range(PAR.3m_yzxt$tilt)

summary(PAR.3m_yzxt) 

qplot(timestamp1, PAR, data = PAR.3m, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, PAR, data = PAR.3m_yzxt, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

PAR.3m_yzxtp <- subset(PAR.3m_yzxt, PAR >= 0)
range(PAR.3m_yzxtp$PAR)

qplot(timestamp1, PAR, data = PAR.3m_yzxtp, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# 3. add in column for depth, deployment and sensor 
PAR.3m_yzxtp$depth <- 3
PAR.3m_yzxtp$deployment <- "Winter2018"
PAR.3m_yzxtp$sensor <- 695220


######################################################
########## ALL DO + Winter2018 DATA #################

###
# 1. Add in year 
PME_PAR_winter18 <- transform(PAR.3m_yzxtp,
                             year = as.numeric(format(timestamp1, '%Y')))

names(old.datPAR)
names(PME_PAR_winter18)
###
# 3.select for relevant parameters
old.datPAR <- subset(old.datPAR, select=c(sensor, deployment, year, timestamp1, depth,
                                                      temperature, PAR, acceleration.X, acceleration.Y,
                                                      acceleration.Z,
                                                      battery))
###
# 3.select for relevant parameters
PME_PAR_winter18 <- subset(PME_PAR_winter18, select=c(sensor, deployment, year, timestamp1, depth,
                                                    Temperature, PAR, Acceleration.X, Acceleration.Y,
                                                    Acceleration.Z,
                                                    Battery))
###
# 4. change names
colnames(PME_PAR_winter18)[6] = "temperature"
colnames(PME_PAR_winter18)[8] = "acceleration.X"
colnames(PME_PAR_winter18)[9] = "acceleration.Y"
colnames(PME_PAR_winter18)[10] = "acceleration.Z"
colnames(PME_PAR_winter18)[11] = "battery"

###
# 5. Add winter 2018 to summer 2018
PME_PAR_agg18 <- rbind(old.datPAR, PME_PAR_winter18)
summary(PME_PAR_agg18)

colnames(PME_PAR_agg18)[4] = "timestamp"

# Plot and facet by deployment:
p <- ggplot(PME_PAR_agg18, aes(x=timestamp, y=(PAR), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("PAR") 


##############################
### Summer 2019 Deployment ###

# As a deployment note we removed the buoy from GL4 on 2019-07-30 
# from 9:30-11:00 am to add the chlorophyll-a sensor (C7).

### 9m PAR sensor ###
# 1. Read in new raw data at depth (for 2018-2019): PAR_995520_190725_190820_9m.TXT
PAR.9m <- read.delim("PAR_995520_190725_190820_9m.TXT", header=T, sep = ',')
names(PAR.9m)
summary(PAR.9m)

###
# 2. fix timestamp
PAR.9m$timestamp <- as.POSIXct(PAR.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(PAR.9m$timestamp)

# restrict for date range
PAR.9ma <- subset(PAR.9m,timestamp >= as.POSIXct('2019-07-25 13:00:00') & 
                   timestamp <= as.POSIXct('2019-07-30 00:00:00'))
range(PAR.9ma$timestamp)
PAR.9mb <- subset(PAR.9m,timestamp >= as.POSIXct('2019-07-30 12:00:00') & 
                   timestamp <= as.POSIXct('2019-08-20 00:00:00'))
range(PAR.9m$timestamp)
PAR.9mm_1 <- rbind(PAR.9ma, PAR.9mb)

#check new summary
summary(PAR.9mm_1)
PAR.9mm_1$tilt <- atan(PAR.9mm_1$Acceleration.X/PAR.9mm_1$Acceleration.Y)
hist(PAR.9mm_1$tilt)
summary(PAR.9mm_1$tilt)

qplot(timestamp, Acceleration.Y, data = PAR.9mm_1, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# restrict Z and Y axis
# QA'QC accelerations:Y 
A.Y_mean <- (mean(PAR.9mm_1$Acceleration.Y)) # 0.04385893
A.Y_sd <- (sd(PAR.9mm_1$Acceleration.Y)) #0.01741817

# look for values 3 SD away from mean 
A.Y_cutoff <- (A.Y_sd*3)

#find outlier values 
A.Y_upL <- (A.Y_mean + A.Y_cutoff)
A.Y_lowL <- (A.Y_mean - A.Y_cutoff)

PAR.9m_y <- subset(PAR.9mm_1, Acceleration.Y <= 0.09611345 & Acceleration.Y >= -0.00839559)
range(PAR.9mm_y$Acceleration.Y)

qplot(timestamp, Acceleration.Y, data = PAR.9m_y, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# QA'QC accelerations:Z
A.Z_mean <- (mean(PAR.9m_y$Acceleration.Z)) # 0.1727163
A.Z_sd <- (sd(PAR.9m_y$Acceleration.Z)) #0.03084187

# look for values 3 SD away from mean 
A.Z_cutoff <- (A.Z_sd*3)

#find outlier values 
A.Z_upL <- (A.Z_mean + A.Z_cutoff)
A.Z_lowL <- (A.Z_mean - A.Z_cutoff)

PAR.9m_yz <- subset(PAR.9m_y, Acceleration.Z <= 0.3169431 & Acceleration.Z >= 0.1439831)
range(PAR.9m_yz$Acceleration.Z)

qplot(timestamp, Acceleration.Z, data = PAR.9m_yz, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 


# QA'QC accelerations:X
A.X_mean <- (mean(PAR.9m_yz$Acceleration.X)) # 1.135499
A.X_sd <- (sd(PAR.9m_yz$Acceleration.X)) #0.004251948

# look for values 3 SD away from mean 
A.X_cutoff <- (A.X_sd*3)

#find outlier values 
A.X_upL <- (A.X_mean + A.X_cutoff)
A.X_lowL <- (A.X_mean - A.X_cutoff)

PAR.9m_yzx <- subset(PAR.9m_yz, Acceleration.X <= 1.14178 & Acceleration.X >= 1.114702)


qplot(timestamp, Acceleration.X, data = PAR.9m_yzx, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

summary(PAR.9m_yzx) 
# not great still, I'm going to remove oulier tilts

# QA'QC accelerations:tilt
A.t_mean <- (mean(PAR.9m_yzx$tilt)) #-1.505361
A.t_sd <- (sd(PAR.9m_yzx$tilt)) #0.2321761

# look for values 3 SD away from mean 
A.t_cutoff <- (A.t_sd*3)

#find outlier values 
A.t_upL <- (A.t_mean + A.t_cutoff)
A.t_lowL <- (A.t_mean - A.t_cutoff)
PAR.9m_yzxt <- subset(PAR.9m_yzx, tilt <= 1.56134 & tilt >= 1.50478)
range(PAR.9m_yzxt$tilt)

summary(PAR.9m_yzxt) 

qplot(timestamp, PAR, data = PAR.9mm_1, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp, PAR, data = PAR.9m_yzxt, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

PAR.9m_yzxtp <- subset(PAR.9m_yzxt, PAR >= 0)
range(PAR.9m_yzxtp$PAR)

qplot(timestamp, PAR, data = PAR.9m_yzxtp, geom="point", ylab = "Y accel.") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# 3. add in column for depth, deployment and sensor 
PAR.9m_yzxtp$depth <- 9
PAR.9m_yzxtp$deployment <- "Summer2019"
PAR.9m_yzxtp$sensor <- 695220

######################################################
########## ALL DO + Winter2018 DATA #################

###
# 1. Add in year 
PME_PAR_summer2019 <- transform(PAR.9m_yzxtp,
                              year = as.numeric(format(timestamp, '%Y')))
names(PME_PAR_agg18)
names(PME_PAR_summer2019)

###
# 3.select for relevant parameters
PME_PAR_summer2019 <- subset(PME_PAR_summer2019, select=c(sensor, deployment, year, timestamp, depth,
                                                      Temperature, PAR, Acceleration.X, Acceleration.Y,
                                                      Acceleration.Z,
                                                      Battery))
###
# 4. change names
colnames(PME_PAR_summer2019)[6] = "temperature"
colnames(PME_PAR_summer2019)[8] = "acceleration.X"
colnames(PME_PAR_summer2019)[9] = "acceleration.Y"
colnames(PME_PAR_summer2019)[10] = "acceleration.Z"
colnames(PME_PAR_summer2019)[11] = "battery"

###
# 5. Add winter 2018 to summer 2018
PME_PAR_agg19 <- rbind(PME_PAR_agg18, PME_PAR_summer2019)
summary(PME_PAR_agg19)


# Plot and facet by deployment:
p <- ggplot(PME_PAR_agg19, aes(x=timestamp, y=(PAR), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") + ylab("PAR") 

#write.csv(PME_PAR_agg19, "Summer2019_PME_PAR.csv") # complied data file of all DO sensors along buoy line


