# QA'QC for RBR TSolo data

library(ggplot2)
library(scales)
library(dplyr)


### RBR sensors ###


### 0.4m ###
m0.4 <- read.csv("102495_07_03_08_21_2018_0.4m.csv", header=T)
names(m0.4)
summary(m0.4)

m0.4$timestamp <- as.POSIXct(m0.4$Time, format="%m/%d/%y %H:%M")
range(m0.4$timestamp)

# restrict for date range "2018-07-03 13:00:00 MDT" to "2018-08-21 00:00:00 MDT"
m0.4.2 <- subset(m0.4,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(m0.4.2$timestamp)

# Temp
hist(m0.4.2$Temperature) # some high temps
temp_mean <- (mean(m0.4.2$Temperature)) #11.53164
temp_sd <- (sd(m0.4.2$Temperature)) #1.104067

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff) #14.84384
temp_lowL <- (temp_mean - temp_cutoff) #8.219444  

m0.4.d3 <- subset(m0.4.2, Temperature <= 14.84384 & Temperature >= 8.219444, 
             select=c(Sensor: timestamp))

range(na.omit(m0.4.d3$Temperature))
hist(m0.4.d3$Temperature)

write.csv(m0.4.d3, "102495_07_03_08_21_2018_0.4m_QA.csv")


### 1.5m ####
d1.5m <- read.csv("102496_07_03_08_21_2018_1.5m.csv", header=T)

d1.5m$timestamp <- as.POSIXct(d1.5m$Time, format="%m/%d/%y %H:%M")
range(d1.5m$timestamp)

# restrict for date range
d1.5m.2 <- subset(d1.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d1.5m.2$timestamp)

# Temp
hist(d1.5m.2$Temperature)
temp_mean <- (mean(d1.5m.2$Temperature)) #11.44062
temp_sd <- (sd(d1.5m.2$Temperature)) #1.06195

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature)

d1.5m.3 <- subset(d1.5m.2, Temperature <= 14.62647 & Temperature >= 8.254769, 
             select=c(Sensor: timestamp))

range((d1.5m.3$Temperature))
hist(d1.5m.3$Temperature)

write.csv(d3, "102496_07_03_08_21_2018_1.5m_QA.csv")

### 3m ####
d3m <- read.csv("102497_07_03_08_21_2018_3m.csv", header=T)

d3m$timestamp <- as.POSIXct(d3m$Time, format="%m/%d/%y %H:%M")
range(d3m$timestamp)

# restrict for date range
d3m_2 <- subset(d3m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d3m_2$timestamp)

# Temp
hist(d3m_2$Temperature)
plot(d3m_2$timestamp)

temp_mean <- (mean(d3m_2$Temperature)) #11.21614
temp_sd <- (sd(d3m_2$Temperature)) #1.130445

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature)

d3m_3 <- subset(d3m_2, Temperature <= 14.60748 & Temperature >= 7.824803, 
             select=c(Sensor: timestamp))

range((d3m_3$Temperature))
hist(d3m_3$Temperature)

write.csv(d3, "102497_07_03_08_21_2018_3m_QA.csv")


### 5.1m ####
d5.1m <- read.csv("102498_07_03_08_21_2018_5.1m.csv", header=T)

d5.1m$timestamp <- as.POSIXct(d5.1m$Time, format="%m/%d/%y %H:%M")
range(d5.1m$timestamp)

# restrict for date range
d5.1m_2 <- subset(d5.1m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d5.1m_2$timestamp)

qplot(timestamp, Temperature, data = d5.1m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# Temp
hist(d5.1m_2$Temperature)
plot(d5.1m_2$timestamp)

temp_mean <- (mean(d5.1m_2$Temperature)) #10.66656
temp_sd <- (sd(d5.1m_2$Temperature)) #1.177188

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature)
d5.1m_3 <- subset(d5.1m_2, Temperature <= 14.19813 & Temperature >= 7.134998, 
             select=c(Sensor: timestamp))

range((d5.1m_3$Temperature))
hist(d5.1m_3$Temperature)

write.csv(d3, "102498_07_03_08_21_2018_5.1m_QA.csv")

### 7.5m ####
d7.5m <- read.csv("102499_07_03_08_21_2018_7.5m.csv", header=T)

d7.5m$timestamp <- as.POSIXct(d7.5m$Time, format="%m/%d/%y %H:%M")
range(d$timestamp)

# restrict for date range
d7.5m_2 <- subset(d7.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d7.5m_2$timestamp)

qplot(timestamp, Temperature, data = d7.5m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


# Temp
hist(d7.5m_2$Temperature)
plot(d7.5m_2$timestamp)

temp_mean <- (mean(d7.5m_2$Temperature)) #9.73467
temp_sd <- (sd(d7.5m_2$Temperature)) #1.367305

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature)
d7.5m_3 <- subset(d7.5m_2, Temperature <= 13.84943 & Temperature >= 5.624177, 
             select=c(Sensor: timestamp))

range((d7.5m_3$Temperature))
hist(d7.5m_3$Temperature) 

write.csv(d3, "102499_07_03_08_21_2018_7.5m_QA.csv")

### 10m ####
d10m <- read.csv("102500_07_03_08_21_2018_10m.csv", header=T)

d10m$timestamp <- as.POSIXct(d10m$Time, format="%m/%d/%y %H:%M")
range(d$timestamp)

# restrict for date range
d10m_2 <- subset(d10m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d10m_2$timestamp)

qplot(timestamp, Temperature, data = d10m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


# Temp
hist(d10m_2$Temperature)
plot(d10m_2$timestamp)

temp_mean <- (mean(d10m_2$Temperature)) #7.933123
temp_sd <- (sd(d10m_2$Temperature)) #1.367189

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d10m_2$Temperature)
d10m_3 <- subset(d10m_2, Temperature <= 12.03469 & Temperature >= 3.831558, 
             select=c(Sensor: timestamp))

range(d10m_3$Temperature)
hist(d10m_3$Temperature) 

write.csv(d3, "102500_07_03_08_21_2018_10m_QA.csv")


### 11.5m ####
dll.5m <- read.csv("102501_07_03_08_21_2018_11.5m.csv", header=T)

dll.5m$timestamp <- as.POSIXct(dll.5m$Time, format="%m/%d/%y %H:%M")
range(dll.5m$timestamp)

# restrict for date range
dll.5m_2 <- subset(dll.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(dll.5m$timestamp)

qplot(timestamp, Temperature, data = dll.5m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


# Temp
hist(dll.5m_2$Temperature)
plot(dll.5m_2$timestamp)

temp_mean <- (mean(dll.5m_2$Temperature)) #7.229486
temp_sd <- (sd(dll.5m_2$Temperature)) #1.072973

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature)
dll.5m_3 <- subset(dll.5m_2, Temperature <= 10.4484 & Temperature >= 4.010568, 
             select=c(Sensor: timestamp))

range(dll.5m_3$Temperature)
hist(dll.5m_3$Temperature) 

write.csv(d3, "102501_07_03_08_21_2018_11.5m_QA.csv")

### 6.5m ####
d6.5m <- read.csv("102502_07_03_08_21_2018_6.4m.csv", header=T)

d6.5m$timestamp <- as.POSIXct(d6.5m$Time, format="%m/%d/%y %H:%M")
range(d6.5m$timestamp)

# restrict for date range
d6.5m_2 <- subset(d6.5m,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d6.5m_2$timestamp)

qplot(timestamp, Temperature, data = d6.5m_2, geom="point", ylab = "Temperature [C]") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


# Temp
hist(d6.5m_2$Temperature)
plot(d6.5m_2$timestamp)

temp_mean <- (mean(d6.5m_2$Temperature)) #10.30964
temp_sd <- (sd(d6.5m_2$Temperature)) #1.270001

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*3)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature)
d6.5m_3 <- subset(d6.5m_2, Temperature <= 14.11964 & Temperature >= 6.499636, 
             select=c(Sensor: timestamp))

range(na.omit(d6.5m_3$Temperature))
hist(d6.5m_3$Temperature) 

write.csv(d3, "102502_07_03_08_21_2018_6.5m_QA.csv")

##### combine all sensors into 1 .cvs file
# d1.5m.3
# d3m_3
# d5.1m_3
# d7.5m_3
# d10m_3
# dll.5m_3
# d6.5m_3

rbr_dat <- rbind(m0.4.d3, d1.5m.3, d3m_3, d5.1m_3, d6.5m_3, d7.5m_3, d10m_3, dll.5m_3)
summary(rbr_dat)

# fix sensor serial number 12500 to 102500
rbr_dat$Sensor[rbr_dat$Sensor == 12500] <- 102500
summary(rbr_dat)

# add in year of deployment
rbr_dat$year <- 2018

# add in deployement 
rbr_dat$deployment <- "Summer2018"

# fix column order
rbr_dat_exp <- subset(rbr_dat, select=c(Sensor, deployment, year, timestamp, Depth, Temperature))
summary(rbr_dat_exp)

write.csv(rbr_dat_exp, "Summer2018_RBR.csv") # complied data file of all RBR temp sensors along buoy line 


## issues for next deployment ##
# no clear battery column in raw download overtime
# salinity and original calibration settings seem a little off


# check to make sure all sensors + data are there
names(rbr_dat)
Summer2018_RBR <- qplot(timestamp, Temperature, data = rbr_dat, geom="point", ylab = "Temperature [C]",
                        color = factor(Depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + theme_classic()

ggsave("Summer2018_RBR.pdf", Summer2018_RBR, scale = 2, width = 15, height = 5, units = c("cm"), dpi = 500)



