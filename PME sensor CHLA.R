####################
# # # Chlor-a # # #
####################
library(ggplot2)
library(scales)
library(dplyr)


d <- read.csv("C7_07_03_08_21_2018_corrected.csv", header=T)
names(d)

d$timestamp <- as.POSIXct(d$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d$timestamp)

# restrict for date range
d2 <- subset(d,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d2$timestamp)

# Temp
hist(d2$Temperature)
qplot(timestamp, Temperature, data = d2, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

temp_mean <- (mean(d2$Temperature)) #8.729836
temp_sd <- (sd(d2$Temperature)) #1.488609

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*4)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature) # currently no temps outside of range 

hist(d2)
qplot(timestamp, Sensor, data = d2, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

chlor_mean <- (mean(d2$Sensor)) #51.62525
chlor_sd <- (sd(d2$Sensor)) #35.51271

# look for values 3 SD away from mean 
chlor_cutoff <- (chlor_sd*3)

#find outlier values 
chlor_upL <- (chlor_mean + chlor_cutoff)
chlor_lowL <- (chlor_mean - chlor_cutoff) # lower limit not realistic so used 3

range(d2$Sensor)
names(d2)

d3 <- subset(d2, Sensor <= 158.1634 & Sensor >= 3, 
             select=c(sensor: timestamp))

range((d3$Sensor))
hist(d3$Sensor)

qplot(timestamp, Sensor, data = d3, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

range(d3$Battery)
range(d3$Gain) # gain = 10 = 10 volts = 0 to 0.075

write.csv(d3, "Chlor_QA_1.csv")




d <- read.csv("PAR_07_03_08_21_2018_corrected.csv", header=T)
names(d)

d$timestamp <- as.POSIXct(d$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d$timestamp)

# restrict for date range
d2 <- subset(d,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d2$timestamp)

hist(d2$Temperature)
qplot(timestamp, Temperature, data = d2, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

temp_mean <- (mean(d2$Temperature)) #0.01148371
temp_sd <- (sd(d2$Temperature)) #1.997242

# look for values 4 SD away from mean 
temp_cutoff <- (temp_sd*4)

#find outlier values 
temp_upL <- (temp_mean + temp_cutoff)
temp_lowL <- (temp_mean - temp_cutoff)

range(d2$Temperature)

hist(d2$PAR)
qplot(timestamp, PAR, data = d2, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

write.csv(d2, "PAR_QA.csv")

#### Last Check ##########
d <- read.csv("Sensors_combined_07_03_08_21_2018_QA.csv", header=T)
names(d)

d$timestamp <- as.POSIXct(d$timestamp, format="%Y-%m-%d %H:%M:%OS")
range(d$timestamp)

qplot(timestamp, temperature, data = d, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# restrict for date range
d2 <- subset(d,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
               timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d2$timestamp)

qplot(timestamp, temperature, data = d2, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


d$Dissolved.Oxygen
qplot(timestamp, chlor_a, data = d2, geom="point", ylab = "Chlor_a [ppb]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, PAR, data = d2, geom="point", ylab = "PAR", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, dissolved_oxygen, data = d2, geom="point", ylab = "DO [mg/L]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


write.csv(d2, "Sensors_combined_07_03_08_21_2018_QA_1.csv")

d <- read.csv("DO_combined_QA.csv", header=T)
names(d)

d$timestamp <- as.POSIXct(d$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(d$timestamp)

qplot(timestamp, Dissolved.Oxygen, data = d, geom="point", ylab = "DO [mg/L]", color = factor(Depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))


#### Last Check ##########
d <- read.csv("Sensors_combined_07_03_08_21_2018_QA_1.csv", header=T)
names(d)
head(d)
d$source

d$timestamp <- as.POSIXct(d$timestamp, format="%Y-%m-%d %H:%M:%OS")
d$date
range(d$timestamp)


png("GL4_Buoy_temp.png",  
    width = 6.25,
    height = 4.25,
    units = "in",
    res = 1200,
    pointsize = 0.25 )

qplot(timestamp, temperature, data = d, geom="point", ylab = "Temperature [C]", color = factor(depth)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

dev.off()

qplot(timestamp, PAR, data = d, geom="point", ylab = "PAR", color = factor(depth), shape=factor(source)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

d$chlor_a
qplot(timestamp, chlor_a, data = d, geom="point", ylab = "PAR", color = factor(depth), shape=factor(source)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, dissolved_oxygen, data = d, geom="point", ylab = "DO [mg/L]", color = factor(depth), shape=factor(source)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

write.csv(d, "Sensors_comp_07_03_08_21_2018_time.csv")

d <- read.csv("Sensors_comp_07_03_08_21_2018_time.csv")
d$timestamp
d$timestamp <- as.POSIXct(d$timestamp, format="%m/%d/%y %H:%M")
range(d$timestamp)

qplot(timestamp, temperature, data = d, geom="point", ylab = "Temperature [C]", color = factor(depth), shape=factor(source)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, PAR, data = d, geom="point", ylab = "PAR", color = factor(depth), shape=factor(source)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

qplot(timestamp, dissolved_oxygen, data = d, geom="point", ylab = "DO [mg/L]", color = factor(depth), shape=factor(source)) +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

