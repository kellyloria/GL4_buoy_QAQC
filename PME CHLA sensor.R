# QA'QC for 1 C7 sensor optically formatted to measure chlorophyll-a
# Summer 2018 deployment

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

################################################
# Need to check C7 range with chlorophyll-a extractions #

# format date for extractions and sensor data
chla <- read.csv("CHLA_GL4.csv", header=T)
chla$date1 <- as.Date(chla$date, format="%m/%d/%y")

d3$date1 <- as.Date(d3$timestamp, format="%Y-%m-%d")
range(d3$date1)

# select for relevant parameters
#restrict sensor measurements to days with samples collected
d2.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-03 10:00:00') & 
               timestamp <= as.POSIXct('2018-07-03 14:00:00'))
range(d2.temp$timestamp)

d1.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-10 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-10 14:00:00'))
range(d1.temp$timestamp)

d3.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-17 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-17 14:00:00'))
range(d3.temp$timestamp)

d4.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-24 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-24 14:00:00'))
range(d4.temp$timestamp)

d5.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-31 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-31 14:00:00'))
range(d5.temp$timestamp)

d6.temp <- subset(d3,timestamp >= as.POSIXct('2018-08-07 9:00:00') & 
                    timestamp <= as.POSIXct('2018-08-07 14:00:00'))
range(d6.temp$timestamp)

newc7<- rbind(d2.temp, d3.temp, d4.temp, d5.temp, d6.temp, d1.temp)

c7comp <- subset(newc7, select=c(date1, Sensor))
c7comp$label <- "C7 measurement"
chlcomp <- subset(chla, select=c(date1, Sensor))
chlcomp$label <- "extraction value"

newdat <- rbind(c7comp, chlcomp)
names(chla)
names(d3)



chlor_comp_plot2 <- ggplot(newdat, aes(x = date1, y = Sensor)) + 
  geom_point(aes(color = label), size = 1.75) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +   
  labs(title = "Chlora at 9 meters from C7 in situ and filter extractions",
       caption = "(Data logged every 30min July to August 2018)",
       x= "Time stamp", y= "Chlor-a ppb") +
  theme_minimal()

ggsave("Summer2018_chlor_comp_plot2.pdf", chlor_comp_plot2, scale = 2, width = 12, height = 5, units = c("cm"), dpi = 500)


# calibration factor between the extracted values and the C7
chla_comp <- lm(Sensor ~ label, data= newdat)
summary(chla_comp)
range(chla$Sensor)
newdat$Sensor

# transform c7 logger data to better match chlor-a extraction

d3$chlora <- (d3$Sensor -38.6688)
# change all negatives to 0 
d3$chlora <- ifelse(d3$chlora < 0, 0, d3$chlora)

range(d3$Sensor)
range(d3$chlora)
range(chla$Sensor)

# look at transformed values 
c7comp <- subset(d3, select=c(date1, Sensor, chlora))
c7comp$label <- "C7 measurement"
chlcomp <- subset(chla, select=c(date1, Sensor))
chlcomp$chlora <- (chlcomp$Sensor)
chlcomp$label <- "extraction value"

newdat_tran <- rbind(c7comp, chlcomp)


chlor_comp_plot.transformed <- ggplot(newdat_tran, aes(x = date1, y = chlora)) + 
  geom_point(aes(color = label), size = 1.75) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +   
  labs(title = "Chlora at 9 meters from C7 in situ and filter extractions",
       caption = "(Data logged every 30min July to August 2018)",
       x= "Time stamp", y= "Chlor-a ppb") +
  theme_minimal()

ggsave("Summer2018_chlor_comp_transformed.pdf", chlor_comp_plot.transformed, scale = 2, width = 12, height = 5, units = c("cm"), dpi = 500)


# select final parameters for C7 data
names(d3)
d3$deployment <- "Summer2018"
d3$year <- 2018
d3$C7_output <- (d3$Sensor)

# select for relevant parameters
PME_CHL_dat_exp <- subset(d3, select=c(sensor, deployment, year, timestamp, depth,
                                              Temperature, C7_output, chlora, Gain, Battery))
summary(PME_CHL_dat_exp)

write.csv(PME_CHL_dat_exp, "Summer2018_PME_CHLA.csv") # complied data file of all RBR temp sensors along buoy line

# final check plot
chlacheck <- qplot(timestamp, chlora, data = PME_CHL_dat_exp, geom="point", ylab = "Chl-a (ug/L)") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + theme_bw()

ggsave("Summer2018_chlor_QAQC.pdf", chlacheck, scale = 2, width = 12, height = 5, units = c("cm"), dpi = 500)








################################################
# SECONDARY CHECK
# maybe need to split to compare July 3th - July 26 and July 30th to August 21st

# Need to check C7 range with chlorophyll-a extractions #
# format date for extractions and sensor data
chla <- read.csv("CHLA_GL4.csv", header=T)
chla$date1 <- as.Date(chla$date, format="%m/%d/%y")

d3$date1 <- as.Date(d3$timestamp, format="%Y-%m-%d")
range(d3$date1)

# select for relevant parameters
#restrict sensor measurements to days with samples collected
d2.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-03 10:00:00') & 
                    timestamp <= as.POSIXct('2018-07-03 14:00:00'))
range(d2.temp$timestamp)

d1.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-10 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-10 14:00:00'))
range(d1.temp$timestamp)

d3.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-17 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-17 14:00:00'))
range(d3.temp$timestamp)

d4.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-24 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-24 14:00:00'))
range(d4.temp$timestamp)

d5.temp <- subset(d3,timestamp >= as.POSIXct('2018-07-31 9:00:00') & 
                    timestamp <= as.POSIXct('2018-07-31 14:00:00'))
range(d5.temp$timestamp)

d6.temp <- subset(d3,timestamp >= as.POSIXct('2018-08-07 9:00:00') & 
                    timestamp <= as.POSIXct('2018-08-07 14:00:00'))
range(d6.temp$timestamp)

chlearly.temp <- subset(chla, date1 >= as.Date('2018-07-03') & 
                          date1 <= as.Date('2018-07-17'))
range(chlearly.temp$date1)

chllate.temp <- subset(chla, date1 >= as.Date('2018-07-18') & 
                          date1 <= as.Date('2018-08-22'))
range(chllate.temp$date1)
chlmid.temp <- subset(chla, date1 == as.Date('2018-07-24'))
range(chlmid.temp$date1)

newc7<- rbind(d1.temp, d2.temp, d3.temp)
newc7_late<- rbind(d5.temp, d6.temp)
newc7_mid<- rbind(d4.temp)

c7comp_mid <- subset(newc7_mid, select=c(date1, Sensor))
c7comp_mid$label <- "C7 measurement"

c7comp_early <- subset(newc7, select=c(date1, Sensor))
c7comp_early$label <- "C7 measurement"

c7comp_late <- subset(newc7_late, select=c(date1, Sensor))
c7comp_late$label <- "C7 measurement"

chlmid <- subset(chlmid.temp, select=c(date1, Sensor))
chlmid$label <- "extraction value"

chllate <- subset(chllate.temp, select=c(date1, Sensor))
chllate$label <- "extraction value"


chlcomp <- subset(chlearly.temp, select=c(date1, Sensor))
chlcomp$label <- "extraction value"

chlcomp <- subset(chla, select=c(date1, Sensor))
chlcomp$label <- "extraction value"

newdat <- rbind(c7comp_early, chlcomp)
newdat_late <- rbind(c7comp_late, chllate)
newdat_mid <- rbind(c7comp_mid, chlmid)
names(chla)
names(d3)



chlor_comp_plot2 <- ggplot(newdat, aes(x = date1, y = Sensor)) + 
  geom_point(aes(color = label), size = 1.75) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +   
  labs(title = "Chlora at 9 meters from C7 in situ and filter extractions",
       caption = "(Data logged every 30min July to August 2018)",
       x= "Time stamp", y= "Chlor-a ppb") +
  theme_minimal()

#ggsave("Summer2018_chlor_comp_plot2.pdf", chlor_comp_plot2, scale = 2, width = 12, height = 5, units = c("cm"), dpi = 500)


# calibration factor between the extracted values and the C7
chla_comp <- lm(Sensor ~ label, data= newdat)
summary(chla_comp)
range(chla$Sensor)
newdat$Sensor


chla_comp <- lm(Sensor ~ label, data= newdat)
summary(chla_comp)
range(chla$Sensor)
newdat_late$Sensor

# transform c7 logger data to better match chlor-a extraction
# early July3-26th 
-37.805 + 2.991

# mid 
-39.7016 + 2.3136

# late July29-Augth 
-46.2457 + 1.6411
 
# subset QA'QC "d3" to fit within early July3-26th
d_early <- subset(d3,timestamp >= as.POSIXct('2018-07-03 13:00:00') & 
                    timestamp <= as.POSIXct('2018-07-28 00:00:00'))
range(d_early$timestamp)

d_early$chlora <- (d_early$Sensor -34.814)
# change all negatives to 0 
d_early$chlora <- ifelse(d_early$chlora < 0, 0, d_early$chlora)
d_early$label <- "Early (sensor-37.805)"

range(d_early$Sensor)
range(d_early$chlora)
d_earlyg <- subset(d_early, select=c(date1, chlora, timestamp, label))


# subset QA'QC "d3" to fit within mid July3-26th
d_mid <- subset(d3,timestamp >= as.POSIXct('2018-07-19 1:00:00') & 
                    timestamp <= as.POSIXct('2018-07-28 00:00:00'))
range(d_mid$timestamp)

d_mid$chlora <- (d_mid$Sensor-39.7016)
# change all negatives to 0 
d_mid$chlora <- ifelse(d_mid$chlora < 0, 0, d_mid$chlora)
d_mid$label <- "Mid (sensor-39.7016)"

range(d_mid$chlora)
range(d_early$chlora)
d_midg <- subset(d_mid, select=c(date1, chlora, timestamp, label))

# subset QA'QC "d3" to fit within late 29th- Aug 21st
d_late <- subset(d3,timestamp >= as.POSIXct('2018-07-19 00:00:00') & 
                    timestamp <= as.POSIXct('2018-08-21 00:00:00'))
range(d_late$timestamp)

d_late$chlora <- (d_late$Sensor-44.6046)
# change all negatives to 0 
d_late$chlora <- ifelse(d_late$chlora < 0, 0, d_late$chlora)
d_late$label <- "Late (sensor-46.1011)"
range(d_late$chlora)

new_tran_datg <- rbind(d_earlyg, d_midg, d_lateg, )
names(new_tran_dat)
d_lateg <- subset(d_late, select=c(date1, chlora, timestamp, label))


chlor_comp_plot.tran_EL <- ggplot(new_tran_datg, aes(x = timestamp, y = chlora)) + 
  geom_point(aes(color = label), size = 1.75) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#66CC99")) +   
  labs(title = "Chlora at 9 meters from C7 in situ transformed between early and late season based off extraction values",
       caption = "(Data logged every 30min July to August 2018)",
       x= "Time stamp", y= "Chlor-a ppb") + 
  geom_vline(xintercept = as.POSIXct('2018-07-19 00:00:00'),
             color = "grey45", size=0.5) + theme_minimal() + 
  geom_vline(xintercept = as.POSIXct('2018-07-28 00:00:00'),
             color = "grey45", size=0.5) + theme_minimal()


ggsave("Summer2018_chlor_early_late_transformations.pdf", chlor_comp_plot.tran_EL, scale = 2, width = 16, height = 6, units = c("cm"), dpi = 500)


range(chla$Sensor)

# look at transformed values 
c7comp <- subset(d3, select=c(date1, Sensor, chlora))
c7comp$label <- "C7 measurement"
chlcomp <- subset(chla, select=c(date1, Sensor))
chlcomp$chlora <- (chlcomp$Sensor)
chlcomp$label <- "extraction value"

newdat_tran <- rbind(c7comp, chlcomp)


chlor_comp_plot.transformed <- ggplot(newdat_tran, aes(x = date1, y = chlora)) + 
  geom_point(aes(color = label), size = 1.75) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +   
  labs(title = "Chlora at 9 meters from C7 in situ and filter extractions",
       caption = "(Data logged every 30min July to August 2018)",
       x= "Time stamp", y= "Chlor-a ppb") +
  theme_minimal()

ggsave("Summer2018_chlor_comp_transformed.pdf", chlor_comp_plot.transformed, scale = 2, width = 12, height = 5, units = c("cm"), dpi = 500)


# select final parameters for C7 data
names(d3)
d3$deployment <- "Summer2018"
d3$year <- 2018
d3$C7_output <- (d3$Sensor)

# select for relevant parameters
PME_CHL_dat_exp <- subset(d3, select=c(sensor, deployment, year, timestamp, depth,
                                       Temperature, C7_output, chlora, Gain, Battery))
summary(PME_CHL_dat_exp)

write.csv(PME_CHL_dat_exp, "Summer2018_PME_CHLA.csv") # complied data file of all RBR temp sensors along buoy line

# final check plot
chlacheck <- qplot(timestamp, chlora, data = new_tran_dat, geom="point", ylab = "Chl-a (ug/L)") +
  scale_x_datetime(date_breaks = "72 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + theme_bw()

#ggsave("Summer2018_chlor_QAQC.pdf", chlacheck, scale = 2, width = 12, height = 5, units = c("cm"), dpi = 500)

# the early and mid season values look unreliable 


