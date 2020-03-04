# QA'QC for 1 C7 sensor optically formatted to measure chlorophyll-a
# Summer 2018 deployment

library(ggplot2)
library(scales)
library(dplyr)
library(lme4)
library(lmerTest)

### For later ###
# read in chl-a extractions: water_qualityCHLA.csv
chla1819 <- read.csv("water_qualityCHLA.csv", header=T)
names(chla1819)
summary(chla1819)
chla1819$ndate <- as.Date(as.Date.character(chla1819$date, format="%m/%d/%y"))
range(chla1819$ndate)

##########################
# Read in past year's data
old.datC7 <- read.csv("gl4.buoy.PMEC7.data.csv", header=T)

# Fix timestamp - so it is no longer a character:
old.datC7$timestamp1 <- as.POSIXct(old.datC7$timestamp, format= "%Y-%m-%d %H:%M")
range(old.datC7$timestamp1)
summary(old.datC7)

qplot(timestamp1, C7_output, data = old.datC7, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, chlora, data = old.datC7, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

#QAQC old data with extraction values, need add date 
old.datC7 <- transform(old.datC7, ndate = as.Date(timestamp1))

sum18comp <- left_join(old.datC7, chla1819[c("ndate", "depth","chl_a")],
                  by = c("ndate" = "ndate"))
summary(sum18comp)
summary(chla1819)

p <- ggplot(sum18comp) +
  geom_point(aes(x=ndate, y=(C7_output)), color="#57a6ad", alpha = 0.2) +
  geom_point(aes(x=ndate, y=(chl_a)), shape = 17, color="#316326", alpha = 0.8)  +
  theme_classic() 

p <- ggplot(sum18comp, aes(x=chl_a, y=C7_output)) +
  geom_point(alpha = 0.2)  +
  stat_smooth(method ="lm") +
  theme_classic()  

# get a beta value
sum18comp.mod <- lmer(chl_a ~ C7_output + (1|depth.y), data=sum18comp)
summary(sum18comp.mod)

#Transfor output for chl-a estimate
old.datC7$chlora20 <- (old.datC7$chlora * 0.065541)

qplot(timestamp1, chlora20, data = old.datC7, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

summary(old.datC7)

##############################
### Winter 2018 Deployment ###

### 3m C7 sensor ###
# 1. Read in new raw data at depth (for 2018-2019): C7_240115_180823_190723_3m.TXT
C7.3m <- read.delim("C7_240115_180823_190723_3m.TXT", header=T, sep = ',')
names(C7.3m)
summary(C7.3m)

###
# 2. fix timestamp
C7.3m$timestamp1 <- as.POSIXct(C7.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(C7.3m$timestamp1)

# restrict for date range
C7.3m <- subset(C7.3m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(C7.3m$timestamp1)

qplot(timestamp1, Sensor, data = C7.3m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

summary(C7.3m)

#QAQC old data with extraction values, need add date 
C7.3m <- transform(C7.3m, ndate = as.Date(timestamp1))

win18comp <- left_join(C7.3m, chla1819[c("ndate", "depth","chl_a")],
                       by = c("ndate" = "ndate"))
summary(win18comp)
summary(chla1819)

p <- ggplot(win18comp) +
  geom_point(aes(x=ndate, y=(Sensor)), color="#57a6ad", alpha = 0.2) +
  geom_point(aes(x=ndate, y=(chl_a)), shape = 17, color="#316326", alpha = 0.8)  +
  theme_classic() 

# get a beta value
win18comp.mod <- lmer(chl_a ~ Sensor + (1|depth), data=win18comp)
summary(win18comp.mod)

#Transfor output for chl-a estimate
C7.3m$chlora20 <- (C7.3m$Sensor * 0.001158)

qplot(timestamp1, chlora20, data = C7.3m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

summary(C7.3m)

# add in column for depth, deployment and sensor 
C7.3m$depth <- 3
C7.3m$deployment <- "Winter2018"
C7.3m$sensor <- 240115

#######################################################
########## ALL DO + Winter2018 DATA #################

###
# 1. Add in year 
PME_C7_winter18 <- transform(C7.3m,
                              year = as.numeric(format(timestamp1, '%Y')))

names(old.datC7)

names(PME_C7_winter18)
###
# 3.select for relevant parameters
old.datC7_1 <- subset(old.datC7, select=c(sensor, deployment, year, timestamp1, depth,
                                          temperature, C7_output, gain, chlora20,
                                          battery))
###
# 3.select for relevant parameters
PME_C7_winter18_1 <- subset(PME_C7_winter18, select=c(sensor, deployment, year, timestamp1, depth,
                                                      Temperature, Sensor, Gain, chlora20,
                                                      Battery))
###
# 4. change names
colnames(PME_C7_winter18_1)[6] = "temperature"
colnames(PME_C7_winter18_1)[7] = "C7_output"
colnames(PME_C7_winter18_1)[8] = "gain"
colnames(PME_C7_winter18_1)[10] = "battery"

###
# 5. Add winter 2018 to summer 2018
PME_C7_agg18 <- rbind(old.datC7_1, PME_C7_winter18_1)
summary(PME_C7_agg18)

# Plot and facet by deployment:
p <- ggplot(PME_C7_agg18, aes(x=timestamp1, y=(chlora20), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") 


##############################
### Summer 2019 Deployment ###
C7.9m <- read.delim("C7_240115_190730_190820_9m.TXT", header=T, sep = ',')
names(C7.9m)
summary(C7.9m)

###
# 2. fix timestamp
C7.9m$timestamp1 <- as.POSIXct(C7.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(C7.9m$timestamp1)

# restrict for date range
C7.9m <- subset(C7.9m,timestamp1 >= as.POSIXct('2019-07-30 12:00:00') & 
                    timestamp1 <= as.POSIXct('2019-08-20 00:00:00'))

qplot(timestamp1, Sensor, data = C7.9m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

summary(C7.9m)

#QAQC old data with extraction values, need add date 
C7.9m <- transform(C7.9m, ndate = as.Date(timestamp1))

sum19comp <- left_join(C7.9m, chla1819[c("ndate", "depth","chl_a")],
                       by = c("ndate" = "ndate"))
summary(sum19comp)

p <- ggplot(sum19comp) +
  geom_point(aes(x=ndate, y=(Sensor)), color="#57a6ad", alpha = 0.2) +
  geom_point(aes(x=ndate, y=(chl_a)), shape = 17, color="#316326", alpha = 0.8)  +
  theme_classic() 

# get a beta value
sum19comp.mod <- lmer(chl_a ~ Sensor + (1|depth), data=sum19comp)
summary(sum19comp.mod)

#Transfor output for chl-a estimate
C7.9m$chlora20 <- (C7.9m$Sensor * 3.437e-03)

qplot(timestamp1, chlora20, data = C7.9m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

summary(C7.9m)

# add in column for depth, deployment and sensor 
C7.9m$depth <- 9
C7.9m$deployment <- "Summer2019"
C7.9m$sensor <- 240115

######################################################
########## ALL DO + Winter2018 DATA #################

###
# 1. Add in year 
PME_C7_summer2019 <- transform(C7.9m,
                                year = as.numeric(format(timestamp1, '%Y')))
names(PME_C7_summer2019)
names(PME_C7_summer2019)

# 3.select for relevant parameters
PME_C7_summer2019_1 <- subset(PME_C7_summer2019, select=c(sensor, deployment, year, timestamp1, depth,
                                                      Temperature, Sensor, Gain, chlora20,
                                                      Battery))
###
# 4. change names
colnames(PME_C7_summer2019_1)[6] = "temperature"
colnames(PME_C7_summer2019_1)[7] = "C7_output"
colnames(PME_C7_summer2019_1)[8] = "gain"
colnames(PME_C7_summer2019_1)[10] = "battery"

###
# 5. Add winter 2018 to summer 2018
PME_C7_agg19 <- rbind(PME_C7_agg18, PME_C7_summer2019_1)
summary(PME_C7_agg19)

# Plot and facet by deployment:
p <- ggplot(PME_C7_agg19, aes(x=timestamp1, y=(chlora20), colour =as.factor(depth))) +
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme_classic() + xlab("Time stamp") 

summary(PME_C7_agg19)

colnames(PME_C7_agg19)[4] = "timestamp"
colnames(PME_C7_agg19)[9] = "est.chl_a"

#write.csv(PME_C7_agg19, "Summer2019_PME_C7.csv") # complied data file of all DO sensors along buoy line

