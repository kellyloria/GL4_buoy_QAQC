## ---------------------------
## QA'QC for 1 C7 sensor optically formatted to measure chlorophyll-a
##    ongoing space for aggregation of new deployment data
##
## Author: Kelly A. Loria
## Date Created: 2020-03-19
## Email: kelly.loria@colorado.edu
##
## ---------------------------
## Load packages:
library(ggplot2)
library(dplyr)
library(lubridate)

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
# Important note:
#    Raw C7 output needs to be corrected for chlorophyll-a exctractions

## ---------------------------
# I. For C7 output corrections 
# read in chl-a extractions: water_qualityCHLA.csv
chla1819 <- read.csv(paste0(inputDir, "2018_2019/C7/1808_1907_deployment/water_qualityCHLA.csv"), header=T)
names(chla1819)

# 2. Fix date and timestamp - so it is no longer a character:
chla1819$ndate <- as.Date(as.Date.character(chla1819$date, format="%Y-%m-%d"))
range(chla1819$ndate)

chla1819$timestamp1 <- as.POSIXct(chla1819$timestamp, format= "%Y-%m-%d %H:%M:%OS")
range(chla1819$timestamp1)


## ---------------------------
# II. Summer 2018 deployment
old.datC7 <- read.csv(paste0(inputDir, "2018_2019/C7/1808_1907_deployment/gl4.buoy.PMEC7.data.csv"), header=T)

# 1. Fix timestamp - so it is no longer a character:
old.datC7$timestamp1 <- as.POSIXct(old.datC7$timestamp, format= "%Y-%m-%d %H:%M")
range(old.datC7$timestamp1)
summary(old.datC7)

# 2. Check data distribution through plots:
qplot(timestamp1, C7_output, data = old.datC7, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

qplot(timestamp1, chlora, data = old.datC7, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# 3. Compare C7 output with chl-a extraction values. 
#       Need to extract just dates from timestamps and then restrict for morining sampling
old.datC7 <- transform(old.datC7, ndate = as.Date(timestamp1))
old.datC7$time <- (strftime(old.datC7$timestamp1,"%H:%M:%OS"))
old.datC7.1 <- with(old.datC7, old.datC7[hour(timestamp1)>= 9 & hour(timestamp1) < 13 , ] )

# 4. Combine C7 output and chl-a extractions
sum18comp <- left_join(old.datC7.1, chla1819[c("ndate", "depth","chl_a")],
                  by = c("ndate" = "ndate"))
summary(sum18comp)

# 5. Plot the new combined data
p <- ggplot(sum18comp) +
  geom_point(aes(x=ndate, y=(C7_output)), color="#57a6ad", alpha = 0.2) +
  geom_point(aes(x=ndate, y=(chl_a)), shape = 17, color="#316326", alpha = 0.8)  +
  theme_classic() 

# 5b.Plot the relationship between chl-a values and C7 values
p <- ggplot(sum18comp, aes(x=chl_a, y=C7_output)) +
  geom_point(alpha = 0.2)  +
  stat_smooth(method ="lm") +
  theme_classic()  

# 6. Get a beta value for the relationship between chl-a values and C7 values
sum18comp.mod <- lmer(chl_a ~ C7_output + (1|depth.y), data=sum18comp)
summary(sum18comp.mod)
ranef(sum18comp.mod) # get intercept for random effect for same depth as sensor deployment 

# 7. Transfor output for chl-a beta estimate
old.datC7$chlora20 <- (old.datC7$chlora * 0.07877 + 6.86848)

qplot(timestamp1, chlora20, data = old.datC7, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# 8. The data pre-July 29st looks poor and should be flagged 
old.datC7$flag_RE[old.datC7$timestamp1 <= as.POSIXct('2018-07-29 00:00:00')] <- "q"
old.datC7$flag_RE[old.datC7$timestamp1 >= as.POSIXct('2018-07-29 00:00:00')] <- "n"

# 9. Check to see if data was appropriately flagged:
qplot(timestamp1, chlora20, data = old.datC7, geom="point", color=flag_RE) +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

## ---------------------------
# III. Winter 2018 Deployment:
#     C7 sensor @ 3m

# 1. Read in new raw data at depth (for 2018-2019): C7_240115_180823_190723_3m.TXT
C7.3m <- read.delim(paste0(inputDir, "2018_2019/C7/1907_1908_deployment/C7_240115_180823_190723_3m.TXT"), header=T, sep = ',')
names(C7.3m)
summary(C7.3m)

# 2. Fix timestamp
C7.3m$timestamp1 <- as.POSIXct(C7.3m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(C7.3m$timestamp1)

# 3. Restrict for date range of deployment:
C7.3m <- subset(C7.3m,timestamp1 >= as.POSIXct('2018-08-24 13:00:00') & 
                  timestamp1 <= as.POSIXct('2019-07-23 00:00:00'))
range(C7.3m$timestamp1)

# 4. Plot the data:
qplot(timestamp1, Sensor, data = C7.3m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# 5. Compare C7 output with chl-a extraction values. 
#       Need to extract just dates from timestamps and then restrict for morining sampling and
#       add in variable for depth
#C7.3m$depth <- 3
C7.3m <- transform(C7.3m, ndate = as.Date(timestamp1))
C7.3m$time <- (strftime(C7.3m$timestamp1,"%H:%M:%OS"))
C7.3m.1 <- with(C7.3m, C7.3m[hour(timestamp1)>= 9 & hour(timestamp1) < 13 , ] )


# 6. Combine C7 output and chl-a extractions
win18comp <- left_join(C7.3m.1, chla1819[c("ndate", "depth","chl_a")],
                       by = c("ndate" = "ndate"))
summary(win18comp)

# 7. Plot the combined data
p <- ggplot(win18comp) +
  geom_point(aes(x=ndate, y=(Sensor)), color="#57a6ad", alpha = 0.2) +
  geom_point(aes(x=ndate, y=(chl_a)), shape = 17, color="#316326", alpha = 0.8)  +
  theme_classic() 

# 7b.Plot the relationship between chl-a values and C7 values
p <- ggplot(win18comp, aes(x=chl_a, y=Sensor)) +
  geom_point(alpha = 0.2)  +
  stat_smooth(method ="lm") +
  theme_classic()  

# 8. Get a beta value for the relationship between chl-a values and C7 values
win18comp.mod <- glm(chl_a ~ Sensor, data=win18comp)
summary(win18comp.mod)
ranef(win18comp.mod) # get intercept for random effect for same depth as sensor deployment 


# 9. Transfor output for chl-a estimate
C7.3m$chlora20 <- ((C7.3m$Sensor * 0.018453) + 5.877161)

# 10. Plot transformed data
qplot(timestamp1, chlora20, data = C7.3m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

# 11. Add in column for depth, deployment and sensor 
C7.3m$depth <- 3
C7.3m$deployment <- "Winter2018"
C7.3m$sensor <- 240115

# 12. Add in variable for flag:
C7.3m$flag_RE[C7.3m$timestamp1 >= as.POSIXct('2018-08-24 13:00:00') ] <- "n"


## ---------------------------
# IV Combine Summer2018 + Winter2018 data

# 1. Add in column for year 
PME_C7_winter18 <- transform(C7.3m,
                              year = as.numeric(format(timestamp1, '%Y')))

# 2. Select for relevant parameters
old.datC7_1 <- subset(old.datC7, select=c(sensor, deployment, year, timestamp1, depth,
                                          temperature, C7_output, gain, chlora20,
                                          battery, flag_RE))

# 3. Select for relevant parameters
PME_C7_winter18_1 <- subset(PME_C7_winter18, select=c(sensor, deployment, year, timestamp1, depth,
                                                      Temperature, Sensor, Gain, chlora20,
                                                      Battery, flag_RE))
# 4. Change names of column to match data
colnames(PME_C7_winter18_1)[6] = "temperature"
colnames(PME_C7_winter18_1)[7] = "C7_output"
colnames(PME_C7_winter18_1)[8] = "gain"
colnames(PME_C7_winter18_1)[10] = "battery"

# 5. Add winter 2018 to summer 2018
PME_C7_agg18 <- rbind(old.datC7_1, PME_C7_winter18_1)
summary(PME_C7_agg18)

# 6. Plot and facet by deployment:
p <- ggplot(PME_C7_agg18, aes(x=timestamp1, y=(chlora20), colour =as.factor(flag_RE))) + # can swap color for deployment too 
  geom_point(alpha = 0.5) +
  #stat_smooth(method="lm", se=TRUE, formula=y ~ poly(x, 3, raw=TRUE), alpha=0.15) +
  theme_classic() + xlab("Time stamp") 


## ---------------------------
# V. Summer 2019 Deployment

# 1. Read in data:
C7.9m <- read.delim(paste0(inputDir, "2018_2019/C7/1907_1908_deployment/C7_240115_190730_190820_9m.TXT"), header=T, sep = ',')

# 2. Fix timestamp
C7.9m$timestamp1 <- as.POSIXct(C7.9m$Mountain.Standard.Time, format="%Y-%m-%d %H:%M:%OS")
range(C7.9m$timestamp1)

# 3. Restrict for date range
C7.9m <- subset(C7.9m,timestamp1 >= as.POSIXct('2019-07-30 12:00:00') & 
                    timestamp1 <= as.POSIXct('2019-08-20 00:00:00'))

# 4. Plot the data:
qplot(timestamp1, Sensor, data = C7.9m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))

# 5. Compare C7 output with chl-a extraction values. 
#       Need to extract just dates from timestamps and then restrict for morining sampling
C7.9m <- transform(C7.9m, ndate = as.Date(timestamp1))
C7.9m$time <- (strftime(C7.9m$timestamp1,"%H:%M:%OS"))
C7.9m.1 <- with(C7.9m, C7.9m[hour(timestamp1)>= 9 & hour(timestamp1) < 13 , ] )

# 6. Combine C7 output and chl-a extractions
sum19comp <- left_join(C7.9m.1, chla1819[c("ndate", "depth","chl_a")],
                       by = c("ndate" = "ndate"))

# 7. Plot the combined data
p <- ggplot(sum19comp) +
  geom_point(aes(x=ndate, y=(Sensor)), color="#57a6ad", alpha = 0.2) +
  geom_point(aes(x=ndate, y=(chl_a)), shape = 17, color="#316326", alpha = 0.8)  +
  theme_classic() 

p <- ggplot(sum19comp, aes(x=chl_a, y=Sensor)) +
  geom_point(alpha = 0.2)  +
  stat_smooth(method ="lm") +
  theme_classic()  

# 8. Get a beta value for the relationship between chl-a values and C7 values
sum19comp.mod <- lmer(chl_a ~ Sensor + (1|depth), data=sum19comp)
summary(sum19comp.mod)

# 9. Transfor output for chl-a estimate
C7.9m$chlora20 <- (C7.9m$Sensor * 0.011536 + 0.131639)

# 10. Plot transformed data
qplot(timestamp1, chlora20, data = C7.9m, geom="point") +
  #scale_x_datetime(date_breaks = "504 hour", labels = date_format("%b %d")) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) 

summary(C7.9m)

# 11. Add in column for depth, deployment and sensor 
C7.9m$depth <- 9
C7.9m$deployment <- "Summer2019"
C7.9m$sensor <- 240115

# 12. Add in variable for flag:
C7.9m$flag_RE[C7.9m$timestamp1 >= as.POSIXct('2019-07-30 12:00:00') ] <- "n"

## ---------------------------
# VI. All C7 data + Summer2019 data

# 1. Add in variable for year: 
PME_C7_summer2019 <- transform(C7.9m,
                                year = as.numeric(format(timestamp1, '%Y')))
names(PME_C7_summer2019)
names(PME_C7_summer2019)

# 2. Select for relevant parameters
PME_C7_summer2019_1 <- subset(PME_C7_summer2019, select=c(sensor, deployment, year, timestamp1, depth,
                                                      Temperature, Sensor, Gain, chlora20,
                                                      Battery, flag_RE))

# 3. change names
colnames(PME_C7_summer2019_1)[6] = "temperature"
colnames(PME_C7_summer2019_1)[7] = "C7_output"
colnames(PME_C7_summer2019_1)[8] = "gain"
colnames(PME_C7_summer2019_1)[10] = "battery"

# 4. Add winter 2018 to summer 2018
PME_C7_agg19 <- rbind(PME_C7_agg18, PME_C7_summer2019_1)
summary(PME_C7_agg19)

# 5. Plot and facet by deployment:
p <- ggplot(PME_C7_agg19, aes(x=timestamp1, y=(est.chl_a), colour =as.factor(depth))) +
  geom_point(alpha = 0.5)  +
  theme_classic() + xlab("Time stamp") 

# 6. Fix column names
colnames(PME_C7_agg19)[4] = "timestamp"
colnames(PME_C7_agg19)[9] = "est.chl_a"

# 7. Export and save data:
# write.csv(PME_C7_agg19, paste0(outputDir, "Summer2019_PME_C7.csv")) # complied data file of all DO sensors along buoy line


## ---------------------------
# VII. End notes:
#   * NWT flgging codes:
#       n=no flag; m=missing; q=questionable; e=estimated; o=outlier
#
#   * Back ground information for users:
#       link to product mannual: https://www.turnerdesigns.com/cyclops-7f-submersible-fluorometer?lightbox=dataItem-jd6b16b81
#       And here: https://www.pme.com/wp-content/uploads/2014/07/Manual1.pdf


