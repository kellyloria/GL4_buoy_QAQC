#Script to process Nel's stream gauge data
#Created by SCE on 4/9/19
#library (magrittr)
#library (dplyr)
library (tidyverse)
library(ggplot2)
# place you save your data in and pull your data from 
setwd("~/Documents/Niwot LTER 2017-2019/GL4 Sensor/Buoy Data/Buoy_QAQC")
getwd()

# read in your data as a data frame
Sdl <- read.csv("Sdl2018_KAL.csv", header=T)
summary(Sdl)

#plot
ggplot(Sdl, aes(x = PT.Level..cm., y = Measured.Stage..cm.)) + 
    geom_point()  +
    labs(x= "PT level", y= "Measured Stage (cm)") + 
    geom_smooth(method="lm")

#get the equation for that line
sdl.mod <- lm(PT.Level..cm. ~ Measured.Stage..cm., data = Sdl)
summary(sdl.mod)
summary(sdl.mod)$r.squared # for r squared value, here =0.9024338


# read in your data as a data frame
Mrt <- read.csv("Mart2018_KAL.csv", header=T)
summary(Mrt)

#plot
ggplot(Mrt, aes(x = PT.Level..cm., y = Measured.Stage..cm.)) + 
  geom_point()  +
  labs(x= "PT level", y= "Measured Stage (cm)") + 
  geom_smooth(method="lm")

#get the equation for that line
mrt.mod <- lm(PT.Level..cm. ~ Measured.Stage..cm., data = Mrt)
summary(mrt.mod)
summary(mrt.mod)$r.squared # for r squared value, here =0.9024338

# Next steps for sensor data 
