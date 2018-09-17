#############################################
# The Fundamental Group
# Date Created: 6/15/18
# Last Updated: 9/14/18

# Joint work completed by:
# Michael E. Ramsey, University of Colorado, Boulder
# Eric Roberts, University of California, Merced
# Olivia Cannon, University of Minnesota, Twin Cities
# Ariel Bowman, University of Texas at Arlington
# Elizabeth Wicks, University of Washington
# Sheng Zhang, Purdue University

#############################################

# This is an r-sript that is used for computing average fluxes. In its current 
# state, it computes the average flux by day of the week (M,T,...,S), and also
# computes an average flux refined by weekday an hour (eg. M1, M2, ... M24, T1, ...)

####### Workspace Items #######
# DATA FRAMES
# stationFluxHours: The actual hourly flux for each station in 2017
# stationFluxAvgHour: Average hourly flux by weekday for each station in 2017
# stationFLuxAvgWeekday: Average daily flux by weekday for each station in 2017

#############################################

# Import necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(readr) 
library(data.table)

# Load the hourly flux data
stationFluxHours <- read.csv("Actual_Hourly_Flux_17.csv")

# Removes redundant X column
stationFluxHours$X <- NULL

# Remove holidays from data analysis
holidays = c("2016-5-8","2016-5-30" ,"2016-6-19",
              "2016-7-4" ,"2016-9-5" ,"2016-10-10",
              "2016-11-11","2016-11-24" ,"2016-11-25")
stationFluxHours <- stationFluxHours %>% 
  filter(!date %in% holidays) 

# Calculate average flux for each station by weekday
stationFluxAvgWeekday <- stationFluxHours %>%
  subset(select = c("Stationnumber","Name","weekday","RidesOut","RidesIn","Rideoutminusin"))
require(data.table)
stationFluxAvgWeekday <- data.table(stationFluxAvgWeekday)
stationFluxAvgWeekday <- stationFluxAvgWeekday[,list(RidesOut=mean(RidesOut),
                                                     RidesIn=mean(RidesIn),
                                                     Rideoutminusin=mean(Rideoutminusin)), 
                                               by = "Stationnumber,Name,weekday"]

# Orders the data set by station number
stationFluxAvgWeekday <- stationFluxAvgWeekday[order(stationFluxAvgWeekday$Stationnumber),]

# Writes stationFluxAvgWeekday to CSV
#write.csv(stationFluxAvgWeekday, file = "Average_Weekday_Flux_17.csv")

# Calculate average flux for each station by weekday and hour
stationFluxAvgHour <- stationFluxHours %>%
  subset(select = c("Stationnumber","Name","weekday","hour","RidesOut","RidesIn","Rideoutminusin"))
require(data.table)
stationFluxAvgHour <- data.table(stationFluxAvgHour)
stationFluxAvgHour <- stationFluxAvgHour[,list(RidesOut=mean(RidesOut),
                                               RidesIn=mean(RidesIn),
                                        Rideoutminusin=mean(Rideoutminusin)), 
                                        by = "Stationnumber,Name,weekday,hour"]

# Orders the data set by station number, weekday, hour
stationFluxAvgHour <- stationFluxAvgHour[order(stationFluxAvgHour$Stationnumber,
                                               stationFluxAvgHour$weekday,
                                               stationFluxAvgHour$hour),]

# Writes stationFluxAvgHour to CSV
#write.csv(stationFluxAvgHour, file = "Average_Hourly_Flux_17.csv")



