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

# This is an R-script to compute the average flux of every station by day of the
# week in fifteen minute increments. This dataset was used to create a visualization
# of the flow of bikes coming in and out of stations

# The code for the creation of the visualization is currenlty missing, but the
# visualization is present with this file

####### Workspace Items #######
# DATA FRAMES
# rides: The 2017 nice ride trip history data
# locations: The 2017 nice ride location data
# data: A merged form of the 2017 nice ride trip history and location data
# stationFluxOut: Subset of data when a bike is leaving a station
# stationFluxIn: Subset of data when a bike is coming in to a station
# stationFlux: Merged form of stationFluxOut and stationFluxIn

# GOAL DATASET
# minuteAverages: Averages by day of the week in fifteen minute increments

##############################################################

# Import necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)

##############################################################
# Data Processing

# Import datasets
rides <- read.csv("../Nice_ride_data_2017_season/Nice_ride_trip_history_2017_season.csv", 
                  na.strings=c(""))
locations <- read.csv("../Nice_ride_data_2017_season/Nice_Ride_2017_Station_Locations.csv")
locations$Terminal <- as.factor(locations$Number)

# Join Stations and rides data sets   
data <- rides %>%
  left_join(locations, by=c(Start.station.number = "Terminal")) %>%
  rename(start_lat=Latitude, start_long=Longitude) %>%
  left_join(locations, by=c(End.station.number = "Terminal")) %>%
  rename(end_lat=Latitude, end_long=Longitude)

# Delete redundant rows
data$Station.x <- NULL
data$Station.y <- NULL

# Create new columns starting and ending date, hour, minute, and weekday
data <- data %>%
  mutate(Startingdate = date(mdy_hm(Start.date)), 
         Startinghour = hour(mdy_hm(Start.date)),
         Startingminute = minute(mdy_hm(Start.date)),
         Startingweekday = wday(mdy_hm(Start.date)),
         Endingdate = date(mdy_hm(End.date)), 
         Endinghour = hour(mdy_hm(End.date)),
         Endingminute = minute(mdy_hm(End.date)),
         Endingweekday = wday(mdy_hm(Start.date)))

# Delete duration time of less than 60 seconds
data <- data %>%
  filter(data$Total.duration..Seconds. > 60)

# creates subset of data only collecting data on rides out
stationFluxOut <- data %>%
  select(Start.station,Total.docks.x,
         start_lat,start_long,Start.date, Startingdate, 
         Startinghour, Startingminute, Startingweekday)

# adds columns which take boolean values 
# indicating that this piece of data is a ride out
stationFluxOut <- stationFluxOut %>%
  mutate(Rideout=TRUE)

# changes station number, docks, lat, and long variable names 
# (in prep for merge with rides in data frame)
names(stationFluxOut) <- c("Station","Numdocks",
                           "Latitude","Longitude","date",
                           "dateISO", "hour","minute","weekday","Rideout")

# creates subset of data only collecting data on rides in
stationFluxIn <- data %>%
  select(End.station,Total.docks.y,
         end_lat,end_long,End.date, Endingdate,
         Endinghour, Endingminute, Endingweekday)

# adds columns which take boolean values 
# indicating that this piece of data is a ride in
stationFluxIn <- stationFluxIn %>%
  mutate(Rideout=FALSE)

# changes station number, docks, lat, and long variable names (in prep for merge)
names(stationFluxIn) <- c("Station","Numdocks",
                          "Latitude","Longitude","date",
                          "dateISO", "hour","minute","weekday","Rideout")

# now we combine both data frames vertically into one big data frame
stationFlux <- rbind(stationFluxOut,stationFluxIn)

# Order the dates
stationFlux <- stationFlux[order(mdy_hm(stationFlux$date)),]

# Group by 15 minute
stationFlux <- stationFlux %>%
  mutate(minutezone = ifelse(minute < 15, "0_14",
                           ifelse(minute >= 15 & minute < 30, "15_29",
                           ifelse(minute >= 30 & minute < 44, "30_44", "45_59"))))

# 


# Find the hourly averages by station
uniquedays <- length(unique(stationFlux$dateISO))
minuteAverages <- stationFlux %>%
  group_by(Station, hour, minutezone) %>%
  summarise(bikesout = sum(Rideout)/uniquedays,
            bikesin = sum(Rideout-1)/uniquedays,
            bikediff = bikesout + bikesin)
