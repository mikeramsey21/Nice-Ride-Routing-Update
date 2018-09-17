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

# This is an R-script to perform a categorical analysis of our data. We will investigate
# properties such as the most popular rides, rides by the time of day, rides by members
# and nonmembers, etc.

####### Workspace Items #######
# DATA FRAMES
# rides: The 2017 nice ride trip history data
# locations: The 2017 nice ride location data
# data: A merged form of the 2017 nice ride trip history and location data
# stationFluxOut: Subset of data when a bike is leaving a station
# stationFluxIn: Subset of data when a bike is coming in to a station
# stationFlux: Merged form of stationFluxOut and stationFluxIn
# weekday: Subset of stationFlux where all rides occurr on a weekday
# weekend: Subset of stationFlux where all rides occurr on a weekend
# weekdayHour: Averages of weekday rides by Morning, Afternoon, and Evening
# WeekendHour: Averages of weekend rides by Morning, Afternoon, and Evening
# morning: Averages for flux for a weekday morning
# afternoon: Averages for flux for a weekday afternoon
# evening: Averages for flux for a weekday evening
# ridecount: Total number of rides in 2017 coming in and out by station
# tiecounttimezone: ridecount by station by timezon (M,A,E)
# popularRides: The most popular rides 
# lakeRides: Rides around the lake
# members: Number of member rides and non-member rides
# weekdayfluxgeneral: The average flux of a station by day for a weekday

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
         Startingweekday = wday(mdy_hm(Start.date),label = T),
         Endingdate = date(mdy_hm(End.date)), 
         Endinghour = hour(mdy_hm(End.date)),
         Endingminute = minute(mdy_hm(End.date)),
         Endingweekday = wday(mdy_hm(Start.date), label = T))

# Delete ride duration time of less that 60 seconds
data <- data %>%
  filter(data$Total.duration..Seconds. > 60)

# Create subset of columns for rides out
stationFluxOut <- data %>%
  select(Start.station,Total.docks.x,
         start_lat,start_long,Start.date, Startingdate, 
         Startinghour, Startingweekday)

# Create column indicating this is describing a ride out
stationFluxOut <- stationFluxOut %>%
  mutate(Rideout=TRUE)

# Change names of data frame
names(stationFluxOut) <- c("Station","Numdocks",
                           "Latitude","Longitude","date",
                           "dateISO", "hour","weekday","Rideout")

# Create subset of columns for rides in
stationFluxIn <- data %>%
  select(End.station,Total.docks.y,
         end_lat,end_long,End.date, Endingdate,
         Endinghour, Endingweekday)

# Create column indicating this is describing a ride in
stationFluxIn <- stationFluxIn %>%
  mutate(Rideout=FALSE)

# Change names of data frame
names(stationFluxIn) <- c("Station","Numdocks",
                          "Latitude","Longitude","date",
                          "dateISO", "hour","weekday","Rideout")

# Combine both data frames for rides in and rides out
stationFlux <- rbind(stationFluxOut,stationFluxIn)

# Order by date
stationFlux <- stationFlux[order(mdy_hm(stationFlux$date)),]

# Create timezones
# Morning is 2 to 10
# Afternoon is 10 to 18
# Evening is 18 to 2
stationFlux <- stationFlux %>%
  mutate(timezone = ifelse(hour >= 2 & hour < 10, "M",
                    ifelse(hour >= 10 & hour < 18, "A", "E")))

# Find the hourly counts for each city
weekday <- stationFlux %>%
  filter(weekday %in% c("Mon","Tue","Wed","Thu","Fri"))
weekend <- stationFlux %>%
  filter(weekday %in% c("Sat","Sun"))

# Find the hourly averages by station
uniquedays <- length(unique(stationFlux$dateISO))
weekdayHour <- weekday %>%
  group_by(Station,timezone) %>%
  summarise(bikesout = sum(Rideout)/uniquedays,
            bikesin = sum(Rideout-1)/uniquedays,
            bikediff = bikesout + bikesin)
weekendHour <- weekend %>%
  group_by(Station,timezone) %>%
  summarise(bikesout = sum(Rideout)/uniquedays,
            bikesin = sum(Rideout-1)/uniquedays,
            bikediff = bikesout + bikesin)

# Data frames for average flux for weekday by timezone
morning <- weekdayHour %>%
  filter(timezone == "M")
afternoon <- weekdayHour %>%
  filter(timezone == "A")
evening <- weekdayHour %>%
  filter(timezone == "E")

# Check total number of rides
ridecount <- stationFlux %>%
  group_by(Station,Rideout) %>%
  summarise(n())

# Check total rides for morning, afternoon, and evening
ridecounttimezone <- stationFlux %>%
  group_by(Station,timezone,Rideout) %>%
  summarise(n())

# Find most popular rides
popularRides <- data %>%
  group_by(Start.station,End.station) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Check lake rides
lakeRides <- popularRides %>%
  filter(Start.station %in% c("Lake Street & Knox Ave S","Lake Calhoun Center",
                  "Lake Harriet Bandshell","W 36th Street & W Calhoun Parkway"))

# Number of member to non-member rides
members <- data %>%
  group_by(Account.type) %>%
  summarise(count = n())

# Get overall average flux of stations during weekday
weekdayfluxgeneral <- weekdayHour %>%
  group_by(Station) %>%
  summarise(sum(bikediff))
            
