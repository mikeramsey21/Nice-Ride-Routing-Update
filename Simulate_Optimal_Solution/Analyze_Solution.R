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

# This is an R-script that was intended to run thought the nice-ride data 
# to test our proposed clustering solution. A base skeleton of the simulator
# is available but was never used. We are still in progress of testing our
# proposed solution

#######################
# THIS IS FUTURE WORK #
#######################

####### Workspace Items #######
# DATA FRAMES
# rides: The 2017 nice ride trip history data
# locations: The 2017 nice ride location data
# data: A merged form of the 2017 nice ride trip history and location data
# stationFluxOut: Subset of data when a bike is leaving a station
# stationFluxIn: Subset of data when a bike is coming in to a station
# stationFlux: Merged form of stationFluxOut and stationFluxIn
# subdata1: Specific ride data for a random station
# subdata2: Specific ride data for a random station
# subdata3: Specific ride data for a random station
# subdata4: Specific ride data for a random station
# Bikes: Starting number of bikes at each station

# VALUES
# currentdate: The date of the first data point
# Init_Bikes: The inital number of bikes a station starts with
# Total_Bike: Total number of bikes in the nice-ride system
# Total_Docks: Total number of docks in the system
# Total_Stat: Total number of stations in the nice-ride system

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

# Total number of bikes
Total_Bike <- 1800
Total_Docks <- sum(locations$Total.docks)
Total_Stat <- nrow(locations)

# Load in Bike Initialization data frame
Bikes <- read.csv("Bike_Initial.csv",row.names = 1)
Bikes$Bikes <- rep(9,nrow(Bikes))
Init_Bikes <- Bikes$Bikes

##############################################################
# Simulator

# Get current date
currentdate <- date(mdy_hm(stationFlux$date[1]))

# Loop through every data instance
for (i in c(1:nrow(stationFlux))) {
  
  # Reseset bikes if new date
  #newdate <- date(mdy_hm(stationFlux$date[i]))
  #if (newdate != currentdate){
  #  Bikes$Bikes <- Init_Bikes
  #  currentdate <- newdate
  #}
  
  # Get station name
  stationname <- as.character(stationFlux$Station[i])
  
  # Check to see if Ridein or Rideout
  if (stationFlux$Rideout[i] == T) {
    
    # Check to see if a bike is available
    if (Bikes[stationname,"Bikes"] > 0) {
    
      # Decrease Bike Count by 1
      value <- Bikes[stationname,"Bikes"] - 1
      Bikes[stationname,"Bikes"] <- value
    } else {
      
      # Record that we are out of bikes
      Bikes[stationname,"Bikesout"] <- Bikes[stationname,"Bikesout"] + 1
    }
    
  } else {
    
    # Check to see if a dock is available
    if (Bikes[stationname,"Bikes"] < Bikes[stationname,"Total.docks"]) {
    
      # Increase Bike Count by 1
      value <- Bikes[stationname,"Bikes"] + 1
      Bikes[stationname,"Bikes"] <- value
    } else {
      
      # Record that we are out of docks
      Bikes[stationname,"Docksout"] <- Bikes[stationname,"Docksout"] + 1
    }
  }
}

##############################################################
# Poisson Proccess 

# Calculate inter-arrival times of when people show up to station
data <- data %>% 
  group_by(Start.station) %>%
  mutate(last.showed = lag(Start.date))
data <- data %>%
  mutate(start.interval = abs(time_length(interval(mdy_hm(Start.date), mdy_hm(last.showed)), unit = 'minute')))
data$last.showed <- NULL

##############################################################
# Data visualization

# Look at distribution of inter-arrival times
subdata1 <- data %>%
  filter(Start.station == "South 2nd Street & 3rd Ave S")
ggplot(data = subdata1, aes(x = start.interval)) +
  geom_histogram()

# Look at distribution of inter-arrival times
subdata2 <- data %>%
  filter(Start.station == "Selby Ave & Virginia Street")
ggplot(data = subdata2, aes(x = start.interval)) +
  geom_histogram()

# Look at distribution of inter-arrival times
subdata3 <- data %>%
  filter(Start.station == "Lake Como Pavilion")
ggplot(data = subdata3, aes(x = start.interval)) +
  geom_histogram()

# Look at distribution of inter-arrival times
subdata4 <- data %>%
  filter(Start.station == "Social Sciences")
ggplot(data = subdata4, aes(x = start.interval)) +
  geom_histogram()

