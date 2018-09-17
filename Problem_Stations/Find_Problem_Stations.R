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

# This is an R-script that identifies problematic nice-ride stations dependent on 
# the hourly flux. We define a "problem station" as a station that either exceeds
# 80% capacity or falls below 20% capacity at a given moment

# The hourly flux data is present in the file "Average_Hourly_Flux_17.csv" in the
# Station_Flux_Data folder.

# In it's current form, we are calculating the problem stations for a typical 
# monday morning

####### Workspace Items #######
# DATA FRAMES
# stationFluxAvgHour: The Average Hourly flux data 
# sFAH: Hourly flux data for Monday morning
# docksdata: Number of docks for each station
# alldata: Combined version of sFAH and docksdata
# sinks: The stations where too many bikes come in 
# sources: The stations where too many bikes leave
# problemStations: The problem stations identified

#############################################

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#############################################

# Import the data
stationFluxAvgHour <- read.csv("../Station_Flux_Data/Average_Hourly_Flux_17.csv")

# Filter the data for Monday Morning (between 5:00am and 11:00am)
sFAH <- stationFluxAvgHour %>%
  filter(5<=stationFluxAvgHour$hour & stationFluxAvgHour$hour<=10 & stationFluxAvgHour$weekday=="Mon")

# Extract Station, Name, and flux colulmn
sFAH <- sFAH %>%
  subset(select=c("Stationnumber","Name","Rideoutminusin"))

# Write to csv
#write.csv(sFAH, file="sFAHmondaymorning.csv")

# Get information on number of docks for each station
docksdata <- read.csv("../Nice_ride_data_2017_season/Nice_Ride_2017_Station_Locations.csv")
docksdata <- docksdata %>%
  subset(select=c(Number,Total.docks))
names(docksdata)[names(docksdata) == 'Number'] <- 'Stationnumber'
docksdata$Stationnumber <- as.factor(docksdata$Stationnumber)

# Write docksdata to csv
#write.csv(docksdata, "docksdata.csv")

# # Join docksdata and sFAH
alldata <- left_join(docksdata,sFAH)

# Add initial number of bikes
alldata <- alldata %>%
  mutate(Initialbikes=floor(0.5*Total.docks))

# Yield number of bikes after time period in of data in sFAH
alldata <- alldata %>%
  mutate(Predbikes = Initialbikes-Rideoutminusin)

# Get the sinks of the network (Too many bikes coming in)
sinks <- alldata %>%
  filter(alldata$Predbikes >= alldata$Total.docks*(.8))
# there are 13 of these

# Write sinks to csv file
#write.csv(sinks, file="sinksMonMorning.csv")

# Get the sources of the network (Too many bikes leaving)
sources <- alldata %>%
  filter(alldata$Predbikes <= alldata$Total.docks*(.2))
# there are 39 of these

# Write to csv file
#write.csv(sources, file="sourcesMonMorning.csv")

# Combine sources and sinks data
problemStations <- rbind(sources,sinks)

# Write to csv file
#write.csv(problemStations, file="probStationsMonMorning.csv")