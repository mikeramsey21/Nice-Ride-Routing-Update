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

# This is an R-script that extracts the location information of the identified problem
# stations for Monday Morning and writes a csv with the information

####### Workspace Items #######
# data <- The Monday Morning Problem Stations
# loc <- location data on all of the stations
# locsub <- location data on only the problem stations

#############################################

# Import necessary packages
library(dplyr)
library(tidyr)

# Load the dataset
data <- read.csv("probStationsMonMorning.csv")
loc <- read.csv("Nice_ride_data_2017_season\Nice_Ride_2017_Station_Locations.csv")

# List of problem stations
problemS <- data$Stationnumber

# Get subset of data
locsub <- loc %>%
  filter(Number %in% problemS)

# Write locations to a csv
#write.csv(locsub, "probStationsMonMorningLoc.csv")
