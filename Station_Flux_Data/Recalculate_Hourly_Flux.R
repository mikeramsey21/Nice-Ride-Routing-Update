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

# This is an R script to reformulate the data present in "stationFluxAvgHour17.csv"
# This csv file contains the calculated average hourly flux for each station for 
# each day of the week. The flux is defined as the number of rides out of the 
# station minus the number of rides coming into the station.

# In it's current form, this R-script recalculates the flux to be viewed in the 
# form of morning, afternoon, and evening. This corresponding to new routes being
# made 3 times a day for each shuttler.

####### Workspace Items #######
# data: The original average hourly flux calculation for each day of the week
# monday: The updated flux calculation by Morning, Afternoon, and Evening

#############################################

# Load necessary packages
library(tidyr)
library(dplyr)

# Load the data
data <- read.csv("Station_Flux_Data/Average_Hourly_Flux_17.csv",row.names = 1)

# Define new column for morning, afternnoon, and evening
data <- data %>%
  mutate(timezone = ifelse(hour >= 3 & hour < 11, "M",
                           ifelse(hour >= 11 & hour < 19, "A", "E")))

# Add up the fluxes for each time period
data <- data %>%
  group_by(Stationnumber,weekday, timezone) %>%
  summarise(RidesOut = sum(RidesOut),
            RidesIn = sum(RidesIn),
            Rideoutminusin = sum(Rideoutminusin))

# Round the flux column
data$Rideoutminusin <- round(data$Rideoutminusin)

# Reformat the data frame:
Monday <- data %>%
  filter(weekday == "Mon") %>%
  select(Stationnumber,timezone,Rideoutminusin) %>%
  spread(timezone,Rideoutminusin)

# Replace NAs with zeros
Monday[is.na(Monday)] <- 0

# Write to a csv
#write.csv(Monday, "Average_Hourly_Flux_17_Mon_MAE.csv")
