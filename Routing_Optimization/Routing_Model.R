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

# This is an R-script to solve the individual traveling salesman problem (TSP) for
# each cluster of stations present in "clusters_3.csv". The clustering results come 
# from k-means clustering, solved using AMPL (see AMPL_files folder). 

# We estimate the distances from each station using the "taxicab distance". These distances
# are in the file "Station_Distances.csv" located in the Station_Distance_Data folder

# We have the latitudes and longituds for each station in the file 
# "Nice_Ride_2017_Station_Locations.csv" in the folder Nice_ride_data_2017_season 

####### Workspace Items #######
# DATA FRAMES
# clusters: A data-frame containing the clustering results from AMPL. The first 
#           column lists the station number and the remaining columns correspond
#           to the cluster that the station is in (1 means in cluster)
# clustersub1: Subset of stations in cluster 1
# clustersub2: Subset of stations in cluster 2
# clustersub3: Subset of stations in cluster 3
# clustersub4: Subset of stations in cluster 4

# data: A data-frame where the (i,j)^th entry corresponds to the "taxicab distance" 
#       from station i to j
# subdata1: Subset of data in data corresponding to stations in cluster 1
# subdata2: Subset of data in data corresponding to stations in cluster 2
# subdata3: Subset of data in data corresponding to stations in cluster 3
# subdata4: Subset of data in data corresponding to stations in cluster 4

# loc: Data-frame containing latitutes and longitudes of the nice-ride stations
# loc1: Filtered location data from "loc" for cluster 1
# loc2: Filtered location data from "loc" for cluster 2
# loc3: Filtered location data from "loc" for cluster 3
# loc4: Filtered location data from "loc" for cluster 4

# paths: Row binded data frame of paths1 through paths4
# paths1: Merged and tidyied information of solution1 and loc1
# paths2: Merged and tidyied information of solution1 and loc2
# paths3: Merged and tidyied information of solution1 and loc3
# paths4: Merged and tidyied information of solution1 and loc4

# MergedRides: Data-frame contining the starting and ending latitudes and longitudes for each shuttler
# part1: Data-frame containing the starting latitudes and longitudes for each shuttler
# part2: Data-frame containing the ending latitudes and longitudes for each shuttler

# MODELS
# model1: Formulation of TSP Model on Cluster 1
# model2: Formulation of TSP Model on Cluster 2
# model3: Formulation of TSP Model on Cluster 3
# model4: Formulation of TSP Model on Cluster 4

# MODEL RESULTS
# result1: Results of solving model1 using glpk
# result2: Results of solving model2 using glpk
# result3: Results of solving model3 using glpk
# result4: Results of solving model4 using glpk

# MODEL SOLUTIONS
# solution1: Optimal route for salesman for cluster 1
# solution2: Optimal route for salesman for cluster 2
# solution3: Optimal route for salesman for cluster 3
# solution4: Optimal route for salesman for cluster 4

# VALUES
# cluster1: Matrix form of clustersub1
# cluster2: Matrix form of clustersub2
# cluster3: Matrix form of clustersub3
# cluster4: Matrix form of clustersub4

# MN: ggmap of optimal routes for each shuttler

#############################################

# Load necessary libraries
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(tidyr)

#######################################################
# Data Processing

# Load the data frames
data <- read.csv("../Station_Distance_Data/Station_Distances.csv",check.names = F)
clusters <- read.csv("clusters_4.csv")

# Extract subsets of cluster and include the depot station (30205)
clustersub1 <- clusters %>% 
  filter(X1 == 1)
cluster1 <- clustersub1$Number
cluster1 <- c(30205, cluster1)
clustersub2 <- clusters %>% 
  filter(X2 == 1)
cluster2 <- clustersub2$Number
cluster2 <- c(30205, cluster2)
clustersub3 <- clusters %>% 
  filter(X3 == 1)
cluster3 <- clustersub3$Number
cluster3 <- c(30205,cluster3)
clustersub4 <- clusters %>% 
  filter(X4 == 1)
cluster4 <- clustersub4$Number
cluster4 <- c(30205,cluster4)

# Get the subdata
subdata1 <- data %>%
  filter(Title %in% cluster1) %>%
  select(one_of(as.character(cluster1)))
subdata2 <- data %>%
  filter(Title %in% cluster2) %>%
  select(one_of(as.character(cluster2)))
subdata3 <- data %>%
  filter(Title %in% cluster3) %>%
  select(one_of(as.character(cluster3)))
subdata4 <- data %>%
  filter(Title %in% cluster4) %>%
  select(one_of(as.character(cluster4)))

############################################################
# Model 1
n <- nrow(subdata1)
m <- 1

distance1 <- as.matrix(subdata1)
# the depot is always idx 1
model1 <- MIPModel() %>%
  
  # we create a variable that is 1 iff we travel from city i to j by Salesman k
  add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:m, type = "binary") %>%
  
  # helper variable for the MTZ sub-tour constraints
  add_variable(u[i, k], i = 1:n, k = 1:m, lb = 1, ub = n) %>% 
  
  # minimize travel distance and latest arrival
  set_objective(sum_expr(distance1[i, j] * x[i, j, k], i = 1:n, j = 1:n, k = 1:m), "min") %>%
  
  # you cannot go to the same city
  add_constraint(x[i, i, k] == 0, i = 1:n, k = 1:m) %>%
  
  # each salesman needs to leave the depot
  add_constraint(sum_expr(x[1, j, k], j = 2:n) == 1, k = 1:m) %>%
  
  # each salesman needs to come back to the depot
  add_constraint(sum_expr(x[i, 1, k], i = 2:n) == 1, k = 1:m) %>%
  
  # if a salesman comes to a city he has to leave it as well
  add_constraint(sum_expr(x[j, i, k], j = 1:n) == sum_expr(x[i, j, k], j = 1:n), i = 2:n, k = 1:m) %>%
  
  # leave each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], j = 1:n, k = 1:m) == 1, i = 2:n) %>% 
  
  # arrive at each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], i = 1:n, k = 1:m) == 1, j = 2:n) %>% 
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i, k] >= 2, i = 2:n, k = 1:m) %>% 
  add_constraint(u[i, k] - u[j, k] + 1 <= (n - 1) * (1 - x[i, j, k]), i = 2:n, j = 2:n, k = 1:m)
model1

result1 <- solve_model(model1, with_ROI(solver = "glpk",verbose = TRUE))

solution1 <- get_solution(result1, x[i, j, k]) %>% 
  filter(value > 0) 

############################################################
# Model 2
n <- nrow(subdata2)
m <- 1

distance2 <- as.matrix(subdata2)
# the depot is always idx 1
model2 <- MIPModel() %>%
  
  # we create a variable that is 1 iff we travel from city i to j by Salesman k
  add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:m, type = "binary") %>%
  
  # helper variable for the MTZ sub-tour constraints
  add_variable(u[i, k], i = 1:n, k = 1:m, lb = 1, ub = n) %>% 
  
  # minimize travel distance and latest arrival
  set_objective(sum_expr(distance2[i, j] * x[i, j, k], i = 1:n, j = 1:n, k = 1:m), "min") %>%
  
  # you cannot go to the same city
  add_constraint(x[i, i, k] == 0, i = 1:n, k = 1:m) %>%
  
  # each salesman needs to leave the depot
  add_constraint(sum_expr(x[1, j, k], j = 2:n) == 1, k = 1:m) %>%
  
  # each salesman needs to come back to the depot
  add_constraint(sum_expr(x[i, 1, k], i = 2:n) == 1, k = 1:m) %>%
  
  # if a salesman comes to a city he has to leave it as well
  add_constraint(sum_expr(x[j, i, k], j = 1:n) == sum_expr(x[i, j, k], j = 1:n), i = 2:n, k = 1:m) %>%
  
  # leave each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], j = 1:n, k = 1:m) == 1, i = 2:n) %>% 
  
  # arrive at each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], i = 1:n, k = 1:m) == 1, j = 2:n) %>% 
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i, k] >= 2, i = 2:n, k = 1:m) %>% 
  add_constraint(u[i, k] - u[j, k] + 1 <= (n - 1) * (1 - x[i, j, k]), i = 2:n, j = 2:n, k = 1:m)
model2

result2 <- solve_model(model2, with_ROI(solver = "glpk",verbose = TRUE))

solution2 <- get_solution(result2, x[i, j, k]) %>% 
  filter(value > 0)

############################################################
# Model 3
n <- nrow(subdata3)
m <- 1

distance3 <- as.matrix(subdata3)
# the depot is always idx 1
model3 <- MIPModel() %>%
  
  # we create a variable that is 1 iff we travel from city i to j by Salesman k
  add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:m, type = "binary") %>%
  
  # helper variable for the MTZ sub-tour constraints
  add_variable(u[i, k], i = 1:n, k = 1:m, lb = 1, ub = n) %>% 
  
  # minimize travel distance and latest arrival
  set_objective(sum_expr(distance3[i, j] * x[i, j, k], i = 1:n, j = 1:n, k = 1:m), "min") %>%
  
  # you cannot go to the same city
  add_constraint(x[i, i, k] == 0, i = 1:n, k = 1:m) %>%
  
  # each salesman needs to leave the depot
  add_constraint(sum_expr(x[1, j, k], j = 2:n) == 1, k = 1:m) %>%
  
  # each salesman needs to come back to the depot
  add_constraint(sum_expr(x[i, 1, k], i = 2:n) == 1, k = 1:m) %>%
  
  # if a salesman comes to a city he has to leave it as well
  add_constraint(sum_expr(x[j, i, k], j = 1:n) == sum_expr(x[i, j, k], j = 1:n), i = 2:n, k = 1:m) %>%
  
  
  # leave each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], j = 1:n, k = 1:m) == 1, i = 2:n) %>% 
  
  # arrive at each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], i = 1:n, k = 1:m) == 1, j = 2:n) %>% 
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i, k] >= 2, i = 2:n, k = 1:m) %>% 
  add_constraint(u[i, k] - u[j, k] + 1 <= (n - 1) * (1 - x[i, j, k]), i = 2:n, j = 2:n, k = 1:m)
model3

result3 <- solve_model(model3, with_ROI(solver = "glpk",verbose = TRUE))

solution3 <- get_solution(result3, x[i, j, k]) %>% 
  filter(value > 0)

############################################################
# Model 4
n <- nrow(subdata4)
m <- 1

distance4 <- as.matrix(subdata4)
# the depot is always idx 1
model4 <- MIPModel() %>%
  
  # we create a variable that is 1 iff we travel from city i to j by Salesman k
  add_variable(x[i, j, k], i = 1:n, j = 1:n, k = 1:m, type = "binary") %>%
  
  # helper variable for the MTZ sub-tour constraints
  add_variable(u[i, k], i = 1:n, k = 1:m, lb = 1, ub = n) %>% 
  
  # minimize travel distance and latest arrival
  set_objective(sum_expr(distance4[i, j] * x[i, j, k], i = 1:n, j = 1:n, k = 1:m), "min") %>%
  
  # you cannot go to the same city
  add_constraint(x[i, i, k] == 0, i = 1:n, k = 1:m) %>%
  
  # each salesman needs to leave the depot
  add_constraint(sum_expr(x[1, j, k], j = 2:n) == 1, k = 1:m) %>%
  
  # each salesman needs to come back to the depot
  add_constraint(sum_expr(x[i, 1, k], i = 2:n) == 1, k = 1:m) %>%
  
  # if a salesman comes to a city he has to leave it as well
  add_constraint(sum_expr(x[j, i, k], j = 1:n) == sum_expr(x[i, j, k], j = 1:n), i = 2:n, k = 1:m) %>%
  
  # leave each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], j = 1:n, k = 1:m) == 1, i = 2:n) %>% 
  
  # arrive at each city with only one salesman
  add_constraint(sum_expr(x[i, j, k], i = 1:n, k = 1:m) == 1, j = 2:n) %>% 
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i, k] >= 2, i = 2:n, k = 1:m) %>% 
  add_constraint(u[i, k] - u[j, k] + 1 <= (n - 1) * (1 - x[i, j, k]), i = 2:n, j = 2:n, k = 1:m)
model4

result4 <- solve_model(model4, with_ROI(solver = "glpk",verbose = TRUE))

solution4 <- get_solution(result4, x[i, j, k]) %>% 
  filter(value > 0)

###############################################################
# Creating the map
# Load the other dataset
loc <- read.csv("../Nice_ride_data_2017_season/Nice_Ride_2017_Station_Locations.csv")
loc$Number <- as.factor(loc$Number)

# Filter the data that we need for each cluster
loc1 <- loc %>%
  filter(Number %in% colnames(distance1))
loc1$Number <- as.numeric(as.character(loc1$Number))
loc2 <- loc %>%
  filter(Number %in% colnames(distance2))
loc2$Number <- as.numeric(as.character(loc2$Number))
loc3 <- loc %>%
  filter(Number %in% colnames(distance3))
loc3$Number <- as.numeric(as.character(loc3$Number))
loc4 <- loc %>%
  filter(Number %in% colnames(distance4))
loc4$Number <- as.numeric(as.character(loc4$Number))

# Before this point, our solutioni data frames contain the optimal route
# for traveling to each station. The stations are labeled arbitratily (1 to 2)
# or (3,4). The next chunk of code replaces these arbitrary labels with the
# actaual station numbers. Coded by BRUTE FORCE
# This relied on the ordering of the station numbers and arbitrary numbers clearly
for (entry in 1:nrow(solution1)){
  a <- solution1$i[entry]
  b <- solution1$j[entry]
  solution1$i[entry] <- loc1$Number[a]
  solution1$j[entry] <- loc1$Number[b]
}
for (entry in 1:nrow(solution2)){
  a <- solution2$i[entry]
  b <- solution2$j[entry]
  solution2$i[entry] <- loc2$Number[a]
  solution2$j[entry] <- loc2$Number[b]
}
for (entry in 1:nrow(solution3)){
  a <- solution3$i[entry]
  b <- solution3$j[entry]
  solution3$i[entry] <- loc3$Number[a]
  solution3$j[entry] <- loc3$Number[b]
}
for (entry in 1:nrow(solution4)){
  a <- solution4$i[entry]
  b <- solution4$j[entry]
  solution4$i[entry] <- loc4$Number[a]
  solution4$j[entry] <- loc4$Number[b]
}

# Create data_frames for optimal paths
paths1 <- select(solution1, i, j, k) %>% 
  rename(from = i, to = j, salesman = k) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(loc1, by = c("idx_val" = "Number"))
paths2 <- select(solution2, i, j, k) %>% 
  rename(from = i, to = j, salesman = k) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(loc2, by = c("idx_val" = "Number"))
paths3 <- select(solution3, i, j, k) %>% 
  rename(from = i, to = j, salesman = k) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(loc3, by = c("idx_val" = "Number"))
paths4 <- select(solution4, i, j, k) %>% 
  rename(from = i, to = j, salesman = k) %>% 
  mutate(trip_id = row_number()) %>% 
  tidyr::gather(property, idx_val, from:to) %>% 
  mutate(idx_val = as.integer(idx_val)) %>% 
  inner_join(loc4, by = c("idx_val" = "Number"))

# Rename the salesman number 
paths2$salesman <- rep(2,nrow(paths2))
paths3$salesman <- rep(3,nrow(paths3))
paths4$salesman <- rep(4,nrow(paths4))
  
# Bind all of the data into one data frame
paths <- rbind(paths1,paths2,paths3,paths4)

# Create data frame that contains starting locations for each shuttler
part1 <- paths %>% 
  filter(property == "from") %>%
  arrange(trip_id) %>%
  select(salesman,Latitude,Longitude) %>%
  rename(start_lat = Latitude, start_long = Longitude)

# Create data frame that contains ending locations for each shuttler
part2 <- paths %>% 
  filter(property == "to") %>%
  arrange(trip_id) %>%
  select(Latitude,Longitude) %>%
  rename(end_lat = Latitude, end_long = Longitude)

# Data frame containing each route for each shuttler
MergedRides <- cbind(part1,part2)

# Create routing map for each bike shuttler
MN <- get_map(location = c(lat = 44.962442, lon = -93.216800), zoom=12)
ggmap(MN) + 
  geom_segment(data=MergedRides, aes(x=start_long, y=start_lat,
                                     xend=end_long, yend=end_lat, 
                                     color = as.factor(salesman)),size = 1) + 
  scale_color_discrete(name = "Salesman") +
  ggtitle("Optimal Routes for Bike-Shuttlers") +
  ylab("Latitude") + 
  xlab("Longitude")

