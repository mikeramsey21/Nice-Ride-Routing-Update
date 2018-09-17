# Nice-Ride-Routing-Update
A cleaned version of Nice Ride Routing

## Note: You will have to unzip "Nice_ride_data_2017_season"

## Files
Clustering_Optimization_AMPL: Contains AMPL .dat and .mod files to perform k-means clustering optimization problem on the problematice NiceRide stations.

Descriptive_Analysis: Contains R script taking a look at the NiceRide data by subgroup. It also contains a .csv file for the most popular rides

Figures: Contains a figure of our optimal routing scheme.

Problem_Stations: Contains two R scripts identifying the problem stations for monday morning. Also contain two .csv files containing details of the problem stations and its location (latitute and longitude).

Reference_Papers: Relevant papers on the subject.

Routing_Optimization: R script that computes the optimal routing scheme for each cluster of stations to visit. Also contains .csv files of the ouput clusters for the AMPL optimization problem.

Simulate_Optimal_Solution: An unfinished R-script that is intended to be used as a method to see how our routing strategy performs.

Station_Distance_Data: Contains .csv files containing the distances relative from station to station. Note that we computed these distances using the "taxicab metric" using the latitudes and longitudes.

Station_Flux_Data: Contains many .csv files summarizing station flux, defined as the number of riders coming into the station minus the number of riders coming out of the station.

Visual_Simulation: Contains an R scipt that comuptes the flux for every station in increments of 15 minutes. We are missing the R script that actually produces the simulation.

Nice_ride_data_2017_season: Contains the data for the NiceRide rides in 2017.

Nice_Ride_Bikes_Presentation: A presentation of our findings and recommendations from analyzing the data.

