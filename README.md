# Nice-Ride-Routing-Update
We analyze the publically avilable data from Nice Ride Minnesota for 2017 to identify pitfalls of the system and provide solutions for improvment. 

We propose an improved bike-shuttling service. We noticed that at many times during the day, a subset of the stations experienced periods of high flux. In other words, many bikes were either leaving the station or going toward a station. This causes the bike docking station to either become empty or full. If this happens, this causes customers to become unhappy and dissatisfied, which leads to decresed revenue. After interviewing NiceRide experts, we learned that they have not implemented a data-driven method to shuttle bikes. We propose the following

1. Identify problematic stations during a given time interval (ex 5-10am). A problem station is defined as a station that falls outside the docking capacity range of 20-80%. 

2. Depedent on the number of shuttle workers available, optimally cluster the stations that are near each other. This is performed in AMPL and is done with an algorithm similar to k-means clustering.

3. Construct an optimal deliver schedule for each driver. This is done by posing the problem as a traveling salesman problem (TSP)

## Collaborators
Michael Ramsey
Eric Roberts
Olivia Cannon
Ariel Bowman
Elizabeth Wicks
Sheng Zhang

After analyzing the data

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

