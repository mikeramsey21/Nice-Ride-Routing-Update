#############################################
# Michael Ramsey
# Date Created: 6/15/18
# Last Updated: 9/15/18
#############################################

# This is an AMPL file to perform (almost) k-means clusering on
# the identified problem stations for the nice-ride data
# Rather than minimizing with respect to a mean center in k-means 
# clustering, we define the cluster center as one of the Locations

# This model is created in the style of optimization in respect to
# Locations and Stores. The question we solve is in what way can
# we open Locations out of the possible Stores to minimize a cost
# that is dependent on opening costs and relative distances to each
# Store

# In our case, we have a number of depots that are dependent on the
# number of workers that are avaialable. The opening costs are zero
# since each NiceRide station is already available. We essentially
# choose 4 depots as our "cluster centers" and we minimize the squared
# distance from each point in the cluster to their corresponding depot

#############################################
# Reset the model
reset;

#############################################
# Parameters and variables

# The nodes of the system
set Locations; # Possible Depot's to open
set Stores; # The possbile stores to visit

# The opening cost of each Location (Zero for nice ride)
# Note: Could explore making new stations with this
param opening_cost {Locations} :=0;

# Demands are 1 since you need to put it in one cluster
# Note: Could explore removing a station with this
param demands {Stores} :=1;

# Longitude and Latitude of each Store
param xCoord {Stores};
param yCoord {Stores};

# Define euclidean distance matrix
param trans_cost {i in Locations, j in Stores} :=
sqrt((xCoord[i]-xCoord[j])^2+(yCoord[i]-yCoord[j])^2);

# Binary variable to see which Stores are the Depots
# Note: For NiceRide Data, every Store is opened
var x {Locations} binary;

# Indicates which cluster (Location) that each Store is in
var y {Locations, Stores} >=0;

#############################################
# Objective function

minimize total_cost: sum{l in Locations} opening_cost[l]*x[l]+
sum{l in Locations, s in Stores} demands[s]*trans_cost[l,s]*y[l,s];

#############################################
# Constraints

# Don't send a truck if you don't want to open a store
# For NiceRide, we open every station, so y[l,s] = 1
s.t. y_cons {l in Locations, s in Stores} : y[l,s] <= x[l];

# Include each Store once in each cluster
# Don't send multiple NiceRide workers
s.t. demand_cons {s in Stores}: sum{l in Locations} y[l,s] = 1;

# Specify the total number of cluster
# Total number of NiceRide workers available
s.t. cardinality: sum{l in Locations} x[l]=4;

# Control the size of each cluster
# Make sure that the nice-ride worker can visit every station
s.t. cluster_size {l in Locations}: sum{s in Stores} y[l,s] <= 13;

#############################################
# Choose the solver
option solver cplex;


