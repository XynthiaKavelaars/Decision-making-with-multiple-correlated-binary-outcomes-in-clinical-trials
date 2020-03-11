rm(list=ls())

set.seed(500000)

# Set working directory 
setwd()

# Load packages
require(abind)
require(diagram)
require(doParallel) 
require(foreach) 
require(gsDesign)
require(inline)
require(lattice)
require(matrixStats)
require(mvtnorm)
require(Rcpp)
require(xtable)
require(shape)

# Load functions and simulation conditions
source("Functions/DefineScenarios.R") 
source("Functions/FunctionsSimulate.R") 
source("Functions/VariableDefinitions.R") 
source("Functions/FunctionsComputeN.R")
source("Functions/ComputeN.R")
source("Functions/FunctionsPresentation.R")

# Find least favorable values
source("0.1 FindLeastFavorable.R")                # Run simulation
source("0.2 EvaluateLeastFavorable.R")            # Evaluate simulation

# Run simulations 
source("1.1 Simulate FD.R")                       # Fixed design
source("1.2 Simulate SD.R")                       # Group sequential design
source("1.3 Simulate AD.R")                       # Adaptive design
nSim <- 5000
# Evaluate simulation
source("2.0 Evaluate.R")                          # Extract and tabulate quantities of interest
source("2.1 Make tables.R")                       # Make Latex table
source("2.2 Make figures.R")                      # Make figures
