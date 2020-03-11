# Decision-making-with-multiple-binary-outcomes-in-clinical-trials
Respository storing code for Kavelaars, Mulder, &amp; Kaptein (2020) Decision-making with multiple binary outcomes in clinical trials

Overview of contents:
This folder contains all necessary files to replicate the simulation study for the manuscript ”Decision-making with multiple correlated binary outcomes in clinical trials" by Xynthia Kavelaars, Joris Mulder, and Maurits Kaptein.

Description of files and folders:
########################################################################################################
#					Simulation 						       #
########################################################################################################
\Simulation:				        Folder containing subfolders with scripts, functions, simulation conditions.
RunSimulation.R				      R-script to run all types of simulations
1.1 Simulate FD.R			      R-script to run the simulation for a fixed design 
1.2 Simulate SD.R			      R-script to run the simulation for a group sequential design
1.3 Simulate AD.R			      R-script to run the simulation for an adaptive design
2.0 Evaluate.R				      R-script to extract output from workspaces and tabulate quantities of interest
2.1 Make tables.R			      R-script to create Latex tables as presented in the manuscript
2.2 Make figures.R			    R-script to create plots presented in manuscript

\\Functions:				        Folder containing functions used by scripts in folder \Simulation:
ComputeN.R				          R-script to compute sample sizes for all conditions using "FunctionsComputeN.R"
DefineScenarios.R			      Various functions to find parameters of defined data generating mechansms
FunctionsSimulate.R			    Various functions used by "1.1 Simulate FD.R", "1.2 Simulate SD.R", and "1.3 Simulate AD.R"
FunctionsComputeN.R			    Functions to compute sample sizes for various decison rules under a fixed design
FunctionsPresentation.R			Auxiliary functions for layout of Latex tables
VariableDefinitions.R			  Various variable definitions

\\Workspaces:				        Empty folder, where workspaces will be stored after running "1.1 Simulate FD.R", "1.2            Simulate SD.R", "1.3 Simulate AD.R"

\\Plots:				            Empty folder, where figures will be stored after running "2.2 Make figures.R"
########################################################################################################
#					Important 						       #
########################################################################################################
Make sure the working directory is set to the folder that contains the extracted files when running scripts. 

For any help with the files in this archive, please contact Xynthia Kavelaars (x.m.kavelaars@tilburguniversity.edu). 
