
# This script runs describes how to run all functions needed for the comparison
# of an individual scan against a control cohort.

# Provided is a control_cohort of normal scans from open-source datasets and 
# from Great Ormond Street Hospital, London (n = 223)

#===============================================================================

# libraries
install.library ("readxl")

library(readxl)
library(readr)
library(gamlss)


# Please set variables as described below

growthchart_folder_path <- "C:/Users/andre/OneDrive/Desktop/Growth_Charts"  # path for folder containing scripts

case_path <- "C:/Users/andre/OneDrive/Desktop/Growth_Charts/datasets/example_case.csv" # path to case .csv file (including file)

output_path <- "C:/Users/andre/OneDrive/Desktop/outputs" # path of desired output folder


#===============================================================================

# Creating dataframes

control_data_path <- paste(growthchart_folder_path, "/datasets/control_data.csv", sep = "")

control_data <- read.csv(control_data_path)

case_data <- read.csv(case_path)

# if formatting needed
#case_data <- ExtractSynthSeg(path, csv_path)


#-------------------------------------------------------------------------------
# 2. Show sex and demographics for control cohort data

function_demographic_plots(control_data)

#-------------------------------------------------------------------------------
# 3. Create growth charts (optional)
#       (structure-specific growth charts and trajectory comparison)

function_structure_trajectories(control_data, output_path)

#-------------------------------------------------------------------------------
# 4. Compare trajectories to control dataset

function_compare_scan(control_data, case_data, output_path)









