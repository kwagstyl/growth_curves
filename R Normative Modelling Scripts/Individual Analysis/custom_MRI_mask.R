# create a custom NIFTI mask from an individual patient MRI and z case

# z case structures similarly to z dataset with single line

growth_chart_path <- "/home/meldstudent/R/growth_curves-working_branch"

# import case datast
case_z_data <- read.csv(paste(growth_chart_path, 
                              "/datasets/sample_control_z.csv", sep = ""))
# subset case dataset
case_z_data <- subset(case_z_data, select = c(seglist))
head(case_z_data) # check data


# ===============================================================================
# start function and libraries

funtion_custom_LUT <- function (case_z_data, output_path) {}

# libraries
library(dplyr)
library(readr)
library(readtext)
library(quanteda)
library(RNifti)
library(oro.nifti)
library(neurobase)

# ========================================================================
# For column vectors (individual case)

# turn horizontal data into vertical dataframe
case_vals <- t(case_z_data)
case_vals <- subset(case_z_data, select = c(relative_z_score))
case_vals <- as.data.frame(t(case_z_data))
colnames(case_vals) <- "relative_z_scores"

# check data
print(case_vals)

# ================================================================================
# for linear regression weights only - difference in column labels

# For data frames
case_vals <- subset(case_z_data, select = c("Segmentation", "Estimate")) 
case_vals <- as.data.frame(case_vals)
colnames(case_vals)[colnames(case_vals) == "Estimate"] <- "relative_z_score"

colnames(case_vals) <- c("structure", "relative_z_score")
print(case_vals)

case_vals <- data.frame(Segmentation = c(seglist),
                        relative_z_scores = case_vals$relative_z_scores)


# ================================================================================
# Read the NIfTI file for the patient case MRI
nifti_data <- readNifti("/home/meldstudent/R/growth_curves-working_branch/datasets/sub-pixar002_T1w.nii.gz")

# check data structure
head(nifti_data)
str(nifti_data)

# import the base look up table and modify it for use
corr_vals <- baseLUT
corr_vals$`Name:` <- baseLUT$`#No.`
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("_", ".", corr_vals$Label)
corr_vals <- subset(corr_vals, select = c(`Label`, `Name:`))
print(n = 20, corr_vals) # print first 20


# Convert labels in corr_vals to lowercase and remove spaces
corr_vals <- corr_vals %>%
  mutate(Label = tolower(gsub("\\s", ".", Label)))


# Filter corr_vals based on the seglist
filtered_corr_vals <- corr_vals[corr_vals$Label %in% seglist, ]

# Display the filtered data frame
print(n = 40, filtered_corr_vals)

case_vals <- as.data.frame(case_vals)
case_vals$Label <- case_vals$Segmentation
# case_vals includes the z score information by structure
# - to be merged with label information in look up table.


# assign numerical label to each structure so grey value can be replaced with z.
corr_vals <- merge(filtered_corr_vals, case_vals, by = "Label",all.x = TRUE, all.y = TRUE)
corr_vals <- na.omit(corr_vals)

# subset and check output
corr_vals <- subset(corr_vals, select = c("Label", "Name:", "relative_z_scores"))
print(corr_vals)



# ============================================================================

# Read the NIfTI file
nifti_data <- readNifti("/home/meldstudent/R/growth_curves-working_branch/datasets/sub-pixar002_T1w.nii.gz")

modified_data <- nifti_data # Example: Multiply all values by 2 here if wanted


# Replace values in the image_data based on corr_vals
for (seg_val in corr_vals$Name) {
  
  print(corr_vals$relative_z_score[corr_vals$Name == seg_val])
  print(corr_vals$Label[corr_vals$Name == seg_val])
  
  indices <- which(modified_data == seg_val, arr.ind = TRUE)
  modified_data[indices] <- corr_vals$relative_z_score[corr_vals$Name == seg_val]
}

# make all exact integers into NA
modified_data[modified_data %% 1 == 0] <- NA

# make all extreme (error) values (+/- <6 sigma) into NA 
modified_data[modified_data > 6] <- NA

# Update the data in the NIfTI object
nifti_data[] <- modified_data


# Display the structure of the nifti_data object
str(nifti_data)
summary(nifti_data)

# Specify the path for the output NIfTI file
output_file <- paste(growth_chart_path, "/datasets/control_sample_mask.nii.gz", sep = "")


# Save the modified NIfTI object to a new file
writeNifti(nifti_data, output_file)
