# Custom look up table based on relative z scores 

growth_chart_path <- "/home/meldstudent/R/growth_curves-working_branch"
case_z_data <- read.csv(paste(growth_chart_path, "/datasets/holm_ATP1A3_linear_regression.csv", sep = ""))
head(case_z_data)

# ===============================================================================

funtion_custom_LUT <- function (case_z_data, output_path) {}

library(readr)
library(readtext)
library(quanteda)
library(RNifti)
library(oro.nifti)
library(neurobase)

# ========================================================================
# For column vectors

case_vals <- t(case_z_data)
case_vals <- subset(case_z_data, select = c(relative_z_score))
case_vals <- as.data.frame(t(case_z_data))
colnames(case_vals) <- "relative_z_scores"

print(case_vals)

# ================================================================================

# For data frames

case_vals <- subset(case_z_data, select = c("Segmentation", "Estimate")) # for linear regression weights
case_vals <- as.data.frame(case_vals)
colnames(case_vals)[colnames(case_vals) == "Estimate"] <- "relative_z_score"
print(case_vals)

# ================================================================================

nifti_file_path <- "/home/meldstudent/R/Growth_Charts/datasets/sub-MELDH4P0059_3T_preop_T1w.nii.gz"

head(nifti_data)
str(nifti_data)


corr_vals <- baseLUT
corr_vals$`Name:` <- baseLUT$`#No.`
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("-", ".", corr_vals$Label)
corr_vals$Label <- sub("_", ".", corr_vals$Label)
corr_vals <- subset(corr_vals, select = c(`Label`, `Name:`))
print(n = 20, corr_vals)



library(dplyr)

# Convert labels in corr_vals to lowercase and remove spaces
corr_vals <- corr_vals %>%
  mutate(Label = tolower(gsub("\\s", ".", Label)))

# Filter corr_vals based on the seglist
filtered_corr_vals <- corr_vals[corr_vals$Label %in% seglist, ]

# Display the filtered data frame
print(n = 40, filtered_corr_vals)

case_vals$Label <- case_vals$Segmentation

case_vals

corr_vals <- merge(filtered_corr_vals, case_vals, by = "Label",all.x = TRUE, all.y = TRUE)

corr_vals

corr_vals <- na.omit(corr_vals)

corr_vals <- subset(corr_vals, select = c("Label", "Name:", "relative_z_score"))
print(corr_vals)



# ============================================================================
library(RNifti)

# Read the NIfTI file
nifti_data <- readNifti("/home/meldstudent/R/Growth_Charts/datasets/sub-MELDH4P0059_3T_preop_T1w.nii.gz")

print(nifti_data)

print(modified_data)

modified_data <- nifti_data  # Example: Multiply all values by 2


# Replace values in the image_data based on corr_vals
for (seg_val in corr_vals$Name) {
  
  print(corr_vals$relative_z_score[corr_vals$Name == seg_val])
  print(corr_vals$Label[corr_vals$Name == seg_val])
  
  indices <- which(modified_data == seg_val, arr.ind = TRUE)
  modified_data[indices] <- corr_vals$relative_z_score[corr_vals$Name == seg_val]
}


modified_data[modified_data %% 1 == 0] <- NA
#modified_data[modified_data > 6] <- NA

# Update the data in the NIfTI object
nifti_data[] <- modified_data

# Display the structure of the nifti_data object
str(nifti_data)

# Specify the path for the output NIfTI file
output_file <- paste(growth_chart_path, "/datasets/ATP1A3_holm_corrected_mask.nii.gz", sep = "")

# Save the modified NIfTI object to a new file
writeNifti(nifti_data, output_file)
