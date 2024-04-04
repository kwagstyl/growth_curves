
#for every data format

library(dplyr)
library(stringr)


main_directory <- "/home/meldstudent/Documents/RDS/ATP1A3_MRI_data"


# ==============================================================================
# For any folder structure (ATP1A3 not BIDS)

extract_SynthSeg <- function(directory_scans, demographhics_csv) {
  # Assuming you want to read the CSV file into a data frame
  data <- read.csv(csv_file)
  
  # Add any additional processing steps if needed
  
  return(data)
}

subject_folders <- list.dirs(main_directory, full.names = TRUE)
head(subject_folders)
print(subject_folders)


all_files <- list.files(path = file.path(subject_folders), full.names = TRUE)
head(all_files)


# Filter files with the "_synthseg_vol.csv" suffix
synthseg_files <- all_files[grep('synthseg_vol.csv', all_files)]
head(synthseg_files)

# Loop subject folder

merged_data <- data.frame()
for (file in synthseg_files) {
  print(file)
    
    result <- extract_SynthSeg(file) # pull out volumes 
    
    file_name <- sub("_synthseg_vol.csv", "", file) # pull out file name from path
    file_name <- sub(".*/synthseg/", "", file_name)
    print(file_name)
    
    sub_name <- sub(".*MRI_data/", "", file) # pull out subject folder name
    sub_name <- sub("/.*", "", sub_name)
  
    date <- sub("/synthseg.*", "", file) # pull out date
    date <- sub(".*/", "", date)
  
    
    result$subject <- sub_name# - only subject ID
    result$file <- file_name # - only file name
    result$scan_date <- date
    
    merged_data <- rbind(merged_data, result)
  
}

print(merged_data)

write.csv(merged_data , file = paste(output_path,"Extract_ATP1A3.csv", sep = "/"))

# -------------------------------------------------------------------------------
# merge with demographics



# load data
demographics_csv <- read.csv(paste(output_path, "ATP1A3demographics.csv", sep = "/"))
raw_data_csv <- read.csv(paste(output_path,"Extract_ATP1A3.csv", sep = "/"))

head(demographics_csv)
head(raw_data_csv) 


# formatting
sub_names <- raw_data_csv$subject


for (i in seq_along(sub_names)){
  raw_data_csv$subject[i] <- sub("ATP1A3-", "", raw_data_csv$subject[i])
  
}

head(raw_data_csv)
print(raw_data_csv$subject)
 
sub_names <- unique(demographics_csv$subject)
print(sub_names)


 complete_data <- data.frame() # make df to fill 

 for (sub_name in sub_names){ # add demographics subject by subject

    print(sub_name)
    
    data_subset <- raw_data_csv[raw_data_csv$subject == sub_name, ]
    demo_subset <- demographics_csv[demographics_csv$subject == sub_name, ]
  
    print(data_subset)
    print(demo_subset)
    
    timepoints <- demo_subset$timepoint
    
    for (timepoint in timepoints) {
      
      data_time <- data_subset[data_subset$timepoint == timepoint, ]
      
      scan_nos <- data_time$scan
      
      for (scan_no in scan_nos) {
        
      
      data_scan <- data_time[data_time$scan == scan_no, ]
      demo_scan <- demo_subset[demo_subset$timepoint == timepoint, ]
      
      bound_data <- cbind(data_scan, demo_scan)
      
      complete_data <- rbind(complete_data, bound_data)
      
      }
      
    }
    
  }

head(complete_data)


ATP1A3 <- subset(complete_data, select = c("subject", "timepoint", "scan", "file", "iso_MRI", "Clinical.diagnosis", "Genetic.diagnosis", "Sex", "ATP1A3_variant", "Age", seglist))

colnames(ATP1A3)[colnames(ATP1A3) == "file"] <- "modality"

head(ATP1A3)


write.csv(ATP1A3 , file = paste(output_path,"ATP1A3_vol.csv", sep = "/"))

#===============================================================================
#--------------------------------------------
# For HS and open-source (BIDS) file structures

# Define a function to extract data from SynthSeg CSV files
extract_SynthSeg <- function(csv_file) {
  data <- read.csv(csv_file)
  return(data)
}

# Create an empty data frame to store the merged data
merged_data <- data.frame()

# Get a list of subject folders
subject_folders <- list.dirs(main_directory, full.names = FALSE)

# Loop through subject folders
for (subject_folder in subject_folders) {
  
  # Use list.files to get all files in the subject folder
  all_files <- list.files(path = file.path(main_directory, subject_folder), full.names = TRUE)
  print(all_files)
  
  # Filter files with the "_synthseg_vol.csv" suffix
  synthseg_files <- all_files[grep('_vol.csv', all_files)]
  
  print(synthseg_files)
  
  # Loop through SynthSeg files
  for (synthseg_file in synthseg_files) {
    
    if (file.exists(synthseg_file)) {
      
      # Call the extract_SynthSeg function
      result <- extract_SynthSeg(synthseg_file)
      
      # Add a column for Subject (using the subject folder name)
      result$Subject <- subject_folder
      
      # Merge the data into merged_data
      merged_data <- rbind(merged_data, result)
    }
  }
}

# Print the merged data
head(merged_data)

merged_data$file_name <- merged_data$subject


# -----------------------------------------------------------------
# format subjects for raw dataset

subject_raw <- merged_data$subject
unique_age_sex_data <- subject_raw  # [!duplicated(subject_raw)]

unique_age_sex_data <- sub("_.*", "", unique_age_sex_data)
unique_age_sex_data <- sub("^sub-", "", unique_age_sex_data)

print(unique_age_sex_data)

merged_data$Subject <- unique_age_sex_data

no_demo_cohort <- subset(merged_data, select = c("Subject", "file_name", seglist))
head(no_demo_cohort)


# ------------------------------------------------------------------------------
# format demographics csv
Demographics <- read.csv("/home/meldstudent/Documents/RDS/HS_data_MR/HS_demographics.csv")
head(Demographics)


subject_names <- Demographics$subject
subject_names  <- sub("_", "", subject_names )
subject_names  <- sub("_", "", subject_names )
subject_names  <- sub("_", "", subject_names )

head(subject_names)

Demographics$subject <- subject_names
cols <- colnames(Demographics)
#print(cols)
cols[1] <- "Subject"
#print(cols)
colnames(Demographics) <- cols
head(Demographics)


# ------------------------------------------------------------------------------
# merge datasets

HS_dataset <- merge(no_demo_cohort, Demographics, by = "Subject")

head(HS_dataset)

.keywords. <- "postop"

str_detect(HS_dataset$file_name, paste0(.keywords., collapse = '|'))

rows_postop <- which(str_detect(HS_dataset$file_name, paste0(.keywords., collapse = '|')) == TRUE)
print(rows_postop)

for (row in rows_postop) {
  
  HS_dataset <- HS_dataset[-row, ]
}


HS_dataset %>%  filter(!row_number() %in% c(rows_postop))
HS_dataset[HS_dataset == 0] <- NA
HS_dataset <- na.omit(HS_dataset)
print(HS_dataset)

HS_dataset <- subset(HS_dataset, select = c("Subject","file_name", "HS_hemisphere", "sex", "age", seglist))
head(HS_dataset)
colnames(HS_dataset)[1] <- "subject"
colnames(HS_dataset)[4] <- "Sex"
colnames(HS_dataset)[5]<- "Age"


write.csv(HS_dataset , file = paste(output_path,"HS_vol.csv", sep = "/"))


# ------------------------------------------------------------------------------
# side specific 

HS_left_vol <- HS_dataset[HS_dataset$HS_hemisphere == "L", ]
head(HS_left_vol)


HS_right_vol <- HS_dataset[HS_dataset$HS_hemisphere == "R", ]
head(HS_right_vol)

write.csv(HS_left_vol , file = paste(output_path,"HS_left_vol.csv", sep = "/"))
write.csv(HS_right_vol , file = paste(output_path,"HS_right_vol.csv", sep = "/"))









