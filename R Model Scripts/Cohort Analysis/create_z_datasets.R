
# This script sets up datasets of z scores for a specific set of volumes
# Does not require individual results precomputed - this will produce the data 
# into the desired output path for every patient.

# Requires a volume dataset for the desired cohort.

# produces data read-outs per person, then contructs the z dataset based on:
  # a. 1 scan per timepoint, 1 timepoint per patient
  # b. 1 scan per timepioint, multiple timepoints per pateint (ATP1A3)
  # c. >1 scan per timepoint, >1 timepoints per patient 

# ============================================================================

# inputs
growthchart_folder_path <- "/home/meldstudent/R/Growth_Charts"  # path for folder containing scripts

cohort_path <- "/home/meldstudent/R/Growth_Charts/datasets/ATP1A3_vol.csv" # path to case .csv file (including file)

output_path <- "/home/meldstudent/Desktop/ATP1A3_z" # path of desired output folder



# Distribution?
desired_distribution <- "z_score" # or relative_z_score

# ==============================================================================

#libraries
library(dplyr)
library(gamlss)
library(mgcViz)
library(SemiPar)
library(ggplot2)


# create avg z scored and trajectories for a cohort

# import control dataset
control_data_path <- paste(growthchart_folder_path, "/datasets/control_vol.csv", sep = "")
control_data <- read.csv(control_data_path)

# import cohort dataset and subset for desired variables 
cohort_data <- read.csv(cohort_path)
cohort_data <- subset(cohort_data, select = c("subject", "scan", "timepoint","Genetic.diagnosis", "modality", "Age", "Sex", seglist))

# set age limits based control dataset 
min_age <- min(control_data$Age)
max_age <- max(control_data$Age)

# limit the cohort data to match control age limits      
cohort_data <- cohort_data %>%
  filter(Age < max_age)
cohort_data<- cohort_data %>%
  filter(Age > min_age)

# check both datasets
head(control_data)
head(cohort_data)



# =============================================================================
# Make curves and produce a data read out for every scan in volume dataset (Cohort)


# produce normal curves for each structure
for (i in seq_along(seglist)) {
  
  Segmentation <- as.character(seglist[i])
  print(paste("Segmentation:", Segmentation))
  

  
  control_iter<- control_data %>%
    select(all_of(Segmentation), Age)
  colnames(control_iter)[1] <- "Volume"
  colnames(control_iter)[2] <- "Age"
  
  head(control_iter)
  
  
  NLMS <- lms(Volume, Age,
              data = control_iter,
              #families = c("BCCGo", "BCPEo", "BCTo", "NO", "LOG", "BCPE", "BCT"),
              families = "NO",
              k = 2 ,
              calibration = FALSE,
              trans.x = TRUE,
              cent = 50,
              control = gamlss.control(maxIter = 100000),
              plot = FALSE
  )
  
 
  med_est <- gamlss(Volume ~ Age, data = control_iter)
  #print(med_est)
  #summary(med_est)
  #plot(med_est)

  assign(paste("NLMS", Segmentation, sep = ""), NLMS)
  assign(paste("med_est_", Segmentation, sep = ""), med_est)
  
  
}

# ------------------------------------------------------------------------------
# compare every subject (and every structure) to control curves 
# FOR ONE SCAN PER PATIENT (i.e. control, HS)

# subject list
subjects <- cohort_data$subject
head(subjects)


# find z scores for each subject in the cohort dataframe
cohort_results <- list()


# loop through each subject 
for (i in seq_along(subjects)) {
  
  subject <- as.character(subjects[i])
  print(paste("subject:", subject))
  case_data <- cohort_data[cohort_data$subject == subject, ] 
  head(case_data)
  
  # --------------------- for each scan, make data subset
  
  # loop through each scan
  
  for (iter in seq_along(case_data$scan)) {
  scan_data <- data.frame()
  
  scan_no <- case_data$scan[iter]
  print(paste("scan number: ", scan_no, sep = ""))
    
  scan_data <- case_data[case_data$scan == scan_no, ]  
  head(scan_data)



  scan_results <- data.frame(subject = character(), 
                             Age = as.numeric(),
                             case_volume = as.numeric(),
                             control_mean = as.numeric(),
                             difference_from_mean = as.numeric(), 
                             z_score = as.numeric(), 
                             relative_z_score = as.numeric(),
                             stringsAsFactors = FALSE)
  
  
  
  # ------------------------ for each segmentation find results
  
  # loop through each structure
  
  for (i in seq_along(seglist)) {
    
    Segmentation <- as.character(seglist[i])
    print(paste("Segmentation:", Segmentation))
    
    # clear previous values
    summaryresults <- c()
    df_summaryresults <- data.frame()
    
    # find values for comparison
    xval <- as.numeric(scan_data[[Segmentation]])
    yval <- scan_data$Age
    
    # find z score for volume value
    zresult <- z.scores(get(paste("NLMS", Segmentation, sep = "")), xval, yval)
    print(zresult)
    
    # predict equivelent average from controls
    scan_age <- data.frame(yval)
    colnames(scan_age) <- "Age"
    est_vol_mod <- predict(get(paste("med_est_", Segmentation, sep = "")), newdata = scan_age$yval, what = "mu")
    
    # find difference between volumes
    est_vol <- est_vol_mod[1]
    vol_diff <- xval - est_vol
    
    # load into data frame
    summaryresults <- c(Segmentation, yval, xval, est_vol, vol_diff, zresult)
    #print(summaryresults)
    
    
    df_summaryresults <- data.frame(
      Segmentation = as.character(summaryresults[1]),
      Age = as.numeric(summaryresults[2]),
      case_volume = as.numeric(summaryresults[3]),
      control_mean = as.numeric(summaryresults[4]),
      difference_from_mean = as.numeric(summaryresults[5]), 
      z_score = as.numeric(summaryresults[6]),
      relative_z_score = as.numeric(summaryresults[6])
      
    )
    
    # merge all segmentations for a given case
  head(df_summaryresults)
  scan_results <- rbind(scan_results, df_summaryresults)
    
  
  }
    # ----------------------------------- for each scan - normalisation
  
  z_scores <- scan_results$z_score
  avg_z <- mean(z_scores)
  
  # find relative z
  relative_z_scores <- z_scores - avg_z
  scan_results$relative_z_score <- relative_z_scores
  
  scan_results <- subset(scan_results, select = c("Segmentation", "Age", "case_volume", "control_mean", "difference_from_mean", "z_score", "relative_z_score"))
  head(case_results)
    
  cohort_results[[paste(subject, scan_no, sep = "_")]] <- scan_results
  
  pathname <- paste(output_path, "/", subject, "_", scan_no, ".csv", sep = "")

  
  write.csv(file = paste(pathname), scan_results)
  
  }
}

  
head(cohort_results)
summary(cohort_results)



# ===============================================================================
# for ATP1A3 (>1 scan per >1 timepoint per patient)


# subject list
subjects <- unique(cohort_data$subject)
head(subjects)


# find z scores for each subject in the cohort dataframe
cohort_results <- list()

# loop through each subject volume dataset
for (i in seq_along(subjects)) {
  
  subject <- as.character(subjects[i])
  print(paste("subject:", subject))
  case_data <- cohort_data[cohort_data$subject == subject, ] 
  print(case_data)
  
  # --------------------- for each timepoint, make data subset
  for (iter in seq_along(case_data$timepoint)) {
  
    print(iter)
    
    timepoint_data <- data.frame()
    timepoint <- case_data$timepoint[iter]
    timepoint_data <- case_data[case_data$timepoint == timepoint, ]
    
    #head(timepoint_data)
    
    # --------------------- for each scan in timepoint, make data subset
    for (scan_no in seq_along(timepoint_data$scan)) {
    
    print(paste("scan number: ", scan_no, sep = ""))
    
    scan_iter <- timepoint_data$scan[scan_no]
    scan_data <- data.frame()
    scan_data <- timepoint_data[timepoint_data$scan == scan_no, ]  
    head(scan_data)
    
    
    
    scan_results <- data.frame(subject = character(), 
                               Age = as.numeric(),
                               case_volume = as.numeric(),
                               control_mean = as.numeric(),
                               difference_from_mean = as.numeric(), 
                               z_score = as.numeric(), 
                               relative_z_score = as.numeric(),
                               stringsAsFactors = FALSE)
    
    
    # ------------------------ for each segmentation find results
    
    for (i in seq_along(seglist)) {
      
      Segmentation <- as.character(seglist[i])
      #print(paste("Segmentation:", Segmentation))
      
      # clear previous values
      summaryresults <- c()
      df_summaryresults <- data.frame()
      
      # find values for comparison
      xval <- as.numeric(scan_data[[Segmentation]])
      yval <- scan_data$Age
      
      # find z score for volume value
      zresult <- z.scores(get(paste("NLMS", Segmentation, sep = "")), xval, yval)
      #print(zresult)
      
      # predict equivelent average from controls
      scan_age <- data.frame(yval)
      colnames(scan_age) <- "Age"
      est_vol_mod <- predict(get(paste("med_est_", Segmentation, sep = "")), newdata = scan_age$yval, what = "mu")
      
      # find difference between volumes
      est_vol <- est_vol_mod[1]
      vol_diff <- xval - est_vol
      
      # load into data frame
      summaryresults <- c(Segmentation, yval, xval, est_vol, vol_diff, zresult)
      #print(summaryresults)
      
      
      df_summaryresults <- data.frame(
        Segmentation = as.character(summaryresults[1]),
        Age = as.numeric(summaryresults[2]),
        case_volume = as.numeric(summaryresults[3]),
        control_mean = as.numeric(summaryresults[4]),
        difference_from_mean = as.numeric(summaryresults[5]), 
        z_score = as.numeric(summaryresults[6]),
        relative_z_score = as.numeric(summaryresults[6])
        
      )
      
      # merge all segmentations for a given case
      head(df_summaryresults)
      scan_results <- rbind(scan_results, df_summaryresults)
      
      
    }
    # ----------------------------------- for each scan - normalisation
    
    z_scores <- scan_results$z_score
    avg_z <- mean(z_scores)
    print(avg_z)
    
    # find relative z
    relative_z_scores <- z_scores - avg_z
    print(relative_z_scores)
    
    scan_results$relative_z_score <- relative_z_scores
    
    scan_results <- subset(scan_results, select = c("Segmentation", "Age", "case_volume", "control_mean", "difference_from_mean", "z_score", "relative_z_score"))
    head(scan_results)
    
    cohort_results[[paste(subject, timepoint, scan_no, sep = "_")]] <- scan_results
    
    pathname <- paste(output_path, "/", subject, "_", timepoint, "_", scan_no, ".csv", sep = "")
    
    
    write.csv(file = paste(pathname), scan_results)
    
    }
  }
}

# check results
head(cohort_results)
summary(cohort_results)



# ==============================================================================
# Reconstruct data into a z score dataset: 1 scan per patient 

# create dataframe to become cohort z score dataset
cohort_z <- data.frame()

# list all the scan data files
all_files <- list.files(path = file.path(output_path), full.names = TRUE)
print(all_files)

# of these, list .csv files for inclusion
csv_files <- all_files[grep('.csv', all_files)]
print(csv_files)

# loop through each data read-out per patient
for (i in seq_along(csv_files)) {
  
  # find file name and path
  file_name <- as.character(csv_files[i])
  print(paste("file_name:", file_name))
  
  # read data for patient
  subject_results <- read.csv(file_name)
 
  # extract only data and format
  subject_results <- t(subject_results)
  subject_results <- subject_results[2:nrow(subject_results), ]
  
  subject_results <- as.data.frame(subject_results)
  # subject_results <- subset(subject_results, select = c("Segmentation", desired_distribution))
  head(subject_results)
  
  
  
  # for each subject dataset, extract a new set to add to the 
  subject_iter <- data.frame(
    #Segmentation = as.character(subject_results[1,]),
    z_score = as.numeric(subject_results[6,])
    )
  
  cols <- colnames(subject_iter) 
  cols[cols == "z_score"] <- subject
  #cols[cols == "Segmentation"] <- "subject"
  colnames(subject_iter) <- cols
  subject_iter <- t(subject_iter)
  
  
  # check data and bind new row to dataset
  head(subject_iter)
  cohort_z <- rbind(cohort_z, subject_iter)
  
  
}

# set as dataframe from list and check for data appearing
as.data.frame(cohort_z)
head(cohort_z)


# loop again through each file and attach its age to the dataset
z_age <- list()

for (i in seq_along(csv_files)) {
  
  file_name <- as.character(csv_files[i])
  print(paste("file_name:", file_name))
  
  subject_results <- read.csv(file_name)
  
  z_age[i] <- subject_results$Age[1]
  
}

head(z_age) # check ages


# set ages as df
z_age <- as.data.frame(z_age)
z_age <- t(z_age)
colnames(z_age) <- "Age"
head(z_age)

# bind ages
cohort_z <- cbind(cohort_z, z_age)

# remove infinite vals
cohort_z[cohort_z == -Inf] <- NA
cohort_z <- na.omit(cohort_z)

colnames(cohort_z) <- c(seglist, "Age")

# check for NA or inf values
summary(cohort_z)

# analyse data breifly
summary(cohort_z)
print(cohort_z)

# save
write.csv(file = paste(output_path, "/HS_left_z.csv", sep = ""), cohort_z)




# ==============================================================================
# Reconstruct data into a z score dataset: >1 scan per patient 
# functions similarly to set above

cohort_z <- data.frame()

all_files <- list.files(path = file.path("/home/meldstudent/Desktop/ATP1A3_z"), full.names = TRUE)
print(all_files)

csv_files <- all_files[grep('.csv', all_files)]
print(csv_files)


# for >1 scan per patient
for (i in seq_along(csv_files)) {
  
  subject_results <- data.frame()
  timepoint <- as.numeric()
  scan_no <- as.numeric()
  
  file_name <- as.character(csv_files[i])
  print(paste("file_name:", file_name))
  
  subject_results <- read.csv(file_name)
  
  
  subject_results <- t(subject_results)
  subject_results <- subject_results[2:nrow(subject_results), ]
  
  subject_results <- as.data.frame(subject_results)
  # subject_results <- subset(subject_results, select = c("Segmentation", desired_distribution))
  
  head(subject_results)
  
  subject_iter <- data.frame(
    z_score = as.numeric(subject_results[7,]) # 6 z, 7 rel z
  )
  
  cols <- colnames(subject_iter) 
  cols[cols == "z_score"] <- subject
  #cols[cols == "Segmentation"] <- "subject"
  colnames(subject_iter) <- cols
  subject_iter <- t(subject_iter)
  

  timepoint <- sub(".csv", "",  file_name)
  timepoint <- sub(".*GOSH", "",  timepoint)
  timepoint <- sub("-", "",  timepoint)
  
  print(timepoint)
  
  subject_iter <- as.data.frame(subject_iter) 
  subject_iter$scan_id <- timepoint
    
  head(subject_iter)
  
  
  cohort_z <- rbind(cohort_z, subject_iter)
  
  
}

# set as dataframe from list and check for data appearing
as.data.frame(cohort_z)
head(cohort_z)
view(cohort_z)



z_age <- list()

for (i in seq_along(csv_files)) {
  
  file_name <- as.character(csv_files[i])
  print(paste("file_name:", file_name))
  
  subject_results <- read.csv(file_name)
  
  z_age[i] <- subject_results$Age[1]
  
}

head(z_age)


# set ages as df
z_age <- as.data.frame(z_age)
z_age <- t(z_age)
colnames(z_age) <- "Age"
head(z_age)

# bind ages
cohort_z <- cbind(cohort_z, z_age)

# remove infinite vals
cohort_z[cohort_z == -Inf] <- NA
cohort_z <- na.omit(cohort_z)

colnames(cohort_z) <- c(seglist, "scan_tag", "Age")

# check for NA or inf values
summary(cohort_z)

str(cohort_z)
head(cohort_z)
summary(cohort_z)
print(cohort_z)

write.csv(file = paste(output_path, "/ATP1A3_rel_z.csv", sep = ""), cohort_z)


