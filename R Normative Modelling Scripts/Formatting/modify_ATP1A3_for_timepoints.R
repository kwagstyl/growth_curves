# change dataset to single timepoint structure
    # previously used for ATP1A3 


# ===============================================================================
# Additional post-processing (1 z score row per timepoint average)

growthchart_folder_path <- "/home/meldstudent/R/Growth_Charts"  # path for folder containing scripts

cohort_data_path <- paste(output_path, "/ATP1A3_rel_z.csv", sep = "")

cohort_data <- read.csv(cohort_data_path)
head(cohort_data)

timepoints <- unique(cohort_data$Age)

iter <- as.numeric()
time_avg <- data.frame()

for (timepoint in timepoints) {
  
  print(timepoint)
  
  timepoint_data <- data.frame()
  timepoint_data <- cohort_data[cohort_data$Age == timepoint, ]
  
  head(timepoint_data)
  
  timepoint_for_avg <- subset(timepoint_data, select = seg_list)
  
  time_avg_iter <- colMeans(timepoint_for_avg)
  print(time_avg_iter)
  
  col_avgs <- as.data.frame(t(time_avg_iter))
  col_avgs$Age <- timepoint
  
  time_avg <- rbind(time_avg, col_avgs)
}

summary(time_avg)

as.data.frame(summary(time_avg))


write.csv(file = paste(output_path, "/ATP1A3_timepoint_summary.csv", sep = ""), as.data.frame(summary(time_avg)))

write.csv(file = paste(output_path, "/ATP1A3_timepoint_rel_z.csv", sep = ""), time_avg)



