
# This functions compares a specific case against control and extracts a z score
# for each region of interest in the segmentation list.


#====================================================================

function_extract_z <- function (control_data, case_data) {

library(dplyr)
library(gamlss)
library(mgcViz)
library(SemiPar)
library(ggplot2)
library(readr)
library(stringr)
  
head(case_data)
case_data <- na.omit(case_data)
  
# ------------------------------------------------------------------------------
  # set up datasets
  
control_data <- control_data %>%
  filter(Age <= 20)

# create male and female subsets
male_control_data <- control_data %>%
filter(Sex == "M") # male

female_control_data <- control_data %>%
  filter(Sex == "F") # female

head(male_control_data) # check 
head(female_control_data)

# set list of segmentations to compare (from controls)
seglist<- colnames(control_data)
print(seglist)


#start list of segmentation at first structure
index <- which(seglist == "left.cerebral.white.matter")

if (length(index) > 0) {
  seglist <- seglist[(index[1]):length(seglist)]
}


# remove any excesses structures not in control data
case_data <- subset(case_data, select = c("Age", "Sex", seglist))
head(case_data)


# ------------------------------------------------------------------------------

# set up data frame to fill with case data and z scores 
case_results <- data.frame(Segmentation = character(), 
                              Age = as.numeric(),
                              case_volume = as.numeric(),
                              control_mean = as.numeric(),
                              difference_from_mean = as.numeric(), 
                              z_score = as.numeric(), 
                              relative_z_score = as.numeric(),
                              stringsAsFactors = FALSE)
summary(case_results) # check its empty



# loop through structures and extract data into case results frame

for (i in seq_along(seglist)) {

  Segmentation <- as.character(seglist[i])
  print(paste("Segmentation:", Segmentation))
  
  # find values for case segmentation
  xval <- as.numeric(case_data[[Segmentation]])
  print(xval) # volume
  yval <- case_data$Age
  print(yval) # age 
  
  # make a structure specific iteration (subset)
  control_iter<- control_data %>%
    select(all_of(Segmentation), Age)
  colnames(control_iter)[1] <- "Volume"
  colnames(control_iter)[2] <- "Age"

  
  
  # create a non-linear model using the volume and age control data for the 
    # itterating structure. commented out other options for model families.
    # this uses non-linear normal (NO) distribution.
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
  
  # predict z score for the new pair of volume and age (patient case)
  zresult <- z.scores(NLMS, xval, yval)
  print(zresult)
  

  # calculate the 50th centile estimate curve for control volumes
  med_est <- gamlss(Volume ~ Age, data = control_iter)
  print(med_est)
  
  # make a dataframe for case age
  case_age <- data.frame(yval)
  colnames(case_age) <- "Age"
  head(case_age)
  
  # predict the median volume using the age and 50th centile curve
  est_vol_mod <- predict(med_est, newdata = case_age$yval, what = "mu")
  print(est_vol)
  
  # isolate single estimate value
  est_vol <- est_vol_mod[1]
  
  # calulate difference from patient volume
  vol_diff <- xval - est_vol
  print(vol_diff)
  
  # assemble all results into a list
  summaryresults <- c(Segmentation, yval, xval, est_vol, vol_diff, zresult)
  print(summaryresults)
  
  # place list in a data frame
  df_summaryresults <- data.frame(
    Segmentation = as.character(summaryresults[1]),
    Age = as.numeric(summaryresults[2]),
    case_volume = as.numeric(summaryresults[3]),
    control_mean = as.numeric(summaryresults[4]),
    difference_from_mean = as.numeric(summaryresults[5]), 
    z_score = as.numeric(summaryresults[6]),
    relative_z_score = as.numeric(summaryresults[6])
    
  )
  head(df_summaryresults) # check structure results 
  
  # merge each structure as a row in the patients results data frame
  case_results <- rbind(case_results, df_summaryresults)
  
  
}

head(case_results)
summary(case_results)


#--------------------------------------

#create relative z scores

#list all produced z scores
z_scores <- case_results$z_score

# average all z scores
avg_z <- mean(z_scores)
print(avg_z)

# calculate relative (scan-normalised) z scores
relative_z_scores <- z_scores - avg_z

# add new column to case results and check 
case_results$relative_z_score <- relative_z_scores
head(case_results)


} # end of function



