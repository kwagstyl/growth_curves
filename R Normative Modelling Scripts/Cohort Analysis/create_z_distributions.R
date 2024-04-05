
# This function creates 3 ridgeline plots (controls only, cohort only, and combined)
# for the given data. This then saves these plots to the output folder


# MANUAL ASSIGNMENT
control_z_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/control_z.csv")
#control_z_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/control_vol.csv")

cohort_z_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/ATP1A3__z.csv")
#cohort_z_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/ATP1A3_vol.csv")

UPPER_LIM <- as.numeric(6)
LOWER_LIM <- as.numeric(-6)





# ==============================================================================
function_z_ridgeline <- function(control_z_data, cohort_z_data, output_path) {

library(ggridges)
library(ggplot2)


# ==============================================================================
# Create ridgeline plot for comparing 

# make control dataset
ridge_control <- data.frame()
ridge_cohort <- data.frame()

for (Segmentation in seglist) {
  print(Segmentation)
  
  new_rows <- subset(control_z_data, select = c(Segmentation, "Age"))
  colnames(new_rows)[1] <- "z_score"
  colnames(new_rows)[2] <- "Cohort"
  new_rows$Segmentation <- Segmentation
  new_rows$Cohort <- "Control Cohort"
  
  print(new_rows)
  
  ridge_control <- rbind(ridge_control, new_rows)
} 

head(ridge_control)



# make cohort dataset

head(cohort_z_data)

for (Segmentation in seglist) {
  print(Segmentation)
  
  new_rows <- subset(cohort_z_data, select = c(Segmentation, "Age"))
  colnames(new_rows)[1] <- "z_score"
  colnames(new_rows)[2] <- "Cohort"
  new_rows$Segmentation <- Segmentation
  new_rows$Cohort <- "Clincial Cohort"
  
  print(new_rows)
  
  ridge_cohort <- rbind(ridge_cohort, new_rows)
} 

ridge_cohort <- ridge_cohort %>%
  filter(z_score <= (UPPER_LIM))
ridge_cohort <- ridge_cohort %>%
  filter(z_score >= (LOWER_LIM))

head(ridge_cohort)

# control only plot
ggplot(ridge_control, aes(x = z_score, y = Segmentation)) +
  geom_density_ridges(scale = 1.5, rel_min_height = 0.01, fill = "lightblue", alpha = 0.5) +
  labs(title = "Comparison of Z Distributions by Structure (Control)") +
  theme_ridges() + 
  theme(legend.position = "none")

# cohort only plot
ggplot(ridge_cohort, aes(x = z_score, y = Segmentation)) +
  geom_density_ridges(scale = 1.5, alpha = 0.5, rel_min_height = 0.01, fill = "red") +
  labs(title = "Comparison of Z Distributions by Structure (Clinical Cohort)") +
  theme_ridges() + 
  theme(legend.position = "none")

#===============================================================================
# combine both cohorts
combined_ridge <- rbind(ridge_control, ridge_cohort)
summary(combined_ridge)


# make combined plot
ggplot(combined_ridge, aes(x= z_score, y= Segmentation, group = interaction(Cohort, Segmentation), fill = Cohort)) +
  geom_density_ridges(scale = 1.75, rel_min_height = 0.01, alpha = 0.4) +
  labs(title = "Z Distributions by Structure (Control vs ATP1A3)") +
  theme_ridges() 
  #theme(legend.position = "none")


} # end of function



# =============================================================================
# FOR VOLUMES _ OUTSIDE FUNCTION
vol_ridge <- ggplot(combined_ridge, aes(x= z_score, y= Segmentation, group = interaction(Cohort, Segmentation), fill = Cohort)) +
  geom_density_ridges(scale = 1.75, rel_min_height = 0.01, alpha = 0.4) +
  labs(title = "Volume Distributions by Structure (Control vs ATP1A3)") +
  theme_ridges() 
#theme(legend.position = "none")

# =============================================================================
# sides comparison

left_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/HS_left_z.csv")
right_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/HS_right_z.csv")

# make control dataset
ridge_left <- data.frame()
ridge_right <- data.frame()


HS_list <- c("left.hippocampus", "right.hippocampus")


for (Segmentation in HS_list) {
  print(Segmentation)
  
  new_rows <- subset(left_data, select = c(Segmentation, "Age"))
  colnames(new_rows)[1] <- "z_score"
  colnames(new_rows)[2] <- "Hemisphere"
  new_rows$Segmentation <- Segmentation
  new_rows$Hemisphere <- "Left"
  
  print(new_rows)
  
  ridge_left <- rbind(ridge_left, new_rows)
} 

head(ridge_left)

# make cohort dataset
for (Segmentation in HS_list) {
  print(Segmentation)
  
  new_rows <- subset(right_data, select = c(Segmentation, "Age"))
  colnames(new_rows)[1] <- "z_score"
  colnames(new_rows)[2] <- "Hemisphere"
  new_rows$Segmentation <- Segmentation
  new_rows$Hemisphere <- "Right"
  
  print(new_rows)
  
  ridge_right <- rbind(ridge_right, new_rows)
} 

head(ridge_right)

ridge_control



# left only plot
ggplot(ridge_left, aes(x = z_score, y = Segmentation)) +
  geom_density_ridges(scale = 1.5, rel_min_height = 0.01, fill = "lightblue", alpha = 0.5) +
  labs(title = "Comparison of Z Distributions by Structure (Left)") +
  theme_ridges() + 
  theme(legend.position = "none")

# right only plot
ggplot(ridge_right, aes(x = z_score, y = Segmentation)) +
  geom_density_ridges(scale = 1.5, alpha = 0.5, rel_min_height = 0.01, fill = "red") +
  labs(title = "Comparison of Z Distributions by Structure (Right)") +
  theme_ridges() + 
  theme(legend.position = "none")

#===============================================================================
# combine both cohorts

hs_control <- ridge_control[(ridge_control$Segmentation == c("left.hippocampus", "right.hippocampus")), ]
colnames(hs_control)[colnames(hs_control) == "Cohort"] <- "Hemisphere"
hs_control


combined_ridge <- rbind(ridge_left, ridge_right, hs_control)
summary(combined_ridge)




# make combined plot
ggplot(combined_ridge, aes(x= z_score, y= Segmentation, group = interaction(Hemisphere, Segmentation), fill = Hemisphere)) +
  geom_density_ridges(scale = 1, rel_min_height = 0.01, alpha = 0.4) +
  labs(title = "Z Distributions by Structure: HS (Left vs Right vs Control)") +
  theme_ridges() 
  #theme(legend.position = "none")







