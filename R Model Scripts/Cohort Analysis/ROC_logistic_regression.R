
# Create ROC curves for individual strucutrues and produce individual structure
# logistic regression outputs

# libraries
library(movieROC)
library(ggplot2)
library(pROC)
library(plotROC)
library(pROC)

# ==========================================================================
# Load data for single LR and ROC curves and reformat into long data frames

control_vol <- read.csv("//ad.ucl.ac.uk/homen/zchaeen/
                    DesktopSettings/Desktop/270324/270324/Data_Final/control_vol.csv")
control_raw_z <- read.csv("//ad.ucl.ac.uk/homen/zchaeen/
                    DesktopSettings/Desktop/270324/270324/Data_Final/control_z.csv")
control_rel_z <- read.csv("//ad.ucl.ac.uk/homen/zchaeen/
                    DesktopSettings/Desktop/270324/270324/Data_Final/control_z.csv")

cohort_vol  <- read.csv("//ad.ucl.ac.uk/homen/zchaeen/
                  DesktopSettings/Desktop/270324/270324/Data_Final/HS_left_vol.csv")
cohort_raw_z  <- read.csv("//ad.ucl.ac.uk/homen/
                  zchaeen/DesktopSettings/Desktop/270324/270324/Data_Final/HS_left_z.csv")
cohort_rel_z  <- read.csv("//ad.ucl.ac.uk/homen/zchaeen/
                  DesktopSettings/Desktop/270324/270324/Data_Final/HS_left_rel_z.csv")

# select structure
Segmentation <- "left.hippocampus"

# control data ----------------------------------------------------------------

con_vol_sub <- data.frame()[1:nrow(control_vol), ]
con_vol_sub$Value <- control_vol$left.hippocampus
con_vol_sub$Cohort[1:nrow(control_vol)] <- 0
con_vol_sub$Data[1:nrow(control_vol)] <- "volume"
con_vol_sub$Segmentation[1:nrow(control_vol)] <- "left.hippocampus"
con_vol_sub <- subset(con_vol_sub, select = c("Cohort", "Data", "Value", "Segmentation"))

con_z_sub <- data.frame()[1:nrow(control_raw_z), ]
con_z_sub$Value <- control_raw_z$left.hippocampus
con_z_sub$Cohort[1:nrow(control_raw_z)] <- 0
con_z_sub$Data[1:nrow(control_raw_z)] <- "raw_z"
con_z_sub$Segmentation[1:nrow(control_raw_z)] <- "left.hippocampus"
con_z_sub <- subset(con_z_sub, select = c("Cohort", "Data", "Value", "Segmentation"))

con_rel_z_sub <- data.frame()[1:nrow(control_rel_z), ]
con_rel_z_sub$Value <- control_rel_z$left.hippocampus
con_rel_z_sub$Cohort[1:nrow(control_rel_z)] <- 0
con_rel_z_sub$Data[1:nrow(control_rel_z)] <- "rel_z"
con_rel_z_sub$Segmentation[1:nrow(control_rel_z)] <- "left.hippocampus"
con_rel_z_sub <- subset(con_rel_z_sub, select = c("Cohort", "Data", "Value", "Segmentation"))

# cohort data ------------------------------------------------------------------

coh_vol_sub <- data.frame()[1:nrow(cohort_vol), ]
coh_vol_sub$Value <- cohort_vol$left.hippocampus
coh_vol_sub$Cohort[1:nrow(cohort_vol)] <- 1
coh_vol_sub$Data[1:nrow(cohort_vol)] <- "volume"
coh_vol_sub$Segmentation[1:nrow(cohort_vol)] <- "left.hippocampus"
coh_vol_sub <- subset(coh_vol_sub, select = c("Cohort", "Data", "Value", "Segmentation"))


coh_z_sub <- data.frame()[1:nrow(cohort_raw_z), ]
coh_z_sub$Value <- cohort_raw_z$left.hippocampus
coh_z_sub$Cohort[1:nrow(cohort_raw_z)] <- 1
coh_z_sub$Data[1:nrow(cohort_raw_z)] <- "raw_z"
coh_z_sub$Segmentation[1:nrow(cohort_raw_z)] <- "left.hippocampus"
coh_z_sub <- subset(coh_z_sub, select = c("Cohort", "Data", "Value", "Segmentation"))

coh_rel_z_sub <- data.frame()[1:nrow(cohort_rel_z), ]
coh_rel_z_sub$Value <- cohort_rel_z$left.hippocampus
coh_rel_z_sub$Cohort[1:nrow(cohort_rel_z)] <- 1
coh_rel_z_sub$Data[1:nrow(cohort_rel_z)] <- "rel_z"
coh_rel_z_sub$Segmentation[1:nrow(cohort_rel_z)] <- "left.hippocampus"
coh_rel_z_sub <- subset(coh_rel_z_sub, select = c("Cohort", "Data", "Value", "Segmentation"))


# bind all data for overall set
ROC_data <- rbind(con_vol_sub, con_z_sub, con_rel_z_sub, coh_vol_sub, coh_z_sub, coh_rel_z_sub)

# bind data to compare AUC of seperate curves
ROC_vol <- rbind(con_vol_sub,coh_vol_sub)
ROC_z <- rbind(con_rel_z_sub,coh_rel_z_sub)
ROC_relz <- rbind(con_z_sub,coh_z_sub)

# check data formatting 
ROC_data$Cohort <- as.numeric(ROC_data$Cohort)
head(ROC_data)
summary(ROC_data)

# ==========================================================================
# Threshold only ROC for 1 structure: Left hippocampus in Left HS

# Plot all ROC rough data on top of one another
ggplot(ROC_data, aes(d = Cohort, m = Value, color = Data)) +  
  geom_roc(increasing = FALSE) + style_roc()

summary(ROC_data) # check structure if any issues


# Seperate ROC curves per data type
roc_vol <- gROC(X = ROC_vol$Value, D = ROC_vol$Cohort) # volume
plot_densityROC(roc_vol)

roc_relz <- gROC(X = ROC_relz$Value, D = ROC_relz$Cohort) # relative z
plot_densityROC(roc_relz)

roc_z <- gROC(X = ROC_z$Value, D = ROC_z$Cohort) # raw z
plot_densityROC(roc_z)


# pull out AUC values for each data type
rocvol <- roc(ROC_vol$Cohort, ROC_vol$Value, color = Data) # volume
rocvol
rocz <- roc(ROC_z$Cohort, ROC_z$Value, color = Data) # volume
rocz
roc_relz <- roc(ROC_relz$Cohort, ROC_relz$Value, color = Data) # volume
roc_relz








# =========================================================================
# logistical regression models: Load dataset

# control data
LR_con_z <- subset(control_raw_z, select = c(seglist))
LR_con_z$cohort[1:nrow(control_raw_z)] <- 0
head(LR_con_z)

# HS left
LR_HS_L   <- read.csv(paste(growth_chart_path, "/datasets/HS_right_rel_z.csv", sep = ""))
LR_HS_L <- subset(LR_HS_L , select = c(seglist))
LR_HS_L $cohort[1:nrow(LR_HS_L)] <- 1
head(LR_HS_L)

# HS right
LR_HS_R <-read.csv(paste(growth_chart_path, "/datasets/HS_left_rel_z.csv", sep = ""))
LR_HS_R <- subset(LR_HS_R , select = c(seglist))
LR_HS_R$cohort[1:nrow(LR_HS_R)] <- 2
head(LR_HS_R)

# Bind datasets
LR_dat <- rbind(LR_con_z, LR_HS_L, LR_HS_R)
summary(LR_dat)

# Clean
LR_dat <- na.omit(LR_dat)
LR_dat <- LR_dat[!is.infinite(rowSums(LR_dat)),]
head(LR_dat)

# Assign data 
data <- LR_dat
str(data)


# ==============================================================================

# Initialize lists to store estimates and p-values
linear_estimate <- list()
linear_p <- list()

for (Segmentation in seglist) {
  lm_model <- lm(get(Segmentation) ~ cohort, data = data)
  
  # Extract estimates and p-values
  estimate <- summary(lm_model)$coefficients[2, 1]
  p_value <- summary(lm_model)$coefficients[2, 4]
  
  # Store in lists
  linear_estimate[[Segmentation]] <- estimate
  linear_p[[Segmentation]] <- p_value
}

# Convert lists to data frames
linear_estimate_df <- data.frame(Segmentation = names(linear_estimate), Estimate = unlist(linear_estimate))
linear_p_df <- data.frame(Segmentation = names(linear_p), P_Value = unlist(linear_p))

# Print the data frames
print(linear_estimate_df)
print(linear_p_df)

linear_results <- cbind(linear_estimate_df, linear_p_df)
linear_results <- subset(linear_results, select = c("Segmentation", "Estimate", "P_Value"))


write.csv(linear_results, file = "/home/meldstudent/Desktop/linear_est_z.csv", row.names = FALSE)




