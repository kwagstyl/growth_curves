
# libraries
library(dplyr)
library(gamlss)
library(mgcViz)
library(SemiPar)
library(ggplot2)
library(gamlss)
library(cultevo)
library(ggridges)

control_data <- control_data %>%
  filter(Age < 20)



# ===============================================================================

control_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/control_vol.csv")
head(control_data)

cohort_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/ATP1A3_timepoint_vol.csv")
cohort_data <- as.data.frame(HS_IPSI_CONTRA_vol)[, 2:ncol(HS_IPSI_CONTRA_vol)]
head(cohort_data)
#====================================================================

order_control <- order(control_data$Age)

for (Segmentation in seg_list) {
  print(paste("Segmentation:", Segmentation))
  
  iter <- subset(control_data, select = c(Segmentation, "Age"))
  
  iterforsave <- subset(control_data, select = c(Segmentation, "Age"))
  assign(paste("ASubset", Segmentation, sep = ""), iterforsave)
  
  
  
  colnames(iter)[1] <- "Vol"
  colnames(iter)[2] <- "Age"
  
  fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.5)
  ft50 <- predict(fitQ50, se = TRUE)
  assign(paste("Aft50", Segmentation, sep = ""), ft50)
  
  fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.05)
  ft50 <- predict(fitQ05, se = TRUE)
  assign(paste("Aft05", Segmentation, sep = ""), ft05)
  
  fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.95)
  ft50 <- predict(fitQ95, se = TRUE)
  assign(paste("Aft95", Segmentation, sep = ""), ft95)
  
}



order_cohort <- order(cohort_data$Age)

for (Segmentation in seg_list) {
  print(paste("Segmentation:", Segmentation))
  
  iter <- subset(cohort_data, select = c(Segmentation, "Age"))
  
  iterforsave <- subset(cohort_data, select = c(Segmentation, "Age"))
  assign(paste("BSubset", Segmentation, sep = ""), iterforsave)
  
  colnames(iter)[1] <- "Vol"
  colnames(iter)[2] <- "Age"
  
  fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.5)
  ft50 <- predict(fitQ50, se = TRUE)
  assign(paste("Bft50", Segmentation, sep = ""), ft50)
  
}

HS_list <- c("contralateral.hippocampus", "ipsilateral.hippocampus")

for (Segmentation in HS_list) {
  print(paste("Segmentation:", Segmentation))
  
  iter <- subset(cohort_data, select = c(Segmentation, "Age"))
  
  iterforsave <- subset(cohort_data, select = c(Segmentation, "Age"))
  assign(paste("BSubset", Segmentation, sep = ""), iterforsave)
  
  colnames(iter)[1] <- "Vol"
  colnames(iter)[2] <- "Age"
  
  fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.5)
  ft50 <- predict(fitQ50, se = TRUE)
  
  assign(paste("Bft50", Segmentation, sep = ""), ft50)
  
}

# =============================================================================





Plot_Data <- ggplot(control_data, aes(x = Age[order_control])) +
  geom_point(aes(y = left.cerebellum.white.matter), size = 0.5, alpha = 0.5, color = "darkred") +
  geom_line(aes(y = Aft50left.cerebellum.white.matter$fit[order_control], color = "Left Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(aes(x = Age[Aorder_iter], y = Aft50right.cerebellum.white.matter$fit[Aorder_iter], color = "Right Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_ribbon(aes(ymin = (Aft50left.cerebellum.white.matter$fit[order_control] - 2 * Aft50left.cerebellum.white.matter$se.fit[order_control]),
                  ymax = (Aft50left.cerebellum.white.matter$fit[order_control] + 2 * Aft50left.cerebellum.white.matter$se.fit[order_control])),
              fill = "darkred", alpha = 0.4) +
  geom_ribbon(aes(ymin = (Aft50right.cerebellum.white.matter$fit[order_control] - 2 * Aft50right.cerebellum.white.matter$se.fit[order_control]),
                  ymax = (Aft50right.cerebellum.white.matter$fit[order_control] + 2 * Aft50right.cerebellum.white.matter$se.fit[order_control])),
              fill = "blue", alpha = 0.4) +
  scale_color_manual(values = c("Left Cerebellum White Matter" = "#ad0006", "Right Cerebellum White Matter" = "#690408"), name = "Structure") +
  guides(linetype = FALSE) +
  theme_ridges() +
  labs(title = "GMV Trajectories by Structure",
       x = "Age (years)",
       y = "z_score (scan normalized)") +
  ylim(-5, 5) +
  scale_x_log10()

print(Plot_Data)

#######################################

# CEREBELLAR WHITE MATTER

Plot_Data <- ggplot() +
  geom_point(data = control_data, aes(x = Age[order_control], y = left.cerebellum.white.matter), size = 0, alpha = 0, color = "darkred") +
  geom_line(aes(x = control_data$Age[order_control], y = Aft50left.cerebellum.white.matter$fit[order_control], color = "Control Left Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(aes(x = control_data$Age[order_control], y = Aft50right.cerebellum.white.matter$fit[order_control], color = "Control Right Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_ribbon(aes(x = control_data$Age[order_control],
                  ymin = (Aft50left.cerebellum.white.matter$fit[order_control] - 1 * Aft50left.cerebellum.white.matter$se.fit[order_control]),
                  ymax = (Aft50left.cerebellum.white.matter$fit[order_control] + 1 * Aft50left.cerebellum.white.matter$se.fit[order_control])),
              fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(x = control_data$Age[order_control], 
                  ymin = (Aft50right.cerebellum.white.matter$fit[order_control] - 1 * Aft50right.cerebellum.white.matter$se.fit[order_control]),
                  ymax = (Aft50right.cerebellum.white.matter$fit[order_control] + 1 * Aft50right.cerebellum.white.matter$se.fit[order_control])),
              fill = "pink", alpha = 0.2) +
  
  
  geom_point(data = cohort_data, aes(x = Age[order_cohort], y = left.cerebellum.white.matter), size = 0, alpha = 0, color = "darkblue") +
  geom_line(aes(x = cohort_data$Age[order_cohort], y = Bft50left.cerebellum.white.matter$fit[order_cohort] - 1.1, color = "Cohort Left Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(aes(x = cohort_data$Age[order_cohort], y = Bft50right.cerebellum.white.matter$fit[order_cohort] - 1.2, color = "Cohort Right Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_ribbon(aes(x = cohort_data$Age[order_cohort],
                  ymin = (Bft50left.cerebellum.white.matter$fit[order_cohort] - 1 * Bft50left.cerebellum.white.matter$se.fit[order_cohort]) - 1.1,
                  ymax = (Bft50left.cerebellum.white.matter$fit[order_cohort] + 1 * Bft50left.cerebellum.white.matter$se.fit[order_cohort]) - 1.1),
              fill = "darkblue", alpha = 0.2) +
  geom_ribbon(aes(x = cohort_data$Age[order_cohort], 
                  ymin = (Bft50right.cerebellum.white.matter$fit[order_cohort] - 1 * Bft50right.cerebellum.white.matter$se.fit[order_cohort]) - 1.2,
                  ymax = (Bft50right.cerebellum.white.matter$fit[order_cohort] + 1 * Bft50right.cerebellum.white.matter$se.fit[order_cohort]) - 1.2),
              fill = "darkcyan", alpha = 0.2) +

  geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Control Left Cerebellum White Matter" = "darkred", "Control Right Cerebellum White Matter" = "pink",
                                "Cohort Left Cerebellum White Matter" = "darkblue", "Cohort Right Cerebellum White Matter" = "darkcyan"
                                ), name = "Structure") +
  guides(linetype = FALSE) +
  theme_ridges() +
  labs(title = "Volume Trajectories for Left and Right Cerebellar White Matter (ATP1A3)",
       x = "Age (years)",
       y = "Segmentation Volume") +
  #ylim(-3, 3) +
  xlim(4, 15)

print(Plot_Data)
  

#############################
    
    
    
Plot_Data <- ggplot() +
  geom_point(data = control_data, aes(x = Age[order_control], y = left.hippocampus), size = 0, alpha = 0, color = "darkred") +
  geom_line(aes(x = control_data$Age[order_control], y = Aft50left.hippocampus$fit[order_control], color = "Control Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(aes(x = control_data$Age[order_control], y = Aft50right.hippocampus$fit[order_control], color = "Control Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  geom_ribbon(aes(x = control_data$Age[order_control],
                  ymin = (Aft50left.hippocampus$fit[order_control] - 1 * Aft50left.hippocampus$se.fit[order_control]),
                  ymax = (Aft50left.hippocampus$fit[order_control] + 1 * Aft50left.hippocampus$se.fit[order_control])),
              fill = "darkred", alpha = 0.2) +
  geom_ribbon(aes(x = control_data$Age[order_control], 
                  ymin = (Aft50right.hippocampus$fit[order_control] - 1 * Aft50right.hippocampus$se.fit[order_control]),
                  ymax = (Aft50right.hippocampus$fit[order_control] + 1 * Aft50right.hippocampus$se.fit[order_control])),
              fill = "pink", alpha = 0.2) +
  
  
  geom_point(data = cohort_data, aes(x = Age[order_cohort], y = left.hippocampus), size = 0, alpha = 0, color = "darkblue") +
  geom_line(aes(x = cohort_data$Age[order_cohort], y = Bft50left.hippocampus$fit[order_cohort], color = "Cohort Left Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(aes(x = cohort_data$Age[order_cohort], y = Bft50right.hippocampus$fit[order_cohort], color = "Cohort Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_ribbon(aes(x = cohort_data$Age[order_cohort],
                  ymin = (Bft50left.hippocampus$fit[order_cohort] - 1 * Bft50left.hippocampus$se.fit[order_cohort]),
                  ymax = (Bft50left.hippocampus$fit[order_cohort] + 1 * Bft50left.hippocampus$se.fit[order_cohort])),
              fill = "darkblue", alpha = 0.2) +
  geom_ribbon(aes(x = cohort_data$Age[order_cohort], 
                  ymin = (Bft50right.hippocampus$fit[order_cohort] - 1 * Bft50right.hippocampus$se.fit[order_cohort]),
                  ymax = (Bft50right.hippocampus$fit[order_cohort] + 1 * Bft50right.hippocampus$se.fit[order_cohort])),
              fill = "darkcyan", alpha = 0.2) +
  
  geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Control Left Hippocampus" = "darkred", "Control Hippocampus" = "pink",
                                "Cohort Left Hippocampus" = "darkblue", "Cohort Hippocampus" = "darkcyan"
  ), name = "Structure") +
  guides(linetype = FALSE) +
  theme_ridges() +
  labs(title = "Z Trajectories for Left and Right Hippocampus (ATP1A3)",
       x = "Age (years)",
       y = "z_score (scan normalized)") +
  ylim(-3, 3) +
  xlim(4, 15)

print(Plot_Data)

    
    
    
  
  
  
  
  
  