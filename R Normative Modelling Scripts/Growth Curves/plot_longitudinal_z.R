
# longitudinal z score plots




control_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/ATP1A3_timepoint_rel_z.csv")

head(control_data)



#====================================================================
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

line_list <- list()


for (Segmentation in seg_list) {
  print(paste("Segmentation:", Segmentation))
  
  iter <- subset(control_data, select = c(Segmentation, "Age"))
  
  iterforsave <- subset(control_data, select = c(Segmentation, "Age"))
  assign(paste("Subset", Segmentation, sep = ""), iterforsave)
  
  colnames(iter)[1] <- "Vol"
  colnames(iter)[2] <- "Age"
  
  # Set max values
  max_iter <- max(iter$Vol, na.rm = TRUE)
  
  # Orders
  order_iter <- order(iter$Age)
  
  fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.5)
  ft50 <- predict(fitQ50, se = TRUE)
  assign(paste("ft50", Segmentation, sep = ""), ft50)
  
  # Save the geom_line for each segmentation in the list
  line_list[[Segmentation]] <- geom_line(data = get(paste("Subset", Segmentation, sep = "")), 
                                         aes(x = Age[order_iter], y = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter], linetype = "Mean"), 
                                         alpha = 1)
}


  
  
max_val <- max(control_data)

Plot_Data <- ggplot() +
 #ylim(0,10000) +
  scale_x_log10() 
#scale_y_log10()

Plot_Data <- Plot_Data +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.cerebral.white.matter$fit[order_iter], color = "Cerebral White Matter"), alpha = 1, linetype = "solid") +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.cerebral.cortex$fit[order_iter], color = "Cerebral Cortex"), alpha = 1, linetype = "solid") +
  #geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.lateral.ventricle$fit[order_iter], color = "Lateral Ventricle"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.inferior.lateral.ventricle$fit[order_iter], color = "Inferior Lateral Ventricle"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.cerebellum.white.matter$fit[order_iter], color = "Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.cerebellum.cortex$fit[order_iter], color = "Cerebellum Cortex"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.thalamus$fit[order_iter], color = "Thalamus"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.caudate$fit[order_iter], color = "Caudate"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.putamen$fit[order_iter], color = "Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.pallidum$fit[order_iter], color = "Pallidum"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50X3rd.ventricle$fit[order_iter], color = "3rd Ventricle"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50X4th.ventricle$fit[order_iter], color = "4th Ventricle"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50brain.stem$fit[order_iter], color = "Brain Stem"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.hippocampus$fit[order_iter], color = "Left Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.amygdala$fit[order_iter], color = "Left Amygdala"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.accumbens.area$fit[order_iter], color = "Left Accumbens Area"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.ventral.DC$fit[order_iter], color = "Left Ventral DC"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.cerebral.white.matter$fit[order_iter], color = "Right Cerebral White Matter"), alpha = 1, linetype = "solid") +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.cerebral.cortex$fit[order_iter], color = "Right Cerebral Cortex"), alpha = 1, linetype = "solid") +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.lateral.ventricle$fit[order_iter], color = "Right Lateral Ventricle"), alpha = 1, linetype = "solid", size = 1) +
  
  #geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.inferior.lateral.ventricle$fit[order_iter], color = "Right Inferior Lateral Ventricle"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.cerebellum.white.matter$fit[order_iter], color = "Right Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.cerebellum.cortex$fit[order_iter], color = "Right Cerebellum Cortex"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.thalamus$fit[order_iter], color = "Right Thalamus"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.caudate$fit[order_iter], color = "Right Caudate"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.putamen$fit[order_iter], color = "Right Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.pallidum$fit[order_iter], color = "Right Pallidum"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.amygdala$fit[order_iter], color = "Right Amygdala"), alpha = 1, linetype = "solid", size = 1) +
  #geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.accumbens.area$fit[order_iter], color = "Right Accumbens Area"), alpha = 1, linetype = "solid", size = 1) +
  #geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.ventral.DC$fit[order_iter], color = "Right Ventral DC"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.hippocampus$fit[order_iter], color = "Right Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  
  scale_color_manual(values = c(
    "Right Amygdala" = "#0000FF", "Right Accumbens Area" = "#FF0000", "Right Ventral DC" = "#008000", "Left Cerebral White Matter" = "#800080", "Left Cerebral Cortex" = "#FFA500",
    "Cerebral White Matter" = "#0000FF", "Cerebral Cortex" = "#FF0000", "Lateral Ventricle" = "#008000", "Inferior Lateral Ventricle" = "#800080", "Cerebellum White Matter" = "#FFA500", 
    "Cerebellum Cortex" = "#FFC0CB", "Thalamus" = "#00FFFF", "Caudate" = "#FF00FF", "Putamen" = "#FFFF00", "Pallidum" = "#A52A2A",
    "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#FFA500", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#FFFF00", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#FFA500", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#FFFF00", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#FFA500", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#FFFF00", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Right Hippocampus" = "#00008B"
  ), name = "Structure") +
  
  guides(linetype = FALSE) +
  theme_ridges()+
  labs(title = "GMV Trajectories by Structure",
       x = "Age (years)",
       y = "z_score (not normalised)")


print(Plot_Data)




#=================================================================================================================================================================================================================


Plot_Data <- ggplot() +
ylim(-5,5)+
scale_x_log10() 


Plot_Data <- Plot_Data +
    geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.cerebellum.white.matter$fit[order_iter], color = "Left Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
    geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.putamen$fit[order_iter], color = "Left Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.pallidum$fit[order_iter], color = "Left Pallidum"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.hippocampus$fit[order_iter], color = "Left Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  
    geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.cerebellum.white.matter$fit[order_iter], color = "Right Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
   geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.putamen$fit[order_iter], color = "Right Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.pallidum$fit[order_iter], color = "Right Pallidum"), alpha = 1, linetype = "solid", size = 1) +
   geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.hippocampus$fit[order_iter], color = "Right Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  
  scale_color_manual(values = c(
    "Right Amygdala" = "#0000FF", "Right Accumbens Area" = "#FF0000", "Right Ventral DC" = "#008000", "Left Cerebral White Matter" = "#800080", "Left Cerebral Cortex" = "#FFA500",
    "Left Cerebral White Matter" = "#0000FF", "Cerebral Cortex" = "#FF0000", "Lateral Ventricle" = "#008000", "Inferior Lateral Ventricle" = "#800080", "Left Cerebellum White Matter" = "#ad0006", 
    "Cerebellum Cortex" = "#FFC0CB", "Thalamus" = "#00FFFF", "Caudate" = "#FF00FF", "Left Putamen" = "#008000", "Left Pallidum" = "#850485",
    "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#690408", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#631063", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#690408", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#FFA500", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Right Hippocampus" = "#00008B"
  ), name = "Structure") +
  
  guides(linetype = FALSE) +
  theme_ridges()+
  labs(title = "GMV Trajectories by Structure",
       x = "Age (years)",
       y = "z_score (scan normalised)")

print(Plot_Data)






Plot_Data <- ggplot() +
  ylim(-5,5)+
  scale_x_log10() 


Plot_Data <- Plot_Data +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.cerebellum.white.matter$fit[order_iter]-1.1, color = "Left Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.putamen$fit[order_iter]+0.8, color = "Left Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.pallidum$fit[order_iter]+1.75, color = "Left Pallidum"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.hippocampus$fit[order_iter]+0.3, color = "Left Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.cerebellum.white.matter$fit[order_iter]-1.2, color = "Right Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.putamen$fit[order_iter]+0.8, color = "Right Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.pallidum$fit[order_iter]+1.2, color = "Right Pallidum"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.hippocampus$fit[order_iter]+0.6, color = "Right Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
  scale_color_manual(values = c(
    "Right Amygdala" = "#0000FF", "Right Accumbens Area" = "#FF0000", "Right Ventral DC" = "#008000", "Left Cerebral White Matter" = "#800080", "Left Cerebral Cortex" = "#FFA500",
    "Left Cerebral White Matter" = "#0000FF", "Cerebral Cortex" = "#FF0000", "Lateral Ventricle" = "#008000", "Inferior Lateral Ventricle" = "#800080", "Left Cerebellum White Matter" = "#ad0006", 
    "Cerebellum Cortex" = "#FFC0CB", "Thalamus" = "#00FFFF", "Caudate" = "#FF00FF", "Left Putamen" = "#008000", "Left Pallidum" = "#850485",
    "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#690408", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#631063", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#690408", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#FFA500", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Right Hippocampus" = "#00008B"
  ), name = "Structure") +
  
  guides(linetype = FALSE) +
  theme_ridges()+
  labs(title = "ATP1A3 Relative z score by Structure",
       x = "Age (years)",
       y = "z_score (scan normalised)")

print(Plot_Data)



# ==========================================================================================================



Plot_Data <- ggplot() +
  ylim(-5,5)+
  scale_x_log10() 


Plot_Data <- Plot_Data +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.cerebellum.white.matter$fit[order_iter], color = "Left Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.putamen$fit[order_iter], color = "Left Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.pallidum$fit[order_iter], color = "Left Pallidum"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50left.hippocampus$fit[order_iter], color = "Left Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.cerebellum.white.matter$fit[order_iter], color = "Right Cerebellum White Matter"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.putamen$fit[order_iter], color = "Right Putamen"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.pallidum$fit[order_iter], color = "Right Pallidum"), alpha = 1, linetype = "solid", size = 1) +
  geom_line(data = control_data, aes(x = Age[order_iter], y = ft50right.hippocampus$fit[order_iter], color = "Right Hippocampus"), alpha = 1, linetype = "solid", size = 1) +
  
  scale_color_manual(values = c(
    "Right Amygdala" = "#0000FF", "Right Accumbens Area" = "#FF0000", "Right Ventral DC" = "#008000", "Left Cerebral White Matter" = "#800080", "Left Cerebral Cortex" = "#FFA500",
    "Left Cerebral White Matter" = "#0000FF", "Cerebral Cortex" = "#FF0000", "Lateral Ventricle" = "#008000", "Inferior Lateral Ventricle" = "#800080", "Left Cerebellum White Matter" = "#ad0006", 
    "Cerebellum Cortex" = "#FFC0CB", "Thalamus" = "#00FFFF", "Caudate" = "#FF00FF", "Left Putamen" = "#008000", "Left Pallidum" = "#850485",
    "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#690408", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#631063", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#690408", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Left Hippocampus" = "#0000FF", "Left Amygdala" = "#FF0000",
    "Left Accumbens Area" = "#000000", "Left Ventral DC" = "#808080", "Right Cerebral White Matter" = "#00008B", "Right Cerebral Cortex" = "#8B0000", "Right Lateral Ventricle" = "#006400",
    "Right Inferior Lateral Ventricle" = "#800080", "Right Cerebellum White Matter" = "#FFA500", "Right Cerebellum Cortex" = "#FFC0CB", "Right Thalamus" = "#00FFFF", "Right Caudate" = "#FF00FF",
    "Right Putamen" = "#004d0b", "Right Pallidum" = "#A52A2A", "3rd Ventricle" = "#800080", "4th Ventricle" = "#FFA500", "Brain Stem" = "#008000", "Right Hippocampus" = "#00008B"
  ), name = "Structure") +
  
  guides(linetype = FALSE) +
  theme_ridges()+
  labs(title = "Control Relative z score by Structure",
       x = "Age (years)",
       y = "z_score (scan normalised)")

print(Plot_Data)


