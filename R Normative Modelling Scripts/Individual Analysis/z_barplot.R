
library(ggridges)
library(ggplot2)


case_results <- read.csv("INSERT PATH TO CASE DATA")

#case_results <- read.csv("R/growth_curves-working_branch/datasets/HS_left_rel_z_sample.csv")
head(case_results)

# subset data to im
case_results <- subset(case_results, select = c(seglist))


# transform and rebuid dataframe - may be needed for some formats
t_case_results <- as.data.frame(t(case_results))
t_case_results$Segmentation <- colnames(case_results)
colnames(t_case_results) <- c("relative_z_score", "Segmentation")
head(t_case_results)
case_results <- t_case_results


# set an order of z score
ord_case <- order(case_results$relative_z_score)
case_results <- case_results[ord_case, ]

# set factor for presenting
case_results$Segmentation <- factor(case_results$Segmentation, levels = unique(case_results$Segmentation[order(case_results$relative_z_score)]))


# Barplot with ordered Segmentation z scores
ggplot(data = case_results, 
       aes(x = Segmentation, 
           y = relative_z_score, 
           fill = (relative_z_score > 2 | relative_z_score < -2))) + # flag abnormal
  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "red")) + # abnormal = red
  
  # add lines for abnormal z score thresholds
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.2) +
  geom_hline(yintercept = -2, linetype = "dashed", color = "red") +
  labs(title = "Z Scores for Segmentation",
       x = "Segmentation",
       y = "Z Score") +
  theme_ridges() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")


