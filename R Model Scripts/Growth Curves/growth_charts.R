# This script creates GAMLSS centile-based growth curves controlled for sex.
# This require the control_vol.csv file in the growth charts folder

# ==============================================================================
# Load data for manual use (outwith function)

# Control data
control_data <- read.csv("/home/meldstudent/R/Growth_Charts/datasets/control_vol.csv")
head(control_data)

#optional line to log scale age:
  #control_data$Age <- log10(control_data$Age)

# Output folder path
output_path <- "/home/meldstudent/Desktop"

#===============================================================================
# Begin function and libraries
function_growth_charts <- function(control_data, output_path){
  
  
# libraries
  library(dplyr)
  library(gamlss)
  library(mgcViz)
  library(SemiPar)
  library(ggplot2)
  library(gamlss)
  library(cultevo)
  library(ggridges)
  
#===============================================================================
# Filter and create male and female sets
 
  control_data <- control_data %>%
    filter(Age < 20)
  
  # create male and female subsets
  male_control_data <- control_data %>%
    filter(Sex == "M")
  
  female_control_data <- control_data %>%
    filter(Sex == "F")
  
  head(male_control_data)
  head(female_control_data)
  
  
#===============================================================================
  # create segmentation list
  seglist<- colnames(control_data)
  
  #start list of segmentation at first structure
  index <- which(seglist == "left.cerebral.white.matter")
  
  if (length(index) > 0) {
    seglist <- seglist[(index[1]):length(seglist)]
  }
  # print list of segmentation names
  print(seglist)
  
  
#===============================================================================
  # ensure only Age, Sex and segmentation are included
  
  # Overall subset
  controlsubset <- subset(control_data, select = c("Age", "Sex", seg_list))
  head(controlsubset)
  
  
  # Male only subset
  malesubset <- subset(male_control_data, select = c("Age", "Sex", seg_list))
  head(malesubset)
  
  # Female only subset
  femalesubset <- subset(female_control_data, select = c("Age", "Sex", seg_list))
  head(femalesubset)
  
  
# =============================================================================
 # Plot combined (male + female) growth charts

    # loop through each structure in the dataset and produce curves
    for (i in seq_along(seglist)) {
    
    # set current segmentation 
    Segmentation <- as.character(seglist[i])
    print(paste("Segmentation:", Segmentation))
    
    # create subset for that segmentation from control data
    iter <- subset(control_data, select = c(Segmentation, "Age")) 
    colnames(iter)[1] = "Vol"
    colnames(iter)[2] = "Age"
    
    
    # set max values
    max_iter <- max(iter$Vol, na.rm = TRUE)
    
    #orders
    order_iter <- order(iter$Age)
    
    
    # Create GAMLSS curves (not saved between iterations)
    # 5th, 10th, 25th, 50th, 75th, 90th, 95th centiles
    fitQ05 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.05,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ10 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.1,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ25 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.25,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.5, 
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ75 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.75,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ90 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.9,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ95 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.95,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    
    # Create predictions for centiles
    ft05 <- predict(fitQ05, se = TRUE)
    ft10 <- predict(fitQ10, se = TRUE)
    ft25 <- predict(fitQ25, se = TRUE)
    ft50 <- predict(fitQ50, se = TRUE)
    ft75 <- predict(fitQ75, se = TRUE)
    ft90 <- predict(fitQ90, se = TRUE)
    ft95 <- predict(fitQ95, se = TRUE)
    
    # Save centiles for each segmentation
    assign(paste("ft05", Segmentation, sep = ""), ft05)
    assign(paste("ft10", Segmentation, sep = ""), ft10)
    assign(paste("ft25", Segmentation, sep = ""), ft25)
    assign(paste("ft50", Segmentation, sep = ""), ft50)
    assign(paste("ft75", Segmentation, sep = ""), ft75)
    assign(paste("ft90", Segmentation, sep = ""), ft90)
    assign(paste("ft95", Segmentation, sep = ""), ft95)
    
    
    # create basic scatter plot
    Plot_Iter <- ggplot() +
      geom_point(data = iter, aes(x = Age, y = Vol), color = "grey", size = 0.5) +
      
      # set centile lines (5, 10, 25, 50, 75, 90, 95)
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft05", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter]), color = "darkgrey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter]), color = "black", size = 1,  alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter]), color = "darkgrey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft95", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      
      # create colours between centile lines
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft05", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter]), fill = "darkred", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter]), fill = "orange", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter]), fill = "darkgreen", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter]), fill = "darkgreen", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter]), fill = "orange", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft95", Segmentation, sep = ""))$fit[order_iter]), fill = "darkred", alpha = 0.4) +
      
      # titles and labels
      labs(title = Segmentation, x = "Age (years)", y = "GMV (mm^3)") +
      theme_ridges() +
      ylim(0.4*max_iter, 1.25*max_iter)
    
    # print
    print(Plot_Iter)
    
    # save to output path
    filename <- paste(output_path, "/", "Combined_", Segmentation, ".png", sep = "")
    print(filename)
    png(filename= filename)
    plot(Plot_Iter)
    dev.off()
    
  }
  
  
  # =============================================================================
  # Plot MALE ONLY growth charts
  
  
  for (i in seq_along(seglist)) {
    
    Segmentation <- as.character(seglist[i])
    print(paste("Segmentation:", Segmentation))
    
    iter <- subset(malesubset, select = c(Segmentation, "Age")) 
    
    colnames(iter)[1] = "Vol"
    colnames(iter)[2] = "Age"
    
    # set max values
    max_iter <- max(iter$Vol, na.rm = TRUE)
    
    #orders
    order_iter <- order(iter$Age)
    
    fitQ05 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.05,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ10 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.1,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ25 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.25,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.5, 
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ75 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.75,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ90 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.9,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ95 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.95,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    
    ft05 <- predict(fitQ05, se = TRUE)
    ft10 <- predict(fitQ10, se = TRUE)
    ft25 <- predict(fitQ25, se = TRUE)
    ft50 <- predict(fitQ50, se = TRUE)
    ft75 <- predict(fitQ75, se = TRUE)
    ft90 <- predict(fitQ90, se = TRUE)
    ft95 <- predict(fitQ95, se = TRUE)
    assign(paste("ft05", Segmentation, sep = ""), ft05)
    assign(paste("ft10", Segmentation, sep = ""), ft10)
    assign(paste("ft25", Segmentation, sep = ""), ft25)
    assign(paste("ft50", Segmentation, sep = ""), ft50)
    assign(paste("ft75", Segmentation, sep = ""), ft75)
    assign(paste("ft90", Segmentation, sep = ""), ft90)
    assign(paste("ft95", Segmentation, sep = ""), ft95)
    
    
    # plot
    Plot_Iter <- ggplot() +
      geom_point(data = iter, aes(x = Age, y = Vol), color = "grey", size = 0.5) +
      
      # set centile lines (5, 10, 25, 50, 75, 90, 95)
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft05", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter]), color = "darkgrey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter]), color = "black", size = 1,  alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter]), color = "darkgrey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft95", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      
      # create colours between centile lines
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft05", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter]), fill = "lightblue", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter]), fill = "blue", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter]), fill = "darkblue", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter]), fill = "darkblue", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter]), fill = "blue", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft95", Segmentation, sep = ""))$fit[order_iter]), fill = "lightblue", alpha = 0.4) +
      
      labs(title = paste("Male ", Segmentation, sep = ""), x = "Age (years)", y = "GMV (mm^3)") +
      theme_ridges() +
      
      guides(fill = guide_legend(title = "Percentiles", keywidth = 2, keyheight = 2,
                                 override.aes = list(fill = c("lightblue", "blue", "darkblue", "darkblue", "blue", "lightblue")),
                                 labels = legend_labels)) +
    ylim(0.4, 1.25*max_iter) 
    
    print(Plot_Iter)
    
    
 # Save to output path folder    
 filename <- paste(output_path, "/", "Male_", Segmentation, ".png", sep = "")
    print(filename)
    png(filename= filename)
    plot(Plot_Iter)
    dev.off()
    
  }
     
  
  # =============================================================================
  # Plot FEMALE ONLY growth charts
  

  for (i in seq_along(seglist)) {
    
    Segmentation <- as.character(seglist[i])
    print(paste("Segmentation:", Segmentation))
    
    iter <- subset(femalesubset, select = c(Segmentation, "Age")) 
    
    colnames(iter)[1] = "Vol"
    colnames(iter)[2] = "Age"
    
    # set max values
    max_iter <- max(iter$Vol, na.rm = TRUE)
    
    #orders
    order_iter <- order(iter$Age)
    
    fitQ05 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.05,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ10 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.1,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ25 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.25,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ50 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.5, 
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ75 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.75,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ90 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.9,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    fitQ95 <- qgamV(Vol ~ s(Age), data = iter, qu = 0.95,
                    aQgam = list(argGam = list(select = TRUE, gamma = 1.8)))
    
    ft05 <- predict(fitQ05, se = TRUE)
    ft10 <- predict(fitQ10, se = TRUE)
    ft25 <- predict(fitQ25, se = TRUE)
    ft50 <- predict(fitQ50, se = TRUE)
    ft75 <- predict(fitQ75, se = TRUE)
    ft90 <- predict(fitQ90, se = TRUE)
    ft95 <- predict(fitQ95, se = TRUE)
    assign(paste("ft05", Segmentation, sep = ""), ft05)
    assign(paste("ft10", Segmentation, sep = ""), ft10)
    assign(paste("ft25", Segmentation, sep = ""), ft25)
    assign(paste("ft50", Segmentation, sep = ""), ft50)
    assign(paste("ft75", Segmentation, sep = ""), ft75)
    assign(paste("ft90", Segmentation, sep = ""), ft90)
    assign(paste("ft95", Segmentation, sep = ""), ft95)
    
    
    # plot
    Plot_Iter <- ggplot() +
      geom_point(data = iter, aes(x = Age, y = Vol), color = "grey", size = 0.5) +
      
      # set centile lines (5, 10, 25, 50, 75, 90, 95)
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft05", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter]), color = "darkgrey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter]), color = "black", size = 1,  alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter]), color = "darkgrey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      geom_line(data = iter, aes(x = Age[order_iter], y = get(paste("ft95", Segmentation, sep = ""))$fit[order_iter]), color = "grey", alpha = 1) +
      

      # create colours between centile lines
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft05", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter]), fill = "pink", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft10", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter]), fill = "red", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft25", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter]), fill = "darkred", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft50", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter]), fill = "darkred", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft75", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter]), fill = "red", alpha = 0.4) +
      geom_ribbon(data = iter, aes(x = Age[order_iter],
                                   ymin = get(paste("ft90", Segmentation, sep = ""))$fit[order_iter],
                                   ymax = get(paste("ft95", Segmentation, sep = ""))$fit[order_iter]), fill = "pink", alpha = 0.4) +
      
      labs(title = paste("Female ", Segmentation, sep = ""), x = "Age (years)", y = "GMV (mm^3)") +
      theme_ridges() +
      ylim(0.4, 1.25*max_iter)
     
    # print the curves
    print(Plot_Iter)
    
    
    # Save to output path folder  
    filename <- paste(output_path, "/", "Female_", Segmentation, ".png", sep = "")
    print(filename)
    png(filename= filename)
    plot(Plot_Iter)
    dev.off()
    
    
  }
  
    
} #end of function