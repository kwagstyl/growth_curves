# multinomial logistic regression (HS left and right): MGDA to identify if an
# unseen scan is related to the control cohort, left, or right-sided HS.

# libraries
library(ggplot2)
library(pROC)
library(plotROC)
library(ISLR)
library(glmnet)
library(foreign)
library(nnet)
library(reshape2)
library(tidyverse)
library(MASS)
library(klaR)
library(hrbrthemes)
library(viridis)
library(ggridges)
library(sp)
library(lattice)
library(latticeExtra)
library(unmarked)


# ------------------------------------------------------------------------------
# Load control data 
control_raw_z<- read.csv("//ad.ucl.ac.uk/homen/zchaeen/
            DesktopSettings/Desktop/270324/270324/Data_Final/control_z.csv")

# create segmentation list -----------------------------------------------------
seglist<- colnames(control_raw_z)

#start list of segmentation at first structure
index <- which(seglist == "left.cerebral.white.matter")
if (length(index) > 0) {
  seglist <- seglist[(index[1]):length(seglist)]
}

# print list of segmentation names
print(seglist)


# Produce subset data for logistic regression formatting -----------------------
#CONTROL
LR_con_z <- subset(control_raw_z, select = c(seglist))
LR_con_z$cohort[1:nrow(control_raw_z)] <- "Con"
head(LR_con_z)

# HS left
LR_HS_L   <- read.csv("//ad.ucl.ac.uk/homen/zchaeen/
            DesktopSettings/Desktop/270324/270324/Data_Final/HS_left_rel_z.csv")
LR_HS_L <- subset(LR_HS_L , select = c(seglist))
LR_HS_L $cohort[1:nrow(LR_HS_L)] <- "LHS"
head(LR_HS_L)

# HS right
LR_HS_R <-read.csv("//ad.ucl.ac.uk/homen/zchaeen/DesktopSettings/
                          Desktop/270324/270324/Data_Final/HS_right_rel_z.csv")
LR_HS_R <- subset(LR_HS_R , select = c(seglist))
LR_HS_R$cohort[1:nrow(LR_HS_R)] <- "RHS"
head(LR_HS_R)

# Bind all datasets
LR_dat <- rbind(LR_con_z, LR_HS_L, LR_HS_R)
summary(LR_dat)

# Assignment and check structure
data <- LR_dat
str(data)
head(data)

# ==============================================================================
# Basic checks and data demographics 

# Mean and standard deviation of left and right hippocampal z
with(data, table(left.hippocampus, right.hippocampus, cohort))
with(data, do.call(rbind, tapply(left.hippocampus + right.hippocampus,
                                 cohort, function(x) c(M = mean(x), SD = sd(x)))))

# optional re-level and assing cohort value
data$cohort2 <- relevel(data$cohort, ref = "0")
#data$cohort2 <- data$cohort
data_LR <- data

test <- multinom(data = data_LR, cohort2 ~ left.hippocampus + right.hippocampus)

# Simple 2 tailed z test - note if any predictive possibility
z <- summary(test)$coefficients/summary(test)$standard.errors
print(z)

# show p values for z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p)

# extract the coefficients from the model and exponentiate
exp(coef(test))

# ==============================================================================
# Reformatting of data into for predictive model

dHS <- data.frame() # set up empty data frame
  
# create vectors for range of z scores
lhsvols <- c(seq(-3, 3, length.out = 30))
rhsvols <- c(seq(-3, 3, length.out = 30))

# intialise lists
iter_list <- c()
bound_data <- c()

# create long data frame iterating 30x30 combinations
for(iter in seq_along(1:30)) {
  
  newdat <- list()
  # create iteration for left and right data
  newdat$left.hippocampus[1:30] <- as.numeric(lhsvols[iter])
  newdat$right.hippocampus <- rhsvols

  forbind <- cbind(newdat$left.hippocampus, newdat$right.hippocampus)
  bound_data <- rbind(bound_data, forbind) # bind all rows and columns
}

print(bound_data) # check data

# make data frame of z score combinations
dHS <- data.frame(cohort = rep(c("Con","LHS", "RHS"), each = 30), 
                  left.hippocampus = rep(c(seq(-3, 3, length.out = 30)), 3),
                 right.hippocampus = rep(c(seq(-3, 3, length.out = 30)), 3)
                  )


# set up base dataset for later testing
bound_data <- as.data.frame(bound_data)
colnames(bound_data) <- c("left.hippocampus", "right.hippocampus")
print(bound_data)

# create datasets per group for prediction (split, identify and recombine)
dHS0 <- bound_data
      dHS0$cohort <- "control"
dHS1 <- bound_data
      dHS1$cohort <- "left.HS"
dHS2 <- bound_data
      dHS2$cohort <- "right.HS"

# bind all for prediction
dHS <- rbind(dHS0, dHS1, dHS2)
str(dHS) # check data

# ------------------------------------------------------------------------------
# Calculate probabilities

# store the predicted probabilities for each z score combination and write
pp.HS <- cbind(dHS, predict(test, newdata = dHS, type = "probs", se = TRUE))
head(pp.HS) # check

# calculate the mean probabilities within each level of z score
by(pp.HS[, 4:6], pp.HS$cohort, colMeans)

# melt data set to long for ggplot2
lpp <- melt(pp.HS, id.vars = c("cohort", "left.hippocampus", "right.hippocampus"), 
            value.name = "probability")

# plot predicted probabilities across write values for each level of z score
# facetted by program type
ggplot(lpp, aes(x = right.hippocampus, y = probability, colour = cohort)) + 
      geom_line() + facet_grid(variable ~., scales = "free")


# ------------------------------------------------------------------------------
# set up probability matrix

str(pp.HS) # probailities as a long vector

matrixHS <- matrix(0, 30, 30) # intialised zeros
head(matrixHS) # check 

# iterate along matrix axis
for(iter in seq_along(0:29)){
  
  iterno <- as.numeric(iter) # define axis iteration
  
  HSvec <- pp.HS$LHS[((iterno*30)+1) :((iterno*30)+30)] # add batch of 30 data
  HSvec # check 
  length(HSvec) # ensure length is 30
  matrixHS[, iterno] <- HSvec # add to matrix of HS probability
}

print(matrixHS) # check matrix

# As data frame and give row and column names
dfHS <- as.data.frame(matrixHS) 
colnames(dfHS) <- c(seq(-3, 3, length.out = 30)) # cols
rownames(dfHS) <- c(seq(-3, 3, length.out = 30)) # rows

# ==============================================================================
# print heatmaps

# create colour scale for predictive map
coul <- viridis::mako(100, direction = -1, begin = 0.15, end = 1)

# Create a 3x1 panel with shared color scale
p1 <- levelplot(pp.HS$Con ~ left.hippocampus * right.hippocampus, pp.HS, cex = 0,
                panel = panel.levelplot.points, main = "Healthy Control Probability",
                xlab = "Left Hippocampus", col.regions = coul, ylab = "Right Hippocampus", colorkey =  FALSE
                , region = TRUE
                ) + 
  layer_(panel.2dsmoother(..., n = 700))

p2 <- levelplot(pp.HS$LHS ~ left.hippocampus * right.hippocampus, pp.HS, cex = 0,
                panel = panel.levelplot.points, main = "Right Hippocampal Sclerosis Probability",
                xlab = "Left Hippocampus",col.regions = coul, ylab = "Right Hippocampus",  
                colorkey = list(main = "z score")) + 
  layer_(panel.2dsmoother(..., n = 800))

p3 <- levelplot(pp.HS$RHS ~ left.hippocampus * right.hippocampus, pp.HS, cex = 0,
                panel = panel.levelplot.points, main = "Left Hippocampal Sclerosis Probability",
                xlab = "Left Hippocampus", col.regions = coul, ylab = "Right Hippocampus", colorkey =  FALSE) + 
  layer_(panel.2dsmoother(..., n = 800)) 

print(p3, split = c(1,1, 3, 1), more = TRUE) # L
print(p1, split = c(2,1, 3, 1), more = TRUE) # Control
print(p2, split = c(3,1, 3, 1)) # R, with colourbar


# ==============================================================================
# add datapoints on top of probability maps


# control ----------------------------------------------------------------------
con_points <- subset(control_raw_z, select = c("left.hippocampus", 
                                              "right.hippocampus")) # subset data
x <- con_points$left.hippocampus
y <- con_points$right.hippocampus
name <- "control"
coord_con <- data.frame(x, y, name)
coordinates(coord_con) <- ~ x + y # make coordinates map for data

levelplot(pp.HS$Con ~ left.hippocampus * right.hippocampus, pp.HS, cex = 0,
          panel = panel.levelplot.points, main = "Healthy Control Probability",
          xlab = "Left Hippocampus", ylab = "Right Hippocampus", colorkey =  FALSE
          , region = TRUE, col.regions = coul
) + 
  layer_(panel.2dsmoother(..., n = 700)) + 
  layer(sp.points(coord_con, pch = 19)) # add points

# Left HS --------------------------------------------------------------------------
lhs_points <- subset(LR_HS_L, select = c("left.hippocampus", "right.hippocampus"))
x <- lhs_points$left.hippocampus
y <- lhs_points$right.hippocampus
name <- "LHS"
coord_lhs <- data.frame(x, y, name)
coordinates(coord_lhs) <- ~ x + y

levelplot(pp.HS$LHS ~ left.hippocampus * right.hippocampus, pp.HS, cex = 0,
          panel = panel.levelplot.points, main = "Right Hippocampal Sclerosis Probability",
          xlab = "Left Hippocampus", ylab = "Right Hippocampus",  col.regions = coul,
          colorkey = list(main = "z score")) + 
  layer_(panel.2dsmoother(..., n = 800)) + 
  layer(sp.points(coord_lhs, pch = 19))

# Right HS --------------------------------------------------------------------------
rhs_points <- subset(LR_HS_R, select = c("left.hippocampus", "right.hippocampus"))
print(rhs_points)
x <- rhs_points$left.hippocampus
y <- rhs_points$right.hippocampus
name <- "RHS"
coord_rhs <- data.frame(x, y, name)
coordinates(coord_rhs) <- ~ x + y

levelplot(pp.HS$RHS ~ left.hippocampus * right.hippocampus, pp.HS, cex = 0,
  panel = panel.levelplot.points, main = "Left Hippocampal Sclerosis Probability",
  xlab = "Left Hippocampus", ylab = "Right Hippocampus", colorkey =  FALSE, col.regions = coul) + 
  layer_(panel.2dsmoother(..., n = 800)) +
  layer(sp.points(coord_rhs, pch = 19))






